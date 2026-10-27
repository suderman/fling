;;; fling-test.el --- Kinetic touch-screen scrolling checks -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'fling)

(defun fling-test--swipe (now count step-seconds dy)
  "Record COUNT samples of DY pixels, STEP-SECONDS apart, ending at NOW."
  (setq fling--samples nil)
  (dotimes (i count)
    (fling--record 0 dy (- now (* step-seconds (- count 1 i))))))

(defmacro fling-test--with-clock (frames &rest body)
  "Run BODY with a clock advancing 10 ms per frame for at most FRAMES frames.
Scrolls are summed into `down' and `up'."
  (declare (indent 1))
  `(let ((clock 1000.0) (frame 0) (down 0) (up 0))
     (cl-letf (((symbol-function 'float-time) (lambda (&rest _) clock))
               ((symbol-function 'sit-for)
                (lambda (&rest _)
                  (setq clock (+ clock 0.01) frame (1+ frame))
                  (< frame ,frames)))
               ((symbol-function 'pixel-scroll-precision-scroll-down-page)
                (lambda (pixels) (setq down (+ down pixels))))
               ((symbol-function 'pixel-scroll-precision-scroll-up-page)
                (lambda (pixels) (setq up (+ up pixels)))))
       ,@body)))

(ert-deftest fling-velocity-uses-the-movement-before-release ()
  ;; 20 samples of 30 px, 10 ms apart: 3000 px/s.
  (fling-test--swipe 100.0 20 0.01 30)
  (should (< (abs (- (fling--velocity 100.0) 3000)) 1))
  (fling-test--swipe 100.0 20 0.01 -30)
  (should (< (abs (+ (fling--velocity 100.0) 3000)) 1)))

(ert-deftest fling-velocity-ignores-the-slow-start-of-a-swipe ()
  (setq fling--samples nil)
  (dotimes (i 50)
    (fling--record 0 2 (+ 99.0 (* 0.01 i))))
  (dotimes (i 10)
    (fling--record 0 40 (+ 99.5 (* 0.01 i))))
  (should (> (fling--velocity 99.6) 3000)))

(ert-deftest fling-velocity-is-zero-after-a-pause-or-a-single-sample ()
  (fling-test--swipe 100.0 20 0.01 30)
  (should (zerop (fling--velocity 100.2)))
  (fling-test--swipe 100.0 1 0.01 30)
  (should (zerop (fling--velocity 100.0))))

(ert-deftest fling-velocity-uses-when-touches-happened ()
  ;; Touches 10 ms apart that Emacs reads all at once after a stall.
  (let ((fling--samples nil)
        (fling--event-time nil)
        (posn (list (selected-window) 1 '(0 . 0) 0
                    nil 1 '(0 . 0) nil '(0 . 0) '(1 . 1))))
    (cl-letf (((symbol-function 'float-time) (lambda (&rest _) 500.0)))
      (dotimes (i 20)
        (setcar (nthcdr 3 posn) (+ 100000 (* 10 i)))
        (fling--note-time `(touchscreen-update ((7 . ,posn))))
        (fling--record 0 30))
      (setcar (nthcdr 3 posn) 100190)
      (fling--note-time `(touchscreen-end (7 . ,posn)))
      (should (< (abs (- (fling--velocity fling--event-time) 3000)) 1)))))

(ert-deftest fling-scroll-glides-speed-times-the-time-constant ()
  (with-temp-buffer
    (let ((fling-time-constant 0.35))
      (fling-test--with-clock 1000
        (fling-scroll (selected-window) 3000)
        (should (zerop up))
        ;; Exponential decay travels velocity * time constant, less the tail
        ;; below the stop speed.
        (should (< (abs (- down (* 3000 0.35))) 20)))
      (fling-test--with-clock 1000
        (fling-scroll (selected-window) -3000)
        (should (zerop down))
        (should (< (abs (- up (* 3000 0.35))) 20))))))

(ert-deftest fling-scroll-follows-real-time-not-frame-count ()
  ;; Frames ten times longer must not shorten the glide.
  (with-temp-buffer
    (let ((clock 1000.0) (down 0))
      (cl-letf (((symbol-function 'float-time) (lambda (&rest _) clock))
                ((symbol-function 'sit-for)
                 (lambda (&rest _) (setq clock (+ clock 0.1)) t))
                ((symbol-function 'pixel-scroll-precision-scroll-down-page)
                 (lambda (pixels) (setq down (+ down pixels)))))
        (fling-scroll (selected-window) 3000))
      (should (> down 900)))))

(ert-deftest fling-scroll-stops-on-input-and-at-buffer-ends ()
  (with-temp-buffer
    (fling-test--with-clock 3
      (fling-scroll (selected-window) 3000)
      (should (< down 100)))
    (fling-test--with-clock 1000
      (cl-letf (((symbol-function 'pixel-scroll-precision-scroll-down-page)
                 (lambda (_) (signal 'end-of-buffer nil))))
        (fling-scroll (selected-window) 3000))
      (should (< frame 3)))))

(ert-deftest fling-start-requires-a-completed-scroll-at-speed ()
  (let ((flung nil)
        (touch-screen-current-tool (list 7 (selected-window) nil 'scroll)))
    (cl-letf (((symbol-function 'fling-scroll)
               (lambda (_window velocity &optional _hvelocity)
                 (setq flung velocity))))
      (fling-test--swipe (float-time) 20 0.01 30)
      (fling--start '(touchscreen-end (7 . nil)))
      (should (and flung (> flung 2000)))
      (setq flung nil)
      ;; Too slow.
      (fling-test--swipe (float-time) 20 0.01 1)
      (fling--start '(touchscreen-end (7 . nil)))
      (should-not flung)
      ;; Canceled, or another tool.
      (fling-test--swipe (float-time) 20 0.01 30)
      (fling--start '(touchscreen-end (7 . nil) t))
      (fling-test--swipe (float-time) 20 0.01 30)
      (fling--start '(touchscreen-end (8 . nil)))
      (should-not flung)
      ;; Not a scroll.
      (setcar (nthcdr 3 touch-screen-current-tool) 'drag)
      (fling-test--swipe (float-time) 20 0.01 30)
      (fling--start '(touchscreen-end (7 . nil)))
      (should-not flung))))

(ert-deftest fling-scroll-glides-sideways-and-diagonally ()
  (with-temp-buffer
    (insert (make-string 1000 ?x) "\n")
    (goto-char (point-min))
    (set-window-buffer nil (current-buffer))
    (set-window-hscroll nil 0)
    (cl-letf (((symbol-function 'frame-char-width) (lambda (&rest _) 10)))
      (fling-test--with-clock 1000
        (fling-scroll (selected-window) 1000 3000)
        ;; About 3000 * 0.35 px sideways.
        (should (< (abs (- (fling--hscroll-offset) 1050)) 20))
        (should (< (abs (- down 350)) 20)))
      ;; Scrolling back stops at the start of lines.
      (fling-test--with-clock 1000
        (fling-scroll (selected-window) 0 -6000)
        (should (zerop (fling--hscroll-offset)))
        (should (< frame 50))))))

(ert-deftest fling-hscroll-moves-by-pixels-with-a-line-prefix ()
  (with-temp-buffer
    (insert (make-string 1000 ?x) "\n")
    (set-window-buffer nil (current-buffer))
    (set-window-hscroll nil 0)
    (cl-letf (((symbol-function 'frame-char-width) (lambda (&rest _) 10)))
      (let ((prefix (lambda ()
                      (seq-some (lambda (o) (overlay-get o 'line-prefix))
                                (overlays-in (point-min) (point-max))))))
        (fling--hscroll-pixels 23)
        ;; Three columns, pushed back by 7 pixels.
        (should (= (window-hscroll) 3))
        (should (equal (funcall prefix) '(space :width (7))))
        (should (= (fling--hscroll-offset) 23))
        (fling--hscroll-pixels 7)
        (should (= (window-hscroll) 3))
        (should-not (funcall prefix))
        ;; Scrolling elsewhere discards a stale prefix.
        (fling--hscroll-pixels 5)
        (set-window-hscroll nil 1)
        (should (= (fling--hscroll-offset) 10))
        (fling--drop-stale-prefix (selected-window))
        (should-not (funcall prefix))
        (fling--hscroll-pixels 5)
        (should-not (fling--hscroll-pixels -50))
        (should (zerop (fling--hscroll-offset)))
        (should-not (funcall prefix))))))

(ert-deftest fling-mode-off-drops-a-left-over-prefix ()
  (with-temp-buffer
    (insert (make-string 1000 ?x) "\n")
    (set-window-buffer nil (current-buffer))
    (set-window-hscroll nil 0)
    (cl-letf (((symbol-function 'frame-char-width) (lambda (&rest _) 10)))
      (fling-mode 1)
      (fling--hscroll-pixels 23)
      (should (overlays-in (point-min) (point-max)))
      (fling-mode -1)
      (should-not (overlays-in (point-min) (point-max)))
      (should-not (window-parameter nil 'fling--hscroll))
      (should (= (window-hscroll) 3)))))

(ert-deftest fling-hscroll-shifts-existing-line-prefixes ()
  (with-temp-buffer
    (insert "plain\n"
            (propertize "centered\n" 'line-prefix
                        (propertize " " 'display '(space :align-to (- center 4))))
            (propertize "indented\n" 'line-prefix "  "))
    (set-window-buffer nil (current-buffer))
    (set-window-hscroll nil 0)
    (cl-letf (((symbol-function 'frame-char-width) (lambda (&rest _) 10))
              ((symbol-function 'fling--hscroll-limit) (lambda () 100)))
      (fling--hscroll-pixels 23)
      (let ((prefix-at (lambda (pos)
                         (overlay-get
                          (car (sort (seq-filter (lambda (o) (overlay-get o 'line-prefix))
                                                 (overlays-at pos))
                                     (lambda (a b) (> (or (overlay-get a 'priority) 0)
                                                      (or (overlay-get b 'priority) 0)))))
                          'line-prefix))))
        (should (equal (funcall prefix-at 1) '(space :width (7))))
        (should (equal (get-text-property 0 'display (funcall prefix-at 7))
                       '(space :align-to (+ (- center 4) (7)))))
        (should (equal (funcall prefix-at 16)
                       (concat (propertize " " 'display '(space :width (7)))
                               "  ")))
        ;; The buffer's own prefixes are untouched.
        (should (equal (get-text-property 7 'line-prefix)
                       (propertize " " 'display '(space :align-to (- center 4)))))
        (fling--hscroll-pixels 7)
        (should-not (overlays-in (point-min) (point-max)))))))

(ert-deftest fling-drag-scrolls-sideways-by-pixels-only-where-enabled ()
  (with-temp-buffer
    (insert (make-string 1000 ?x) "\n")
    (set-window-buffer nil (current-buffer))
    (set-window-hscroll nil 0)
    (cl-letf (((symbol-function 'frame-char-width) (lambda (&rest _) 10)))
      (let ((seen 'unset)
            (touch-screen-current-tool
             (list 7 (selected-window) nil 'scroll nil 0 0 0 0)))
        (let ((scroll (lambda (_dx _dy) (setq seen touch-screen-enable-hscroll))))
          (let ((touch-screen-enable-hscroll nil))
            (fling--drag scroll 15 0))
          (should (zerop (fling--hscroll-offset)))
          (let ((touch-screen-enable-hscroll t))
            (fling--drag scroll 15 0))
          ;; touch-screen.el counts the movement but does not scroll.
          (should-not seen)
          (should (= (fling--hscroll-offset) 15))
          ;; A swipe that scrolled vertically first stays vertical.
          (setcar (nthcdr 7 touch-screen-current-tool) 10)
          (let ((touch-screen-enable-hscroll t))
            (fling--drag scroll 15 0))
          (should (= (fling--hscroll-offset) 15)))))))

(ert-deftest fling-start-flings-sideways-only-after-a-sideways-scroll ()
  (with-temp-buffer
    (set-window-buffer nil (current-buffer))
    (set-window-hscroll nil 0)
    (let ((flung nil)
          (touch-screen-current-tool (list 7 (selected-window) nil 'scroll)))
      (cl-letf (((symbol-function 'fling-scroll)
                 (lambda (_window velocity &optional hvelocity)
                   (setq flung (cons hvelocity velocity)))))
        (fling--start '(touchscreen-begin (7 . nil)))
        (setq fling--samples nil)
        (dotimes (i 20)
          (fling--record 30 0 (- (float-time) (* 0.01 (- 19 i)))))
        ;; touch-screen.el ignored the sideways movement.
        (fling--start '(touchscreen-end (7 . nil)))
        (should-not flung)
        (fling--start '(touchscreen-begin (7 . nil)))
        (dotimes (i 20)
          (fling--record 30 0 (- (float-time) (* 0.01 (- 19 i)))))
        (set-window-hscroll nil 5)
        (fling--start '(touchscreen-end (7 . nil)))
        (should (> (car flung) 2000))))))

(ert-deftest fling-vscroll-moves-by-lines-in-a-window-scrolled-sideways ()
  (with-temp-buffer
    (dotimes (i 50) (insert (format "%d %s\n" i (make-string 200 ?x))))
    (set-window-buffer nil (current-buffer))
    (set-window-start nil (point-min))
    (goto-char (point-min))
    (set-window-hscroll nil 5)
    (let ((line (line-pixel-height))
          (unscrolled nil))
      (let ((scroll (lambda (_) (setq unscrolled t))))
        (fling--vscroll-down scroll (* 3 line))
        (should-not unscrolled)
        (should (= (line-number-at-pos (window-start)) 4))
        (should (>= (point) (window-start)))
        (fling--vscroll-up scroll line)
        (should (= (line-number-at-pos (window-start)) 3))
        (should-error (fling--vscroll-up scroll (* 10 line))
                      :type 'beginning-of-buffer)
        (should (= (window-start) (point-min)))
        (set-window-hscroll nil 0)
        (fling--vscroll-down scroll line)
        (should unscrolled)))))

(ert-deftest fling-scroll-splits-steps-taller-than-the-window ()
  (let ((calls nil))
    (cl-letf (((symbol-function 'window-text-height) (lambda (&rest _) 1000))
              ((symbol-function 'pixel-scroll-precision-scroll-down-page)
               (lambda (pixels) (push pixels calls)))
              ((symbol-function 'pixel-scroll-precision-scroll-up-page)
               (lambda (pixels) (push (- pixels) calls))))
      (fling--scroll-pixels 2500)
      (should (equal (nreverse calls) '(999 999 502)))
      (setq calls nil)
      (fling--scroll-pixels -1200)
      (should (equal (nreverse calls) '(-999 -201))))))

(ert-deftest fling-hold-waits-for-queued-input ()
  (let ((touch-screen-current-timer nil)
        (held nil))
    (unwind-protect
        (let ((timeout (lambda (arg) (setq held arg))))
          (cl-letf (((symbol-function 'input-pending-p) #'always))
            (fling--hold timeout t))
          (should-not held)
          (should (timerp touch-screen-current-timer))
          (cl-letf (((symbol-function 'input-pending-p) #'ignore))
            (fling--hold timeout t))
          (should held))
      (when touch-screen-current-timer
        (cancel-timer touch-screen-current-timer)))))

(ert-deftest fling-scroll-notes-when-input-stops-it ()
  (with-temp-buffer
    (setq fling--caught-at nil)
    (fling-test--with-clock 1000
      (fling-scroll (selected-window) 3000))
    (should-not fling--caught-at)
    (cl-letf (((symbol-function 'input-pending-p) #'always))
      (fling-test--with-clock 3
        (fling-scroll (selected-window) 3000)))
    (should fling--caught-at)))

(ert-deftest fling-catch-turns-a-touch-that-stops-a-fling-into-a-scroll ()
  (let* ((touch-screen-current-tool nil)
         (touch-screen-current-timer nil)
         (extend nil)
         (begin (lambda (_event)
                  (setq extend touch-screen-extend-selection
                        touch-screen-current-tool
                        (list 7 (selected-window) '(0 . 0) nil)
                        touch-screen-current-timer
                        (run-at-time 60 nil #'ignore)))))
    (unwind-protect
        (let ((touch-screen-extend-selection t))
          ;; An ordinary touch keeps its long press and region drag.
          (setq fling--caught-at nil)
          (fling--catch begin '(touchscreen-begin (7 . nil)))
          (should extend)
          (should touch-screen-current-timer)
          (should-not (nth 3 touch-screen-current-tool))
          (cancel-timer touch-screen-current-timer)
          ;; A touch right after a fling stopped only scrolls.
          (setq fling--caught-at (float-time))
          (fling--catch begin '(touchscreen-begin (7 . nil)))
          (should-not extend)
          (should-not touch-screen-current-timer)
          (should (eq (nth 3 touch-screen-current-tool) 'scroll))
          (should-not fling--caught-at)
          ;; A later touch is ordinary again.
          (setq fling--caught-at (- (float-time) 1))
          (fling--catch begin '(touchscreen-begin (7 . nil)))
          (should touch-screen-current-timer))
      (when touch-screen-current-timer
        (cancel-timer touch-screen-current-timer)))))

(ert-deftest fling-defers-garbage-collection-until-idle ()
  (let ((gc-cons-threshold 1000)
        (fling--gc-saved nil)
        (fling--gc-timer nil)
        (collected 0))
    (cl-letf (((symbol-function 'garbage-collect)
               (lambda () (setq collected (1+ collected)))))
      (unwind-protect
          (progn
            ;; Only the start of a touch defers.
            (fling--defer-gc '(touchscreen-update ((7 . nil))))
            (should (= gc-cons-threshold 1000))
            (fling--defer-gc '(touchscreen-begin (7 . nil)))
            (should (= gc-cons-threshold fling--gc-threshold))
            (should (memq fling--gc-timer timer-idle-list))
            ;; A later touch keeps the first saved threshold and timer.
            (let ((timer fling--gc-timer))
              (fling--defer-gc)
              (should (eq fling--gc-timer timer)))
            (fling--collect)
            (should (= gc-cons-threshold 1000))
            (should (= collected 1))
            (should-not fling--gc-timer)
            ;; Disabling the mode restores the threshold without collecting.
            (fling--defer-gc)
            (fling-mode -1)
            (should (= gc-cons-threshold 1000))
            (should (= collected 1)))
        (fling--restore-gc)))))

(ert-deftest fling-mode-adds-and-removes-its-advice ()
  (unwind-protect
      (progn
        (fling-mode 1)
        (should (advice-member-p #'fling--record 'touch-screen-handle-scroll))
        (should (advice-member-p #'fling--drag 'touch-screen-handle-scroll))
        (should (advice-member-p #'fling--vscroll-down
                                 'pixel-scroll-precision-scroll-down-page))
        (should (advice-member-p #'fling--start 'touch-screen-handle-touch))
        (should (advice-member-p #'fling--note-time 'touch-screen-handle-touch))
        (should (advice-member-p #'fling--defer-gc 'touch-screen-handle-touch))
        (should (advice-member-p #'fling--catch 'touch-screen-handle-touch))
        (should (advice-member-p #'fling--hold 'touch-screen-handle-timeout))
        (fling-mode -1)
        (should-not (advice-member-p #'fling--record 'touch-screen-handle-scroll))
        (should-not (advice-member-p #'fling--drag 'touch-screen-handle-scroll))
        (should-not (advice-member-p #'fling--vscroll-down
                                     'pixel-scroll-precision-scroll-down-page))
        (should-not (advice-member-p #'fling--start 'touch-screen-handle-touch))
        (should-not (advice-member-p #'fling--note-time 'touch-screen-handle-touch))
        (should-not (advice-member-p #'fling--defer-gc 'touch-screen-handle-touch))
        (should-not (advice-member-p #'fling--catch 'touch-screen-handle-touch))
        (should-not (advice-member-p #'fling--hold 'touch-screen-handle-timeout)))
    (fling-mode -1)))

(ert-deftest fling-touch-screen-internals-are-present ()
  ;; The mode depends on these touch-screen.el internals.  Argument names
  ;; survive advice, unlike `func-arity'.
  (should (equal (help-function-arglist 'touch-screen-handle-scroll t)
                 '(dx dy)))
  (should (eq (car (help-function-arglist 'touch-screen-handle-touch t))
              'event))
  (should (equal (help-function-arglist 'touch-screen-handle-timeout t)
                 '(arg)))
  (dolist (variable '(touch-screen-current-tool
                      touch-screen-current-timer
                      touch-screen-extend-selection))
    (should (boundp variable)))
  (dolist (function '(pixel-scroll-precision-scroll-down-page
                      pixel-scroll-precision-scroll-up-page))
    (should (fboundp function))))

(provide 'fling-test)
;;; fling-test.el ends here
