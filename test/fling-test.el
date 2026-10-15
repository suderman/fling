;;; fling-test.el --- Kinetic touch-screen scrolling checks -*- lexical-binding: t; -*-

;;; Code:

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
               (lambda (_window velocity) (setq flung velocity))))
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

(ert-deftest fling-mode-adds-and-removes-its-advice ()
  (unwind-protect
      (progn
        (fling-mode 1)
        (should (advice-member-p #'fling--record 'touch-screen-handle-scroll))
        (should (advice-member-p #'fling--start 'touch-screen-handle-touch))
        (fling-mode -1)
        (should-not (advice-member-p #'fling--record 'touch-screen-handle-scroll))
        (should-not (advice-member-p #'fling--start 'touch-screen-handle-touch)))
    (fling-mode -1)))

(ert-deftest fling-touch-screen-internals-are-present ()
  ;; The mode depends on these touch-screen.el internals.  Argument names
  ;; survive advice, unlike `func-arity'.
  (should (equal (help-function-arglist 'touch-screen-handle-scroll t)
                 '(dx dy)))
  (should (eq (car (help-function-arglist 'touch-screen-handle-touch t))
              'event))
  (should (boundp 'touch-screen-current-tool))
  (dolist (function '(pixel-scroll-precision-scroll-down-page
                      pixel-scroll-precision-scroll-up-page))
    (should (fboundp function))))

(provide 'fling-test)
;;; fling-test.el ends here
