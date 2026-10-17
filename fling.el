;;; fling.el --- Kinetic touch-screen scrolling -*- lexical-binding: t; -*-

;; Author: Jon Suderman
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience, mouse
;; URL: https://github.com/suderman/fling

;;; Commentary:
;; Emacs scrolls with a touch screen but stops dead when the finger lifts.
;; The kinetic scrolling in `pixel-scroll' was built for touchpads: it
;; averages the whole gesture, slows down linearly for a fixed time, and moves
;; a fixed distance per timer tick, so it drags whenever redisplay is slower
;; than the tick.
;;
;; `fling-mode' continues a released swipe instead.  The release speed comes
;; from the last tenth of a second only, so a pause before lifting does not
;; fling.  A swipe that also scrolled the window sideways flings sideways
;; too, so a diagonal swipe glides diagonally.  The speed decays
;; exponentially, and each step moves by speed times the real time since the
;; last frame, so a slow frame never slows the motion.
;;
;; Emacs scrolls a window sideways only by whole columns.  Where touch
;; scrolls sideways, swipes and flings move by pixels instead: the window
;; scrolls one column too far and a blank `line-prefix' less than a column
;; wide pushes the text back.  Lines with their own `line-prefix' text
;; property get a shifted copy of it, so centered text stays centered.  `pixel-scroll' cannot
;; scroll vertically in a window scrolled sideways, so there the mode
;; scrolls line by line itself.
;; Any input stops the fling, as does reaching either end of the buffer.
;; A touch that stops a fling only scrolls: holding it, tapping, or
;; dragging from the region never selects text or moves point.  A long press
;; also waits until Emacs has read any queued touch movement, so a stall
;; cannot turn a swipe into a selection.
;; Garbage collection waits until the fling ends.
;;
;; The mode advises `touch-screen-handle-scroll', `touch-screen-handle-touch',
;; and `touch-screen-handle-timeout', internal to touch-screen.el, and the
;; `pixel-scroll' page functions.  Check compatibility when upgrading Emacs.

;;; Code:

(require 'pixel-scroll)
(require 'seq)
(require 'touch-screen)

(defgroup fling nil
  "Kinetic touch-screen scrolling."
  :group 'mouse
  :prefix "fling-")

(defcustom fling-time-constant 0.35
  "Seconds for a fling to lose about two thirds of its speed.
Larger values glide further."
  :type 'number)

(defcustom fling-min-velocity 300
  "Release speed in pixels per second below which a swipe does not fling."
  :type 'number)

(defconst fling--sample-seconds 0.1
  "How much of the swipe before release sets the fling speed.")

(defconst fling--stop-velocity 30
  "Speed in pixels per second at which a fling stops.")

(defconst fling--hold-retry-seconds 0.05
  "How long a long press waits for queued input before checking again.")

(defconst fling--catch-seconds 0.2
  "How soon after input stops a fling a touch counts as catching it.")

(defvar fling--caught-at nil
  "When input last stopped a fling, as a float time.")

(defvar fling--samples nil
  "Recent touch-scroll movements as (TIME DX . DY), newest first.")

(defvar fling--hscroll-start nil
  "The window and its sideways offset in pixels when the current swipe began.")

(defun fling--recent-samples (now)
  "Return the touch samples recorded within the sample window before NOW."
  (seq-take-while (lambda (sample)
                    (< (- now (car sample)) fling--sample-seconds))
                  fling--samples))

(defun fling--record (dx dy &optional now)
  "Record touch movement DX and DY at NOW, defaulting to the current time."
  (let ((now (or now (float-time))))
    (unless fling--hscroll-start
      (setq fling--hscroll-start
            (cons (selected-window) (fling--hscroll-offset))))
    (push (cons now (cons dx dy)) fling--samples)
    (setq fling--samples (fling--recent-samples now))))

(defun fling--velocity (&optional now horizontal)
  "Return the swipe speed at NOW in pixels per second.
Return the vertical speed, positive toward the end of the buffer, or with
HORIZONTAL the sideways speed, positive toward the ends of lines."
  (let* ((now (or now (float-time)))
         (samples (fling--recent-samples now))
         (axis (if horizontal #'cadr #'cddr)))
    (if (cdr samples)
        ;; The oldest sample's movement happened before its own timestamp.
        (/ (apply #'+ (mapcar axis (butlast samples)))
           (- now (car (car (last samples)))))
      0)))

(defun fling--scroll-pixels (pixels)
  "Scroll the selected window by PIXELS, toward the end if positive.
`pixel-scroll' cannot move a window's text height in one call, which a
fast fling covers in one slow frame."
  (let ((step (max 1 (1- (window-text-height nil t)))))
    (while (/= pixels 0)
      (let ((move (max (- step) (min step pixels))))
        (if (> move 0)
            (pixel-scroll-precision-scroll-down-page move)
          (pixel-scroll-precision-scroll-up-page (- move)))
        (setq pixels (- pixels move))))))

(defun fling--hscroll-limit ()
  "Return the horizontal scroll that brings the widest visible line's end
to the right edge of the selected window."
  (let ((width 0)
        (end (window-end)))
    (save-excursion
      (goto-char (window-start))
      (while (< (point) end)
        (end-of-line)
        (setq width (max width (current-column)))
        (forward-line)))
    (max 0 (- width (window-max-chars-per-line) -1))))

(defun fling--vscroll-hscrolled (scroll pixels delta)
  "Call SCROLL with PIXELS, or scroll DELTA here in a window scrolled sideways.
SCROLL is `pixel-scroll-precision-scroll-down-page' or its upward twin,
which find the new window start at the left edge: mid-line, or a fling's
blank prefix, once the window scrolls sideways.  There, move the start by
whole lines and keep the remainder as vertical scroll.  A positive DELTA
scrolls toward the end of the buffer."
  (if (zerop (window-hscroll))
      (funcall scroll pixels)
    (let ((vscroll (+ (window-vscroll nil t) delta))
          (edge nil)
          start)
      (save-excursion
        (goto-char (window-start))
        (vertical-motion 0)
        (while (and (not edge) (< vscroll 0))
          (if (zerop (vertical-motion -1))
              (setq vscroll 0 edge 'beginning-of-buffer)
            (setq vscroll (+ vscroll (line-pixel-height)))))
        (while (and (not edge) (>= vscroll (line-pixel-height)))
          (let ((height (line-pixel-height)))
            (if (/= (vertical-motion 1) 1)
                (progn (vertical-motion 0)
                       (setq vscroll 0 edge 'end-of-buffer))
              (setq vscroll (- vscroll height)))))
        (setq start (point)))
      (let ((auto-window-vscroll nil))
        (set-window-start nil start (not (zerop vscroll)))
        (set-window-vscroll nil vscroll t t))
      ;; Keep point on screen so redisplay does not recenter.
      (cond ((< (point) start)
             (goto-char start)
             (unless (zerop vscroll) (vertical-motion 1)))
            ((not (pos-visible-in-window-p (point)))
             (goto-char start)
             (vertical-motion (max 0 (- (floor (window-text-height nil t)
                                               (default-line-height))
                                        2)))))
      (when edge (signal edge nil)))))

(defun fling--vscroll-down (scroll pixels)
  "Call SCROLL down PIXELS, by lines in a window scrolled sideways."
  (fling--vscroll-hscrolled scroll pixels pixels))

(defun fling--vscroll-up (scroll pixels)
  "Call SCROLL up PIXELS, by lines in a window scrolled sideways."
  (fling--vscroll-hscrolled scroll pixels (- pixels)))

(defun fling--hscroll-offset (&optional window)
  "Return how far WINDOW is scrolled sideways, in pixels.
The window's `fling--hscroll' parameter holds (HSCROLL PREFIX TICK .
OVERLAYS) while overlays shift its text PREFIX pixels back from column
HSCROLL, made when the buffer's character tick was TICK;
it counts only while the window is still at that column."
  (let* ((window (window-normalize-window window t))
         (hscroll (window-hscroll window))
         (state (window-parameter window 'fling--hscroll)))
    (- (* hscroll (frame-char-width (window-frame window)))
       (if (eql (car state) hscroll) (cadr state) 0))))

(defun fling--hscroll-pixels (pixels)
  "Scroll the selected window PIXELS sideways, toward line ends if positive.
Return nil once the window reaches the start of lines or the end of the
widest visible line."
  (let* ((width (frame-char-width))
         (offset (fling--hscroll-offset))
         (limit (max offset (* width (fling--hscroll-limit))))
         (target (max 0 (min limit (+ offset pixels))))
         (hscroll (ceiling target width))
         (prefix (- (* hscroll width) target))
         (state (window-parameter nil 'fling--hscroll))
         (tick (buffer-chars-modified-tick))
         (overlays (cdddr state)))
    (set-window-hscroll nil hscroll)
    (unless (and (not (zerop prefix))
                 (eql (nth 2 state) tick)
                 (eq (overlay-buffer (car overlays)) (current-buffer)))
      (mapc #'delete-overlay overlays)
      (setq overlays (unless (zerop prefix) (fling--prefix-overlays))))
    (dolist (overlay overlays)
      (overlay-put overlay 'line-prefix
                   (fling--shift-prefix (overlay-get overlay 'fling--prefix)
                                        prefix)))
    (set-window-parameter nil 'fling--hscroll
                          (and overlays
                               (cons hscroll (cons prefix (cons tick overlays)))))
    (= target (+ offset pixels))))

(defun fling--drop-stale-prefix (window)
  "Remove WINDOW's prefix overlays once something else scrolls it sideways.
Other commands move whole columns and know nothing of the prefix."
  (when-let* ((state (window-parameter window 'fling--hscroll))
              ((or (not (eql (car state) (window-hscroll window)))
                   (not (eq (overlay-buffer (nth 3 state))
                            (window-buffer window))))))
    (mapc #'delete-overlay (cdddr state))
    (set-window-parameter window 'fling--hscroll nil)))

(defun fling--prefix-overlays ()
  "Return overlays to shift the current buffer's lines in the selected window.
The first covers the buffer; each later one covers a run of a
`line-prefix' text property, keeping its value as `fling--prefix'."
  (let ((overlays nil)
        (pos (point-min)))
    (while (setq pos (text-property-not-all pos (point-max) 'line-prefix nil))
      (let ((next (next-single-property-change pos 'line-prefix nil
                                               (point-max)))
            (overlay nil))
        (setq overlay (make-overlay pos next))
        (overlay-put overlay 'fling--prefix (get-text-property pos 'line-prefix))
        (overlay-put overlay 'priority 1)
        (push overlay overlays)
        (setq pos next)))
    (push (make-overlay (point-min) (point-max) nil nil t) overlays)
    (dolist (overlay overlays overlays)
      (overlay-put overlay 'window (selected-window)))))

(defun fling--shift-prefix (prefix pixels)
  "Return line prefix PREFIX with its text pushed PIXELS right.
Space before an `:align-to' stretch does not move what follows it, so
move the stretch's target instead."
  (let* ((string (if (stringp prefix) prefix (propertize " " 'display prefix)))
         (spec (and prefix (get-text-property 0 'display string))))
    (cond
     ((null prefix) `(space :width (,pixels)))
     ((and (eq (car-safe spec) 'space) (plist-get (cdr spec) :align-to))
      (let ((shifted (copy-sequence string))
            (end (next-single-property-change 0 'display string
                                              (length string))))
        (put-text-property
         0 end 'display
         `(space ,@(plist-put (copy-sequence (cdr spec)) :align-to
                              `(+ ,(plist-get (cdr spec) :align-to)
                                  (,pixels))))
         shifted)
        shifted))
     (t (concat (propertize " " 'display `(space :width (,pixels)))
                string)))))

(defun fling--drag (scroll dx dy)
  "Call SCROLL with DX and DY, scrolling sideways by pixels where enabled.
SCROLL is `touch-screen-handle-scroll', which still counts the sideways
movement and decides whether a swipe that scrolled vertically may also
scroll sideways."
  (if (not touch-screen-enable-hscroll)
      (funcall scroll dx dy)
    (let ((touch-screen-enable-hscroll nil))
      (funcall scroll dx dy))
    (when (or (> (or (nth 8 touch-screen-current-tool) 0) 0)
              (< (or (nth 7 touch-screen-current-tool) 0) 1))
      (fling--hscroll-pixels dx))))

(defun fling-scroll (window velocity &optional hvelocity)
  "Keep scrolling WINDOW at VELOCITY pixels per second, slowing with friction.
A positive VELOCITY scrolls toward the end of the buffer.  HVELOCITY scrolls
sideways at the same time, positive toward the ends of lines.  Any input
stops the fling."
  (let ((last (float-time))
        (carry 0.0)
        (hvelocity (or hvelocity 0))
        (hcarry 0.0)
        ;; A collection can take a tenth of a second on a phone, long enough
        ;; to make the fling jump.  Defer it until the fling stops.
        (gc-cons-threshold most-positive-fixnum))
    (with-selected-window window
      (condition-case nil
          (while (and (> (sqrt (+ (* velocity velocity)
                                  (* hvelocity hvelocity)))
                         fling--stop-velocity)
                      (sit-for 0.001))
            (let* ((now (float-time))
                   (dt (- now last))
                   (decay (exp (- (/ dt fling-time-constant))))
                   (distance (+ carry (* velocity dt)))
                   (pixels (truncate distance))
                   (hdistance (+ hcarry (* hvelocity dt)))
                   (hpixels (truncate hdistance)))
              (setq last now
                    carry (- distance pixels)
                    hcarry (- hdistance hpixels)
                    velocity (* velocity decay)
                    hvelocity (* hvelocity decay))
              (when (and (/= hpixels 0)
                         (not (fling--hscroll-pixels hpixels)))
                (setq hvelocity 0))
              (fling--scroll-pixels pixels)))
        ;; A diagonal fling keeps going sideways at either end of the buffer.
        ((beginning-of-buffer end-of-buffer)
         (unless (zerop hvelocity)
           (fling-scroll window 0 (* hvelocity
                                     (exp (- (/ (- (float-time) last)
                                                fling-time-constant)))))))))
    (when (input-pending-p)
      (setq fling--caught-at (float-time)))))

(defun fling--start (event &rest _)
  "Fling after touch-scroll EVENT ends at speed.
Fling sideways only when the swipe scrolled the window sideways, since
touch-screen.el may have ignored the swipe's horizontal movement."
  (when (eq (car event) 'touchscreen-begin)
    (setq fling--hscroll-start nil))
  (when (and (eq (car event) 'touchscreen-end)
             (eq (caadr event) (car touch-screen-current-tool))
             (eq (nth 3 touch-screen-current-tool) 'scroll)
             (not (caddr event)))
    (let* ((window (cadr touch-screen-current-tool))
           (velocity (fling--velocity))
           (hvelocity (if (and (eq (car fling--hscroll-start) window)
                               (window-live-p window)
                               (/= (fling--hscroll-offset window)
                                   (cdr fling--hscroll-start)))
                          (fling--velocity nil t)
                        0)))
      (setq fling--samples nil
            fling--hscroll-start nil)
      (when (> (sqrt (+ (* velocity velocity) (* hvelocity hvelocity)))
               fling-min-velocity)
        (fling-scroll window velocity hvelocity)))))

(defun fling--catch (touch event &rest args)
  "Call TOUCH with EVENT and ARGS, making a touch that stops a fling a scroll.
Such a touch gets no long press, cannot drag the region, and does
nothing when it lifts without moving."
  (if (and (eq (car event) 'touchscreen-begin)
           fling--caught-at
           (< (- (float-time) fling--caught-at) fling--catch-seconds))
      (progn
        (setq fling--caught-at nil)
        (let ((touch-screen-extend-selection nil))
          (apply touch event args))
        (when (and touch-screen-current-tool
                   (null (nth 3 touch-screen-current-tool)))
          (setcar (nthcdr 3 touch-screen-current-tool) 'scroll)
          (when touch-screen-current-timer
            (cancel-timer touch-screen-current-timer)
            (setq touch-screen-current-timer nil))))
    (apply touch event args)))

(defun fling--hold (timeout arg)
  "Call TIMEOUT with ARG once no input is waiting to be read.
After a stall, the long-press timer can fire before Emacs reads touch
movement that is already queued, turning a swipe into a selection."
  (if (and arg (input-pending-p))
      (setq touch-screen-current-timer
            (run-at-time fling--hold-retry-seconds nil
                         #'touch-screen-handle-timeout t))
    (funcall timeout arg)))

;;;###autoload
(define-minor-mode fling-mode
  "Continue touch-screen scrolling after a swipe is released."
  :global t
  :group 'fling
  (if fling-mode
      (progn
        (advice-add 'touch-screen-handle-scroll :before #'fling--record)
        ;; Innermost, to see `touch-screen-enable-hscroll' as other advice
        ;; binds it.
        (advice-add 'touch-screen-handle-scroll :around #'fling--drag
                    '((depth . 100)))
        (add-hook 'pre-redisplay-functions #'fling--drop-stale-prefix)
        (advice-add 'pixel-scroll-precision-scroll-down-page :around
                    #'fling--vscroll-down)
        (advice-add 'pixel-scroll-precision-scroll-up-page :around
                    #'fling--vscroll-up)
        (advice-add 'touch-screen-handle-touch :before #'fling--start)
        (advice-add 'touch-screen-handle-touch :around #'fling--catch)
        (advice-add 'touch-screen-handle-timeout :around #'fling--hold))
    (advice-remove 'touch-screen-handle-scroll #'fling--record)
    (advice-remove 'touch-screen-handle-scroll #'fling--drag)
    (remove-hook 'pre-redisplay-functions #'fling--drop-stale-prefix)
    (advice-remove 'pixel-scroll-precision-scroll-down-page
                   #'fling--vscroll-down)
    (advice-remove 'pixel-scroll-precision-scroll-up-page
                   #'fling--vscroll-up)
    (advice-remove 'touch-screen-handle-touch #'fling--start)
    (advice-remove 'touch-screen-handle-touch #'fling--catch)
    (advice-remove 'touch-screen-handle-timeout #'fling--hold)
    (setq fling--samples nil
          fling--hscroll-start nil
          fling--caught-at nil)))

(provide 'fling)
;;; fling.el ends here
