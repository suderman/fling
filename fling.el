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
;; fling.  The speed decays exponentially, and each step moves by speed times
;; the real time since the last frame, so a slow frame never slows the motion.
;; Any input stops the fling, as does reaching either end of the buffer.
;; A touch that stops a fling only scrolls: holding it, tapping, or
;; dragging from the region never selects text or moves point.
;; Garbage collection waits until the fling ends.
;;
;; The mode advises `touch-screen-handle-scroll' and
;; `touch-screen-handle-touch'.  These are internal to touch-screen.el, so
;; check compatibility when upgrading Emacs.

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

(defconst fling--catch-seconds 0.2
  "How soon after input stops a fling a touch counts as catching it.")

(defvar fling--caught-at nil
  "When input last stopped a fling, as a float time.")

(defvar fling--samples nil
  "Recent vertical touch-scroll movements as (TIME . DY), newest first.")

(defun fling--recent-samples (now)
  "Return the touch samples recorded within the sample window before NOW."
  (seq-take-while (lambda (sample)
                    (< (- now (car sample)) fling--sample-seconds))
                  fling--samples))

(defun fling--record (_dx dy &optional now)
  "Record vertical touch movement DY at NOW, defaulting to the current time."
  (let ((now (or now (float-time))))
    (push (cons now dy) fling--samples)
    (setq fling--samples (fling--recent-samples now))))

(defun fling--velocity (&optional now)
  "Return the vertical swipe speed at NOW in pixels per second.
A positive speed scrolls toward the end of the buffer."
  (let* ((now (or now (float-time)))
         (samples (fling--recent-samples now)))
    (if (cdr samples)
        ;; The oldest sample's movement happened before its own timestamp.
        (/ (apply #'+ (mapcar #'cdr (butlast samples)))
           (- now (car (car (last samples)))))
      0)))

(defun fling-scroll (window velocity)
  "Keep scrolling WINDOW at VELOCITY pixels per second, slowing with friction.
A positive VELOCITY scrolls toward the end of the buffer.  Any input stops
the fling."
  (let ((last (float-time))
        (carry 0.0)
        ;; A collection can take a tenth of a second on a phone, long enough
        ;; to make the fling jump.  Defer it until the fling stops.
        (gc-cons-threshold most-positive-fixnum))
    (with-selected-window window
      (condition-case nil
          (while (and (> (abs velocity) fling--stop-velocity)
                      (sit-for 0.001))
            (let* ((now (float-time))
                   (dt (- now last))
                   (distance (+ carry (* velocity dt)))
                   (pixels (truncate distance)))
              (setq last now
                    carry (- distance pixels)
                    velocity (* velocity (exp (- (/ dt fling-time-constant)))))
              (cond ((> pixels 0)
                     (pixel-scroll-precision-scroll-down-page pixels))
                    ((< pixels 0)
                     (pixel-scroll-precision-scroll-up-page (- pixels))))))
        ((beginning-of-buffer end-of-buffer) nil)))
    (when (input-pending-p)
      (setq fling--caught-at (float-time)))))

(defun fling--start (event &rest _)
  "Fling after touch-scroll EVENT ends at speed."
  (when (and (eq (car event) 'touchscreen-end)
             (eq (caadr event) (car touch-screen-current-tool))
             (eq (nth 3 touch-screen-current-tool) 'scroll)
             (not (caddr event)))
    (let ((velocity (fling--velocity)))
      (setq fling--samples nil)
      (when (> (abs velocity) fling-min-velocity)
        (fling-scroll (cadr touch-screen-current-tool) velocity)))))

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

;;;###autoload
(define-minor-mode fling-mode
  "Continue touch-screen scrolling after a swipe is released."
  :global t
  :group 'fling
  (if fling-mode
      (progn
        (advice-add 'touch-screen-handle-scroll :before #'fling--record)
        (advice-add 'touch-screen-handle-touch :before #'fling--start)
        (advice-add 'touch-screen-handle-touch :around #'fling--catch))
    (advice-remove 'touch-screen-handle-scroll #'fling--record)
    (advice-remove 'touch-screen-handle-touch #'fling--start)
    (advice-remove 'touch-screen-handle-touch #'fling--catch)
    (setq fling--samples nil
          fling--caught-at nil)))

(provide 'fling)
;;; fling.el ends here
