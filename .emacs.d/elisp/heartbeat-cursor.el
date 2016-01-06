;;; heartbeat-cursor.el ---

;; makes the cursor blink

(eval-when-compile 'cl-lib)
(require 'color)

(defvar heartbeat-fps 16)
(defvar heartbeat-period 5)

(defun heartbeat-range (from to cnt)
  (let ((step (/ (- to from) (float cnt))))
    (cl-loop for i below cnt collect (+ from (* step i)))))

(defun heartbeat-cursor-colors ()
  (let ((cnt (* heartbeat-period heartbeat-fps)))
    (mapcar (lambda (r)
              (color-rgb-to-hex 1 r r))
            (nconc (heartbeat-range .2 1 (/ cnt 2))
                   (heartbeat-range 1 .2 (/ cnt 2))))))

(defvar heartbeat-cursor-timer nil)
(defvar heartbeat-cursor-old-color nil)

(define-minor-mode heartbeat-cursor-mode
  "Change cursor color with the heartbeat effect."
  nil "" nil
  :global t
  (when heartbeat-cursor-timer
    (cancel-timer heartbeat-cursor-timer)
    (setq heartbeat-cursor-timer nil)
    (set-face-background 'cursor heartbeat-cursor-old-color))
  (when heartbeat-cursor-mode
    (setq heartbeat-cursor-old-color (face-background 'cursor)
          heartbeat-cursor-timer
          (run-with-timer
           0 (/ 1 (float heartbeat-fps))
           (lexical-let ((colors (heartbeat-cursor-colors)) tail)
             (lambda ()
               (setq tail (or (cdr tail) colors))
               (set-face-background 'cursor (car tail))))))))

(provide 'heartbeat-cursor)
;;; heartbeat-cursor.el ends here
