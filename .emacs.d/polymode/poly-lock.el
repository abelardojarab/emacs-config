
;; `font-lock-mode' call graph:
;; -> font-lock-function <- we are replacing this with `poly-lock-mode'
;;   -> font-lock-default-function
;;     -> font-lock-mode-internal
;;        -> font-lock-turn-on-thing-lock
;;           -> font-lock-turn-on-thing-lock
;;             -> (setq font-lock-flush-function jit-lock-refontify)
;;             -> (setq font-lock-ensure-function jit-lock-fontify-now)
;;             -> (setq font-lock-fontify-buffer-function jit-lock-refontify)
;;             -> (jit-lock-register #'font-lock-fontify-region)
;;               -> (add-hook 'jit-lock-functions #'font-lock-fontify-region nil t)
;;               -> jit-lock-mode

(defvar-local poly-lock-mode nil)
(defvar poly-lock-allow-after-change t)
(defvar poly-lock-allow-fontification t)
(defvar poly-lock-fontification-in-progress nil)

(eval-when-compile
  (defmacro with-buffer-prepared-for-poly-lock (&rest body)
    "Execute BODY in current buffer, overriding several variables.
Preserves the `buffer-modified-p' state of the current buffer."
    (declare (debug t))
    `(let ((inhibit-point-motion-hooks t))
       (with-silent-modifications
         ,@body))))

(defun poly-lock-mode (arg)
  ;; value of `font-lock-function' in polymode buffers
  (unless polymode-mode
    (error "Trying to (de)activate `poly-lock-mode' in a non-polymode buffer (%s)" (current-buffer)))
  (setq poly-lock-mode arg)

  (if arg
      (progn
        (setq-local font-lock-support-mode 'poly-lock-mode)
        (setq-local font-lock-dont-widen t)

        ;; most of the following is setup in `font-lock-turn-on-thing-lock' in jit-lock case.
        
        ;; we don't allow any other functions
        (setq-local fontification-functions '(poly-lock-fontification-function))

        (remove-hook 'after-change-functions 'font-lock-after-change-function t)
        (remove-hook 'after-change-functions 'jit-lock-after-change t)
        (add-hook 'after-change-functions 'poly-lock-after-change nil t)
        
        (setq-local font-lock-flush-function 'poly-lock-refontify)
        (setq-local font-lock-ensure-function 'poly-lock-fontify-region)
        (setq-local font-lock-fontify-buffer-function 'poly-lock-refontify)

        ;; there are some more
        ;; font-lock-unfontify-region-function
        ;; font-lock-unfontify-buffer-function
        
        ;; Don't fontify eagerly (and don't abort if the buffer is large).
        (setq-local font-lock-fontified t)

        ;; Now we can finally call `font-lock-default-function' because
        ;; `font-lock-support-mode' is set to "unrecognizible" value. Thus only
        ;; core font-lock setup happens.
        (font-lock-default-function arg)

        ;; We are using this in `poly-lock-after-change' below.
        (add-hook 'jit-lock-after-change-extend-region-functions
                  'font-lock-extend-jit-lock-region-after-change
                  nil t))
    
    (remove-hook 'after-change-functions 'poly-lock-after-change t)
    (remove-hook 'fontification-functions 'poly-lock-fontification-function t))
  (current-buffer))

(defun poly-lock-fontification-function (start)
  "The only function in `fontification-functions'.
This is the entry point called by the display engine. START is
defined in `fontification-functions'."
  (if poly-lock-allow-fontification
      (when (and poly-lock-mode
                 (not memory-full))
        (unless (input-pending-p)
          (let ((end (next-single-property-change
                      start 'fontified nil (point-max))))
            (poly-lock-fontify-region start end))))
    (put-text-property start (point-max) 'fontified t)))

(defun poly-lock-after-change (beg end old-len)
  "Mark changed region as not fontified after change.
Installed on `after-change-functions'."
  (when (and poly-lock-mode poly-lock-allow-after-change
             (not memory-full))
    (let ((jit-lock-start beg)
          (jit-lock-end end)
          ;; useful info for tracing
          (gl-beg end)
          (gl-end beg)
          exp-error)
      (save-excursion
        (condition-case err
            ;; This sets jit-lock-start and jit-lock-end.
            (run-hook-with-args 'jit-lock-after-change-extend-region-functions
                                beg end old-len)
          (error (message "(poly-lock-after-change:jl-expand (%s %s %s)): %s"
                          beg end old-len (error-message-string err))
                 (setq jit-lock-start beg
                       jit-lock-end end)))
        (pm-map-over-spans
         (lambda ()
           (with-buffer-prepared-for-poly-lock
            (let ((sbeg (nth 1 *span*))
                  (send (nth 2 *span*)))
              (save-restriction
                (widen)
                (setq gl-beg (min gl-beg (max jit-lock-start sbeg))
                      gl-end (max gl-beg jit-lock-end send))
                (put-text-property gl-beg gl-end 'fontified nil)))))
         beg end nil nil nil 'no-cache)
        (cons gl-beg gl-end)))))

(defun poly-lock-fontify-region (beg end &optional verbose)
  "Polymode font-lock fontification function.
Fontifies chunk-by chunk within the region. Assigned to
`font-lock-fontify-region-function'.

A fontification mechanism should call
`font-lock-fontify-region-function' (`jit-lock-function' does
that). If it does not, the fontification will probably be screwed
in polymode buffers."
  (unless poly-lock-fontification-in-progress
    (let* ((font-lock-dont-widen t)
           (pmarker (point-marker))
           (dbuffer (current-buffer))
           (pm--restrict-widen t)
           ;; Fontification in one buffer can trigger fontification in another
           ;; buffer. Particularly, this happens when new indirect buffers are
           ;; created and `normal-mode' triggers font-lock in those
           ;; buffers. We avoid this by dynamically binding
           ;; `poly-lock-fontification-in-progress' and un-setting
           ;; `fontification-functions' in case re-display suddenly decides to
           ;; fontify something else in other buffer.
           (poly-lock-fontification-in-progress t)
           (fontification-functions nil))
      (save-restriction
        (widen)
        (save-excursion
          (pm-map-over-spans
           (lambda ()
             (with-buffer-prepared-for-poly-lock
              (let ((sbeg (nth 1 *span*))
                    (send (nth 2 *span*)))
                (when (and font-lock-mode font-lock-keywords (> send sbeg))
                  (when parse-sexp-lookup-properties
                    (pm--comment-region 1 sbeg))
                  (let ((new-beg (max sbeg beg))
                        (new-end (min send end)))

                    (condition-case-unless-debug err
                        (if (oref pm/chunkmode :font-lock-narrow)
                            (save-restriction
                              ;; fixme: optimization opportunity: Cache
                              ;; chunk state in text properties. For big
                              ;; chunks font-lock fontifies it by smaller
                              ;; segments, thus poly-lock-fontify-region is
                              ;; called multiple times per chunk and spans
                              ;; are re-computed each time.
                              (narrow-to-region sbeg send)
                              (font-lock-unfontify-region new-beg new-end)
                              (funcall pm--fontify-region-original new-beg new-end verbose))
                          (funcall pm--fontify-region-original new-beg new-end verbose))
                      
                      ;; Don't change this error string; it is used in
                      ;; `pm-debug-fontify-last-font-lock-error'
                      (error (message "(poly-lock-fontify-region %s %s) -> (%s %s %s %s): %s "
                                      beg end pm--fontify-region-original new-beg new-end verbose
                                      (error-message-string err))))
                    ;; even if failed or no font-lock, set to t
                    (put-text-property new-beg new-end 'fontified t))
                  
                  (when parse-sexp-lookup-properties
                    (pm--uncomment-region 1 sbeg)))
                
                (pm--adjust-chunk-face sbeg send (pm-get-adjust-face pm/chunkmode)))))
           beg end)))))
  (current-buffer))

(defun poly-lock-refontify (&optional beg end)
  "Force refontification of the region BEG..END.
END is extended to the next chunk separator. This function is
pleased in `font-lock-flush-function' and
`font-lock-ensure-function'"
  (when (and poly-lock-allow-fontification
             (not poly-lock-fontification-in-progress))
    (with-buffer-prepared-for-poly-lock
     (save-restriction
       (widen)
       (cond ((and beg end)
              (setq end (cdr (pm-get-innermost-range end))))
             (beg
              (setq end (cdr (pm-get-innermost-range beg))))
             (t
              (setq beg (point-min)
                    end (point-max))))
       (put-text-property beg end 'fontified nil)))))


(provide 'poly-lock)
