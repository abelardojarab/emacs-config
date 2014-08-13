;; -*- Emacs-Lisp -*-

;; Time-stamp: <2012-05-25 20:35:29 (ajaraber)>

(defun eldoc-settings ()
  "settings for `eldoc'."
  (defun eldoc-print-current-symbol-info-anyway ()
    "Print current symbol info."
    (interactive)
    (condition-case err
        (if eldoc-documentation-function
            (eldoc-message (funcall eldoc-documentation-function))
          (let* ((current-symbol (eldoc-current-symbol))
                 (current-fnsym  (eldoc-fnsym-in-current-sexp))
                 (doc (cond
                       ((null current-fnsym)
                        nil)
                       ((eq current-symbol (car current-fnsym))
                        (or (apply 'eldoc-get-fnsym-args-string
                                   current-fnsym)
                            (eldoc-get-var-docstring current-symbol)))
                       (t
                        (or (eldoc-get-var-docstring current-symbol)
                            (apply 'eldoc-get-fnsym-args-string
                                   current-fnsym))))))
            (eldoc-message doc)))
      ;; This is run from post-command-hook or some idle timer thing,
      ;; so we need to be careful that errors aren't ignored.
      (error (message "eldoc error: %s" err))))

  (defun eldoc-pre-command-refresh-echo-area ())

  (setq eldoc-idle-delay 0.5)

  (eldoc-add-command 'describe-symbol-at-point 'View-scroll-half-page-backward 'l-command
                     'save-buffer-sb 'switch-to-other-buffer)
  (eldoc-remove-command 'goto-paren))

(eval-after-load "eldoc"
  `(eldoc-settings))

(provide 'eldoc-settings)
