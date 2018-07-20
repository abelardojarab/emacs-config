;;; switch-buffer-functions-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "switch-buffer-functions" "switch-buffer-functions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from switch-buffer-functions.el

(defvar switch-buffer-functions nil "\
A list of functions to be called when the current buffer has been changed.
Each is passed two arguments, the previous buffer and the current buffer.")

(autoload 'switch-buffer-functions-run "switch-buffer-functions" "\
Run `switch-buffer-functions' if needed.

This function checks the result of `current-buffer', and run
`switch-buffer-functions' when it has been changed from
the last buffer.

This function should be hooked to `post-command-hook'.

\(fn)" nil nil)

(add-hook 'post-command-hook 'switch-buffer-functions-run)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "switch-buffer-functions" '("switch-buffer-functions--last-buffer")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; switch-buffer-functions-autoloads.el ends here
