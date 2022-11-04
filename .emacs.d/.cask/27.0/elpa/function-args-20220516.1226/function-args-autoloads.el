;;; function-args-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "function-args" "function-args.el" (0 0 0 0))
;;; Generated autoloads from function-args.el

(autoload 'function-args-mode "function-args" "\
Minor mode for C++ code completion bindings.

This is a minor mode.  If called interactively, toggle the
`Function-Args mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `function-args-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{function-args-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'fa-config-default "function-args" "\
Set up default key bindings." nil nil)

(register-definition-prefixes "function-args" '("fa-" "filter" "function-args-mode-map" "moo-" "turn-on-function-args-mode"))

;;;***

;;;### (autoloads nil "semantic-directory" "semantic-directory.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from semantic-directory.el

(register-definition-prefixes "semantic-directory" '("sd-"))

;;;***

;;;### (autoloads nil nil ("function-args-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; function-args-autoloads.el ends here
