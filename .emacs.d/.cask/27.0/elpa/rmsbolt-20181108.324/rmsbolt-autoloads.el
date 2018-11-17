;;; rmsbolt-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "rmsbolt" "rmsbolt.el" (0 0 0 0))
;;; Generated autoloads from rmsbolt.el

(autoload 'rmsbolt-starter "rmsbolt" "\
Setup new file based on the sample STARTER-FILE-NAME.

\(fn LANG-NAME)" t nil)

(autoload 'rmsbolt-mode "rmsbolt" "\
Toggle rmsbolt-mode.

If called interactively, enable Rmsbolt mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it
if ARG is `toggle'; disable the mode otherwise.

This mode is enabled both in modes to be compiled and output buffers.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rmsbolt" '("rmsbolt-")))

;;;***

;;;### (autoloads nil "rmsbolt-java" "rmsbolt-java.el" (0 0 0 0))
;;; Generated autoloads from rmsbolt-java.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rmsbolt-java" '("rmsbolt-java-")))

;;;***

;;;### (autoloads nil "rmsbolt-split" "rmsbolt-split.el" (0 0 0 0))
;;; Generated autoloads from rmsbolt-split.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rmsbolt-split" '("rmsbolt-split-")))

;;;***

;;;### (autoloads nil nil ("rmsbolt-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rmsbolt-autoloads.el ends here
