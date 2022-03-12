;;; function-args-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "function-args" "function-args.el" (0 0 0 0))
;;; Generated autoloads from function-args.el

(autoload 'function-args-mode "function-args" "\
Minor mode for C++ code completion bindings.

If called interactively, enable Function-Args mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\\{function-args-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'fa-config-default "function-args" "\
Set up default key bindings." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "function-args" '("fa-" "filter" "function-args-mode-map" "moo-" "turn-on-function-args-mode")))

;;;***

;;;### (autoloads nil "semantic-directory" "semantic-directory.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from semantic-directory.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "semantic-directory" '("sd-")))

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
