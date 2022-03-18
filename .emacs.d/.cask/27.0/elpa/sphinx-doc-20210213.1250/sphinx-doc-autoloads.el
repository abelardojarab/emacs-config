;;; sphinx-doc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "sphinx-doc" "sphinx-doc.el" (0 0 0 0))
;;; Generated autoloads from sphinx-doc.el

(autoload 'sphinx-doc-mode "sphinx-doc" "\
Sphinx friendly docstring generation for Python code.

If called interactively, enable Sphinx-Doc mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sphinx-doc" '("sphinx-doc")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sphinx-doc-autoloads.el ends here
