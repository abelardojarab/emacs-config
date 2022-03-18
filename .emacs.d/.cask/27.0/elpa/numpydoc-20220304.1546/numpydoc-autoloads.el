;;; numpydoc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "numpydoc" "numpydoc.el" (0 0 0 0))
;;; Generated autoloads from numpydoc.el

(autoload 'numpydoc-use-yasnippet "numpydoc" "\
Enable yasnippet insertion (see `numpydoc-insertion-style')." t nil)

(autoload 'numpydoc-use-prompt "numpydoc" "\
Enable minibuffer prompt insertion (see `numpydoc-insertion-style')." t nil)

(autoload 'numpydoc-use-templates "numpydoc" "\
Enable template text insertion (see `numpydoc-insertion-style')." t nil)

(autoload 'numpydoc-generate "numpydoc" "\
Generate NumPy style docstring for Python function.
Assumes that the current location of the cursor is somewhere in the
function that is being documented." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "numpydoc" '("numpydoc-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; numpydoc-autoloads.el ends here
