;;; helm-grepint-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-grepint" "helm-grepint.el" (0 0 0 0))
;;; Generated autoloads from helm-grepint.el

(autoload 'helm-grepint-grep "helm-grepint" "\
Run grep in the current directory.

See the usage for ARG in `helm-grepint--grep'.

The grep function is determined by the contents of
`helm-grepint-grep-configs' and the order of `helm-grepint-grep-list'.

\(fn &optional ARG)" t nil)

(autoload 'helm-grepint-grep-root "helm-grepint" "\
Function `helm-grepint-grep' is run in a root directory.

See the usage for ARG in `helm-grepint--grep'.

\(fn &optional ARG)" t nil)

(autoload 'helm-grepint-set-default-config-v1\.0\.0 "helm-grepint" "\
Set the default grep configuration into `helm-grepint-grep-configs' and `helm-grepint-grep-list'." nil nil)

(autoload 'helm-grepint-set-default-config-v1\.1\.0 "helm-grepint" "\
Set default grep configuration.

Run `helm-grepint-set-default-config-v1.0.0' and then this function.

Adds configuration for running ag if file set in
`helm-grepint-default-config-ag-presearch-marker-file' is found
in a git repository before the git root.  The use case is running
this in huge git repositories and wanting to limit the searching
to a subdirectory." nil nil)

(fset 'helm-grepint-set-default-config #'helm-grepint-set-default-config-v1\.0\.0)

(fset 'helm-grepint-set-default-config-latest #'helm-grepint-set-default-config-v1\.2\.0)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-grepint" '("helm-grepint-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-grepint-autoloads.el ends here
