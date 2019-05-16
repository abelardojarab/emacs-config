;;; helm-mu-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-mu" "helm-mu.el" (0 0 0 0))
;;; Generated autoloads from helm-mu.el

(autoload 'helm-mu "helm-mu" "\
Search for emails.  If started in mu4e-headers-view, the
current query will be used to initialize the search.  Otherwise
`helm-mu-default-search-string' will be used.

\(fn)" t nil)

(autoload 'helm-mu-contacts "helm-mu" "\
Search for contacts.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-mu" '("helm-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-mu-autoloads.el ends here
