;;; yatemplate-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "test-yatemplate" "test-yatemplate.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from test-yatemplate.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "test-yatemplate" '("touch" "yatemplate")))

;;;***

;;;### (autoloads nil "yatemplate" "yatemplate.el" (0 0 0 0))
;;; Generated autoloads from yatemplate.el

(defvar yatemplate-dir (locate-user-emacs-file "templates") "\
The directory containing file templates.")

(custom-autoload 'yatemplate-dir "yatemplate" t)

(defvar yatemplate-separator ":" "\
Separator used for splitting filenames.

This is the separator that is used to split template filenames into
the ordering and regular expression parts.

Note that this will be used as the SEPARATORS argument of
`split-string', so be careful when setting this to a value that
has special meaning in regular expressions.")

(custom-autoload 'yatemplate-separator "yatemplate" t)

(put 'yatemplate-owner 'safe-local-variable #'stringp)

(put 'yatemplate-license 'safe-local-variable #'stringp)

(autoload 'yatemplate-fill-alist "yatemplate" "\
Fill `auto-insert-alist'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yatemplate" '("yatemplate-")))

;;;***

;;;### (autoloads nil nil ("yatemplate-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; yatemplate-autoloads.el ends here
