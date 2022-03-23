;;; org-modern-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-modern" "org-modern.el" (0 0 0 0))
;;; Generated autoloads from org-modern.el

(autoload 'org-modern-mode "org-modern" "\
Modern looks for Org.

If called interactively, enable Org-Modern mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'org-modern-agenda "org-modern" "\
Finalize Org agenda highlighting." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-modern" '("org-modern-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-modern-autoloads.el ends here
