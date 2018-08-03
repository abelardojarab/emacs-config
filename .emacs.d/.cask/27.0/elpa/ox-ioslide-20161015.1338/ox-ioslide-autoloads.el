;;; ox-ioslide-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ox-ioslide" "ox-ioslide.el" (0 0 0 0))
;;; Generated autoloads from ox-ioslide.el

(autoload 'org-ioslide-export-as-html "ox-ioslide" "\
Export current buffer to an HTML buffer.

Export is done in a buffer named \"*Org HTML5 Slide Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

(autoload 'org-ioslide-export-to-html "ox-ioslide" "\
Export current buffer to a Google ioslide HTML5 slide HTML file.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-ioslide" '("org-ioslide-")))

;;;***

;;;### (autoloads nil "ox-ioslide-helper" "ox-ioslide-helper.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ox-ioslide-helper.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-ioslide-helper" '("ioslide")))

;;;***

;;;### (autoloads nil nil ("ox-ioslide-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ox-ioslide-autoloads.el ends here
