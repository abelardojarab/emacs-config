;;; hlinum-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "hlinum" "hlinum.el" (0 0 0 0))
;;; Generated autoloads from hlinum.el

(autoload 'hlinum-activate "hlinum" "\
Enable highlighting current line number.

\(fn)" t nil)

(autoload 'hlinum-deactivate "hlinum" "\
Disable highlighting current line number.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hlinum" '("hlinum-" "linum-highlight-in-all-buffersp")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; hlinum-autoloads.el ends here
