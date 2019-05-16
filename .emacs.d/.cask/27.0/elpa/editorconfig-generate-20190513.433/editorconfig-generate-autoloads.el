;;; editorconfig-generate-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "editorconfig-generate" "editorconfig-generate.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from editorconfig-generate.el

(autoload 'editorconfig-generate "editorconfig-generate" "\
Generate EditorConfig content for buffer BUF.
if BUF is omitted or nil, works for current buffer.

\(fn &optional BUF)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "editorconfig-generate" '("editorconfig-generate-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; editorconfig-generate-autoloads.el ends here
