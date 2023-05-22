;;; consult-dir-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "consult-dir" "consult-dir.el" (0 0 0 0))
;;; Generated autoloads from consult-dir.el

(autoload 'consult-dir-jump-file "consult-dir" "\
Jump to file from the directory in the minibuffer prompt." t nil)

(autoload 'consult-dir "consult-dir" "\
Choose a directory and act on it.

The action taken on the directory is the value of
`consult-dir-default-command'. The default is to call
`find-file' starting at this directory.

When called from the minibuffer, insert the directory into the
minibuffer prompt instead. Existing minibuffer contents will be
shadowed or deleted depending on the value of
`consult-dir-shadow-filenames'.

The list of sources for directory paths is
`consult-dir-sources', which can be customized." t nil)

(register-definition-prefixes "consult-dir" '("consult-dir-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; consult-dir-autoloads.el ends here
