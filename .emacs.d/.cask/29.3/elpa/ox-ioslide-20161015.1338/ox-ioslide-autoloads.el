;;; ox-ioslide-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from ox-ioslide.el

(autoload 'org-ioslide-export-as-html "ox-ioslide" "\
Export current buffer to an HTML buffer.

Export is done in a buffer named \"*Org HTML5 Slide Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil.

(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t)
(autoload 'org-ioslide-export-to-html "ox-ioslide" "\
Export current buffer to a Google ioslide HTML5 slide HTML file.

(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t)
(register-definition-prefixes "ox-ioslide" '("org-ioslide-"))


;;; Generated autoloads from ox-ioslide-helper.el

(register-definition-prefixes "ox-ioslide-helper" '("ioslide"))

;;; End of scraped data

(provide 'ox-ioslide-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; ox-ioslide-autoloads.el ends here