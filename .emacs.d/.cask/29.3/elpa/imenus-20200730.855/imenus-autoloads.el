;;; imenus-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from imenus.el

(autoload 'imenus "imenus" "\
Prompt for a place from a list of BUFFERS and jump to it.
Interactively, use the current buffer.  With a prefix argument,
prompt for multiple buffers.

In a minibuffer prompt you may use the following commands:
\\{imenus-minibuffer-map}

(fn BUFFERS)" t)
(autoload 'imenus-mode-buffers "imenus" "\
Perform `imenus' on all buffers with MODE.
Interactively, use the major mode of the current buffer.

(fn MODE)" t)
(register-definition-prefixes "imenus" '("imenus-"))

;;; End of scraped data

(provide 'imenus-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; imenus-autoloads.el ends here
