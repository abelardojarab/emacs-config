;;; nova-theme-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from nova-theme.el

(autoload 'nova--build-face "nova-theme" "\
Internal helper to turn FACE into proper face spec.

(fn FACE)")
(autoload 'nova-darken "nova-theme" "\
Darken given rgb string COLOR by ALPHA (0-1).

(fn COLOR ALPHA)")
(autoload 'nova-with-colors "nova-theme" "\
Macro to make color variables available to BODY.

(fn &rest BODY)" nil t)
(function-put 'nova-with-colors 'lisp-indent-function 'defun)
(autoload 'nova-blend "nova-theme" "\
Combine A C1 with (1-a) C2.

(fn C1 C2 A)")
(autoload 'nova-lighten "nova-theme" "\
Lighten given rgb string COLOR by ALPHA (0-1).

(fn COLOR ALPHA)")
(when (and load-file-name (boundp 'custom-theme-load-path)) (add-to-list 'custom-theme-load-path (file-name-directory load-file-name)))
(register-definition-prefixes "nova-theme" '("nova"))

;;; End of scraped data

(provide 'nova-theme-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; nova-theme-autoloads.el ends here