;;; apache-mode-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from apache-mode.el

(autoload 'apache-mode "apache-mode" "\
Major mode for editing Apache configuration files.

(fn)" t)
(add-to-list 'auto-mode-alist '("/\\.htaccess\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("/\\(?:access\\|httpd\\|srm\\)\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("/apache2/.+\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("/httpd/conf/.+\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("/apache2/sites-\\(?:available\\|enabled\\)/" . apache-mode))
(register-definition-prefixes "apache-mode" '("apache-"))

;;; End of scraped data

(provide 'apache-mode-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; apache-mode-autoloads.el ends here