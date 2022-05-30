;;; jenkinsfile-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "jenkinsfile-mode" "jenkinsfile-mode.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from jenkinsfile-mode.el

(autoload 'jenkinsfile-mode "jenkinsfile-mode" "\
Major mode for editing Jenkins declarative pipeline files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("Jenkinsfile\\'" . jenkinsfile-mode))

(register-definition-prefixes "jenkinsfile-mode" '("jenkinsfile-mode-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; jenkinsfile-mode-autoloads.el ends here
