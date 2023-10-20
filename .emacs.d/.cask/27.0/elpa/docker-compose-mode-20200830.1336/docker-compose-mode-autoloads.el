;;; docker-compose-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "docker-compose-mode" "docker-compose-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from docker-compose-mode.el

(autoload 'docker-compose-mode "docker-compose-mode" "\
Major mode to edit docker-compose files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("docker-compose[^/]*\\.ya?ml\\'" . docker-compose-mode))

(register-definition-prefixes "docker-compose-mode" '("docker-compose-"))

;;;***

;;;### (autoloads nil nil ("docker-compose-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; docker-compose-mode-autoloads.el ends here
