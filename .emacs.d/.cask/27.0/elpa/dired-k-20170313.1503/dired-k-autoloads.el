;;; dired-k-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dired-k" "dired-k.el" (0 0 0 0))
;;; Generated autoloads from dired-k.el

(autoload 'dired-k-no-revert "dired-k" "\
Same as `dired-k' except not calling `revert-buffer'.

\(fn)" t nil)

(autoload 'dired-k "dired-k" "\
Highlighting dired buffer by file size, last modified time, and git status.
This is inspired by `k' zsh script

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dired-k" '("dired-k-")))

;;;***

;;;### (autoloads nil "direx-k" "direx-k.el" (0 0 0 0))
;;; Generated autoloads from direx-k.el

(autoload 'direx-k "direx-k" "\


\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "direx-k" '("direx-k--")))

;;;***

;;;### (autoloads nil nil ("dired-k-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dired-k-autoloads.el ends here
