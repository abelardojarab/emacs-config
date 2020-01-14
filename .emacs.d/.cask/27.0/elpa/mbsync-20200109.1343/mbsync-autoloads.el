;;; mbsync-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mbsync" "mbsync.el" (0 0 0 0))
;;; Generated autoloads from mbsync.el

(autoload 'mbsync "mbsync" "\
Run the `mbsync' command, asynchronously, then run `mbsync-exit-hook'.
If SHOW-BUFFER, also show the *mbsync* output.

\(fn &optional SHOW-BUFFER)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mbsync" '("mbsync-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mbsync-autoloads.el ends here
