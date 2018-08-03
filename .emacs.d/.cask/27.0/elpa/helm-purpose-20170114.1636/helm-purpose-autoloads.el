;;; helm-purpose-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-purpose" "helm-purpose.el" (0 0 0 0))
;;; Generated autoloads from helm-purpose.el

(autoload 'helm-purpose-mini-ignore-purpose "helm-purpose" "\
Same as `helm-mini', but disable window-purpose while the command executes.

\(fn)" t nil)

(autoload 'helm-purpose-switch-buffer-with-purpose "helm-purpose" "\
Switch to buffer, choose from buffers with purpose PURPOSE.
PURPOSE defaults to the purpose of the current buffer.

\(fn &optional PURPOSE)" t nil)

(autoload 'helm-purpose-switch-buffer-with-some-purpose "helm-purpose" "\
Choose a purpose, then switch to a buffer with that purpose.

\(fn)" t nil)

(autoload 'helm-purpose-setup "helm-purpose" "\
Setup Helm interface for Purpose.
Currently just sets `purpose-preferred-prompt' to 'helm.
Doesn't bind any keys.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-purpose" '("helm-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-purpose-autoloads.el ends here
