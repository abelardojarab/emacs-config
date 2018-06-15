;;; auto-dim-other-buffers-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "auto-dim-other-buffers" "auto-dim-other-buffers.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from auto-dim-other-buffers.el

(defvar auto-dim-other-buffers-mode nil "\
Non-nil if Auto-Dim-Other-Buffers mode is enabled.
See the `auto-dim-other-buffers-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `auto-dim-other-buffers-mode'.")

(custom-autoload 'auto-dim-other-buffers-mode "auto-dim-other-buffers" nil)

(autoload 'auto-dim-other-buffers-mode "auto-dim-other-buffers" "\
Visually makes non-current buffers less prominent

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "auto-dim-other-buffers" '("auto-dim-other-buffers-dim-on-" "adob--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; auto-dim-other-buffers-autoloads.el ends here
