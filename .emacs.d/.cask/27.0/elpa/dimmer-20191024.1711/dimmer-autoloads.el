;;; dimmer-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dimmer" "dimmer.el" (0 0 0 0))
;;; Generated autoloads from dimmer.el

(defvar dimmer-mode nil "\
Non-nil if Dimmer mode is enabled.
See the `dimmer-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dimmer-mode'.")

(custom-autoload 'dimmer-mode "dimmer" nil)

(autoload 'dimmer-mode "dimmer" "\
visually highlight the selected buffer

If called interactively, enable Dimmer mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it
if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(define-obsolete-function-alias 'dimmer-activate 'dimmer-mode)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dimmer" '("dimmer-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dimmer-autoloads.el ends here
