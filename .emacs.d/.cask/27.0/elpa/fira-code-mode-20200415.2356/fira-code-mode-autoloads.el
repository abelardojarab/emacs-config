;;; fira-code-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "fira-code-mode" "fira-code-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from fira-code-mode.el

(autoload 'fira-code-mode "fira-code-mode" "\
Fira Code ligatures minor mode

If called interactively, enable Fira-Code mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it
if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(defvar global-fira-code-mode nil "\
Non-nil if Global Fira-Code mode is enabled.
See the `global-fira-code-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-fira-code-mode'.")

(custom-autoload 'global-fira-code-mode "fira-code-mode" nil)

(autoload 'global-fira-code-mode "fira-code-mode" "\
Toggle Fira-Code mode in all buffers.
With prefix ARG, enable Global Fira-Code mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Fira-Code mode is enabled in all buffers where
`fira-code-mode' would do it.
See `fira-code-mode' for more information on Fira-Code mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "fira-code-mode" '("fira-code-mode-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; fira-code-mode-autoloads.el ends here
