;;; ws-butler-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ws-butler" "ws-butler.el" (0 0 0 0))
;;; Generated autoloads from ws-butler.el

(autoload 'ws-butler-mode "ws-butler" "\
White space cleanup, without obtrusive white space removal.

This is a minor mode.  If called interactively, toggle the
`Ws-Butler mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `ws-butler-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Whitespaces at EOL and EOF are trimmed upon file save, and only
for lines modified by you.

\(fn &optional ARG)" t nil)

(put 'ws-butler-global-mode 'globalized-minor-mode t)

(defvar ws-butler-global-mode nil "\
Non-nil if Ws-Butler-Global mode is enabled.
See the `ws-butler-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ws-butler-global-mode'.")

(custom-autoload 'ws-butler-global-mode "ws-butler" nil)

(autoload 'ws-butler-global-mode "ws-butler" "\
Toggle Ws-Butler mode in all buffers.
With prefix ARG, enable Ws-Butler-Global mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Ws-Butler mode is enabled in all buffers where `(lambda nil (unless
\(apply #'derived-mode-p ws-butler-global-exempt-modes)
\(ws-butler-mode)))' would do it.

See `ws-butler-mode' for more information on Ws-Butler mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "ws-butler" '("ws-butler-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ws-butler-autoloads.el ends here
