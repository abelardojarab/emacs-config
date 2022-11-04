;;; popper-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "popper" "popper.el" (0 0 0 0))
;;; Generated autoloads from popper.el

(defvar popper-mode nil "\
Non-nil if Popper mode is enabled.
See the `popper-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `popper-mode'.")

(custom-autoload 'popper-mode "popper" nil)

(autoload 'popper-mode "popper" "\
Toggle Popper mode. When enabled, treat certain buffer
windows as popups, a class of window that can be summoned or
dismissed with a command. See the customization options for
details on how to designate buffer types as popups.

This is a minor mode.  If called interactively, toggle the
`Popper mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='popper-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "popper" '("popper-"))

;;;***

;;;### (autoloads nil "popper-echo" "popper-echo.el" (0 0 0 0))
;;; Generated autoloads from popper-echo.el

(defvar popper-echo-mode nil "\
Non-nil if Popper-Echo mode is enabled.
See the `popper-echo-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `popper-echo-mode'.")

(custom-autoload 'popper-echo-mode "popper-echo" nil)

(autoload 'popper-echo-mode "popper-echo" "\
Show popup names in cycling order in the echo area when
  performing an action that involves showing a popup. These
  popups can be accessed directly or acted upon by using quick
  keys (see `popper-echo-dispatch-keys').

This is a minor mode.  If called interactively, toggle the
`Popper-Echo mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='popper-echo-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

To define buffers as popups and customize popup display, see
`popper-mode'.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "popper-echo" '("popper-echo"))

;;;***

;;;### (autoloads nil nil ("popper-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; popper-autoloads.el ends here
