;;; popper-autoloads.el --- automatically extracted autoloads
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

If called interactively, enable Popper mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "popper" '("popper-")))

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

If called interactively, enable Popper-Echo mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

To define buffers as popups and customize popup display, see
`popper-mode'.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "popper-echo" '("popper-echo")))

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
