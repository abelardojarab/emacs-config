;;; smart-tab-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from smart-tab.el

(autoload 'smart-tab "smart-tab" "\
Try to 'do the smart thing' when tab is pressed.
`smart-tab' attempts to expand the text before the point or
indent the current line or selection.

In a regular buffer, `smart-tab' will attempt to expand with
either `hippie-expand' or `dabbrev-expand', depending on the
value of `smart-tab-using-hippie-expand'.  Alternatively, if a
`smart-tab-user-provided-completion-function' is defined, it will
be used to attempt expansion.  If the mark is active, or PREFIX is
\\[universal-argument], then `smart-tab' will indent the region
or the current line (if the mark is not active).

(fn &optional PREFIX)" t)
(autoload 'smart-tab-mode-on "smart-tab" "\
Turn on `smart-tab-mode'.")
(autoload 'smart-tab-mode "smart-tab" "\
Enable `smart-tab' to be used in place of tab.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

This is a minor mode.  If called interactively, toggle the
`Smart-Tab mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `smart-tab-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(put 'global-smart-tab-mode 'globalized-minor-mode t)
(defvar global-smart-tab-mode nil "\
Non-nil if Global Smart-Tab mode is enabled.
See the `global-smart-tab-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-smart-tab-mode'.")
(custom-autoload 'global-smart-tab-mode "smart-tab" nil)
(autoload 'global-smart-tab-mode "smart-tab" "\
Toggle Smart-Tab mode in all buffers.
With prefix ARG, enable Global Smart-Tab mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Smart-Tab mode is enabled in all buffers where `smart-tab-mode-on'
would do it.

See `smart-tab-mode' for more information on Smart-Tab mode.

(fn &optional ARG)" t)
(register-definition-prefixes "smart-tab" '("smart-tab-"))

;;; End of scraped data

(provide 'smart-tab-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; smart-tab-autoloads.el ends here