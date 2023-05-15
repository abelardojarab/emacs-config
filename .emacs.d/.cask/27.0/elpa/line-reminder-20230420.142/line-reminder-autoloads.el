;;; line-reminder-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "line-reminder" "line-reminder.el" (0 0 0 0))
;;; Generated autoloads from line-reminder.el

(autoload 'line-reminder-mode "line-reminder" "\
Minor mode `line-reminder-mode'.

This is a minor mode.  If called interactively, toggle the
`Line-Reminder mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `line-reminder-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-line-reminder-mode 'globalized-minor-mode t)

(defvar global-line-reminder-mode nil "\
Non-nil if Global Line-Reminder mode is enabled.
See the `global-line-reminder-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-line-reminder-mode'.")

(custom-autoload 'global-line-reminder-mode "line-reminder" nil)

(autoload 'global-line-reminder-mode "line-reminder" "\
Toggle Line-Reminder mode in all buffers.
With prefix ARG, enable Global Line-Reminder mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Line-Reminder mode is enabled in all buffers where
`line-reminder--turn-on-line-reminder-mode' would do it.

See `line-reminder-mode' for more information on Line-Reminder mode.

\(fn &optional ARG)" t nil)

(autoload 'line-reminder-clear-reminder-lines-sign "line-reminder" "\
Clear all the reminder lines' sign." t nil)

(autoload 'line-reminder-transfer-to-saved-lines "line-reminder" "\
Transfer the `change-lines' to `saved-lines'." t nil)

(register-definition-prefixes "line-reminder" '("line-reminder-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; line-reminder-autoloads.el ends here
