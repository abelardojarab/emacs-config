;;; flycheck-posframe-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flycheck-posframe" "flycheck-posframe.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from flycheck-posframe.el

(autoload 'flycheck-posframe-configure-pretty-defaults "flycheck-posframe" "\
Configure some nicer settings for prettier display." nil nil)

(autoload 'flycheck-posframe-mode "flycheck-posframe" "\
A minor mode to show Flycheck error messages in a posframe.

This is a minor mode.  If called interactively, toggle the
`Flycheck-Posframe mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `flycheck-posframe-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "flycheck-posframe" '("flycheck-posframe-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flycheck-posframe-autoloads.el ends here
