;;; outshine-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "outshine" "outshine.el" (0 0 0 0))
;;; Generated autoloads from outshine.el

(autoload 'outshine-mode "outshine" "\
Outshine brings the look&feel of Org-mode to the (GNU Emacs)
world outside of the Org major-mode.

This is a minor mode.  If called interactively, toggle the
`Outshine mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `outshine-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'outshine-hook-function "outshine" "\
DEPRECATED, use `outshine-mode'." nil nil)

(register-definition-prefixes "outshine" '("outshine-"))

;;;***

;;;### (autoloads nil "outshine-org-cmds" "outshine-org-cmds.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from outshine-org-cmds.el

(register-definition-prefixes "outshine-org-cmds" '("outshine-"))

;;;***

;;;### (autoloads nil nil ("outshine-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; outshine-autoloads.el ends here
