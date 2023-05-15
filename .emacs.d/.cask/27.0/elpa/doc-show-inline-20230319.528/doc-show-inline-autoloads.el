;;; doc-show-inline-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "doc-show-inline" "doc-show-inline.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from doc-show-inline.el

(autoload 'doc-show-inline-buffer "doc-show-inline" "\
Calculate overlays for the whole buffer." t nil)

(autoload 'doc-show-inline-mode "doc-show-inline" "\
Toggle variable `doc-show-inline-mode' in the current buffer.

This is a minor mode.  If called interactively, toggle the
`Doc-Show-Inline mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the
mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `doc-show-inline-mode'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "doc-show-inline" '("doc-show-inline-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; doc-show-inline-autoloads.el ends here
