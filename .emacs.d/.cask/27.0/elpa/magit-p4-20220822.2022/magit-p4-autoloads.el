;;; magit-p4-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "magit-p4" "magit-p4.el" (0 0 0 0))
;;; Generated autoloads from magit-p4.el

(autoload 'magit-p4-clone "magit-p4" "\
Clone given DEPOT-PATH.

The first argument is P4 depot path to clone.  The TARGET-DIR
argument is directory which will hold the Git repository.

\(fn DEPOT-PATH &optional TARGET-DIR)" t nil)

(autoload 'magit-p4-sync "magit-p4" "\
Synchronize with default and/or given DEPOT-PATH.

The optional argument is P4 depot path which will be synchronized
with.  If not present, git-p4 will try to synchronize with default
depot path which has been cloned to before.

\(fn &optional DEPOT-PATH)" t nil)

(autoload 'magit-p4-rebase "magit-p4" "\
Run git-p4 rebase." t nil)

(autoload 'magit-p4-submit "magit-p4" "\
Run git-p4 submit." t nil)

(autoload 'magit-p4-run-git-with-editor "magit-p4" "\
Run git with P4EDITOR set and `magit-p4-process-filter'.
This is similar to `magit-run-git-with-editor', but also export
P4EDITOR and use custom process filter `magit-p4-process-filter'.

\(fn &rest ARGS)" nil nil)
 (autoload 'magit-p4-popup "magit-p4" nil t)

(autoload 'magit-p4-mode "magit-p4" "\
P4 support for Magit.

This is a minor mode.  If called interactively, toggle the
`Magit-P4 mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `magit-p4-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "magit-p4" '("magit-p4"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; magit-p4-autoloads.el ends here