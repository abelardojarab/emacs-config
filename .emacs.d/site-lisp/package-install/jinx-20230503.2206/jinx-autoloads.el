;;; jinx-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "jinx" "jinx.el" (0 0 0 0))
;;; Generated autoloads from jinx.el

(put 'jinx-languages 'safe-local-variable #'stringp)

(put 'jinx-local-words 'safe-local-variable #'stringp)

(autoload 'jinx-languages "jinx" "\
Set languages locally or globally to LANGS.
With prefix argument GLOBAL change the languages globally.

\(fn LANGS &optional GLOBAL)" t nil)

(autoload 'jinx-correct "jinx" "\
Correct nearest misspelled word.
If prefix argument ALL non-nil correct all misspellings.

\(fn &optional ALL)" t nil)

(autoload 'jinx-mode "jinx" "\
Enchanted Spell Checker.

This is a minor mode.  If called interactively, toggle the `Jinx
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `jinx-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-jinx-mode 'globalized-minor-mode t)

(defvar global-jinx-mode nil "\
Non-nil if Global Jinx mode is enabled.
See the `global-jinx-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-jinx-mode'.")

(custom-autoload 'global-jinx-mode "jinx" nil)

(autoload 'global-jinx-mode "jinx" "\
Toggle Jinx mode in all buffers.
With prefix ARG, enable Global Jinx mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Jinx mode is enabled in all buffers where `jinx--on' would do it.

See `jinx-mode' for more information on Jinx mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "jinx" '("jinx-"))

;;;***

;;;### (autoloads nil nil ("jinx-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; jinx-autoloads.el ends here
