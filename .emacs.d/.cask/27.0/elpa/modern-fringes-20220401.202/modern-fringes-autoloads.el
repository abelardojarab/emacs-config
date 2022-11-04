;;; modern-fringes-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "modern-fringes" "modern-fringes.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from modern-fringes.el

(autoload 'modern-fringes-invert-arrows "modern-fringes" "\
Apply ideal colors for the fringe truncation arrows in a flexible manner.
Should be used before (modern-fringes-mode) is enabled in the user's init file." t nil)

(defvar modern-fringes-mode nil "\
Non-nil if Modern-Fringes mode is enabled.
See the `modern-fringes-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `modern-fringes-mode'.")

(custom-autoload 'modern-fringes-mode "modern-fringes" nil)

(autoload 'modern-fringes-mode "modern-fringes" "\
Toggle Modern-Fringes mode on or off.

This is a minor mode.  If called interactively, toggle the
`Modern-Fringes mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='modern-fringes-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{modern-fringes-mode-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "modern-fringes" '("modern-fringes--"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; modern-fringes-autoloads.el ends here
