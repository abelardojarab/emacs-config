;;; modern-fringes-autoloads.el --- automatically extracted autoloads
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

If called interactively, enable Modern-Fringes mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it
if ARG is `toggle'; disable the mode otherwise.

\\{modern-fringes-mode-map}

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "modern-fringes" '("modern-fringes--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; modern-fringes-autoloads.el ends here
