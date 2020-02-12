;;; lab-themes-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lab-dark-theme" "lab-dark-theme.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from lab-dark-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lab-dark-theme" '("lab-dark")))

;;;***

;;;### (autoloads nil "lab-light-theme" "lab-light-theme.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from lab-light-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lab-light-theme" '("lab-light")))

;;;***

;;;### (autoloads nil "lab-themes" "lab-themes.el" (0 0 0 0))
;;; Generated autoloads from lab-themes.el

(add-to-list 'custom-theme-load-path (file-name-directory load-file-name))

(autoload 'lab-themes-load-style "lab-themes" "\
Load Lab theme variant STYLE.
Argument STYLE can be either 'light or 'dark.

\(fn STYLE)" t nil)

(autoload 'lab-themes-switch-style "lab-themes" "\
Toggle between the light and dark style of Lab theme." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lab-themes" '("lab-themes-current-style")))

;;;***

;;;### (autoloads nil nil ("lab-themes-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lab-themes-autoloads.el ends here
