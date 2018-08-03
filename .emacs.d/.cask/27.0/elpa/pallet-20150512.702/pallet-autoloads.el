;;; pallet-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pallet" "pallet.el" (0 0 0 0))
;;; Generated autoloads from pallet.el

(defvar pallet-mode nil "\
Non-nil if Pallet mode is enabled.
See the `pallet-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pallet-mode'.")

(custom-autoload 'pallet-mode "pallet" nil)

(autoload 'pallet-mode "pallet" "\
Maintain entries in your Cask file automatically.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pallet" '("pallet-")))

;;;***

;;;### (autoloads nil nil ("pallet-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pallet-autoloads.el ends here
