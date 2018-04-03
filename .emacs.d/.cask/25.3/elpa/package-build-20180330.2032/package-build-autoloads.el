;;; package-build-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "package-build" "package-build.el" (23235 51770
;;;;;;  815942 524000))
;;; Generated autoloads from package-build.el

(autoload 'package-build-archive "package-build" "\
Build a package archive for the package named NAME.

\(fn NAME)" t nil)

(autoload 'package-build-package "package-build" "\
Create version VERSION of the package specified by RCP.
Return the archive entry for the package and store the package
in `package-build-archive-dir'.

\(fn RCP VERSION)" nil nil)

(autoload 'package-build-all "package-build" "\
Build all packages in the `package-build-recipe-alist'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "package-recipe-mode" "package-recipe-mode.el"
;;;;;;  (23235 51770 815942 524000))
;;; Generated autoloads from package-recipe-mode.el

(autoload 'package-build-create-recipe "package-recipe-mode" "\
Create a new recipe for the package named NAME using FETCHER.

\(fn NAME FETCHER)" t nil)

(autoload 'package-build-current-recipe "package-recipe-mode" "\
Build archive for the recipe defined in the current buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("package-build-badges.el" "package-build-pkg.el"
;;;;;;  "package-recipe.el") (23235 51770 815942 524000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; package-build-autoloads.el ends here
