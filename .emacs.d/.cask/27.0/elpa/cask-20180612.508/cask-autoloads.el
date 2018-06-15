;;; cask-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cask" "cask.el" (0 0 0 0))
;;; Generated autoloads from cask.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cask" '("cask-" "package-")))

;;;***

;;;### (autoloads nil "cask-bootstrap" "cask-bootstrap.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from cask-bootstrap.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cask-bootstrap" '("cask-bootstrap-")))

;;;***

;;;### (autoloads nil "cask-cli" "cask-cli.el" (0 0 0 0))
;;; Generated autoloads from cask-cli.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cask-cli" '("cask-cli")))

;;;***

;;;### (autoloads nil "package-legacy" "package-legacy.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from package-legacy.el

(defvar package-enable-at-startup t "\
Whether to activate installed packages when Emacs starts.
If non-nil, packages are activated after reading the init file
and before `after-init-hook'.  Activation is not done if
`user-init-file' is nil (e.g. Emacs was started with \"-q\").

Even if the value is nil, you can type \\[package-initialize] to
activate the package system at any time.")

(custom-autoload 'package-enable-at-startup "package-legacy" t)

(autoload 'package-install "package-legacy" "\
Install the package named NAME.
Interactively, prompt for the package name.
The package is found on one of the archives in `package-archives'.

\(fn NAME)" t nil)

(autoload 'package-install-from-buffer "package-legacy" "\
Install a package from the current buffer.
When called interactively, the current buffer is assumed to be a
single .el file that follows the packaging guidelines; see info
node `(elisp)Packaging'.

When called from Lisp, PKG-INFO is a vector describing the
information, of the type returned by `package-buffer-info'; and
TYPE is the package type (either `single' or `tar').

\(fn PKG-INFO TYPE)" t nil)

(autoload 'package-install-file "package-legacy" "\
Install a package from a file.
The file can either be a tar file or an Emacs Lisp file.

\(fn FILE)" t nil)

(autoload 'package-initialize "package-legacy" "\
Load Emacs Lisp packages, and activate them.
The variable `package-load-list' controls which packages to load.
If optional arg NO-ACTIVATE is non-nil, don't activate packages.

\(fn &optional NO-ACTIVATE)" t nil)

(autoload 'describe-package "package-legacy" "\
Display the full documentation of PACKAGE (a symbol).

\(fn PACKAGE)" t nil)

(autoload 'list-packages "package-legacy" "\
Display a list of packages.
Fetches the updated list of packages before displaying.
The list is displayed in a buffer named `*Packages*'.

\(fn)" t nil)

(defalias 'package-list-packages 'list-packages)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "package-legacy" '("package-" "define-package" "describe-package-1")))

;;;***

;;;### (autoloads nil nil ("cask-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cask-autoloads.el ends here
