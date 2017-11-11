;;; gxref-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "gxref" "gxref.el" (23046 44976 471291 553000))
;;; Generated autoloads from gxref.el

(let ((loads (get 'gxref 'custom-loads))) (if (member '"gxref" loads) nil (put 'gxref 'custom-loads (cons '"gxref" loads))))

(defvar gxref-global-exe "global" "\
Path to GNU Global executable.")

(custom-autoload 'gxref-global-exe "gxref" t)

(defvar gxref-create-db-cmd "gtags" "\
Command to create the gtags database.")

(custom-autoload 'gxref-create-db-cmd "gxref" t)

(defvar gxref-update-db-on-save t "\
A flag indicating whether to update the GTAGS database when a file is saved.")

(custom-autoload 'gxref-update-db-on-save "gxref" t)

(defvar gxref-project-root-dir nil "\
The root directory of the project.
If not defined, 'global -p' will be used to find it.")

(custom-autoload 'gxref-project-root-dir "gxref" t)

(defvar gxref-gtags-conf nil "\
Explicit GTAGS/GLOBAL configuration file.")

(custom-autoload 'gxref-gtags-conf "gxref" t)

(defvar gxref-gtags-label nil "\
Explicit GTAGS/GLOBAL label.")

(custom-autoload 'gxref-gtags-label "gxref" t)

(defvar gxref-gtags-lib-path nil "\
Explicit GLOBAL libpath.")

(custom-autoload 'gxref-gtags-lib-path "gxref" t)

(autoload 'gxref-update-db "gxref" "\
Update GTAGS project database for current project.

\(fn)" t nil)

(autoload 'gxref-single-update-db "gxref" "\
Update GTAGS project database for the current file.

\(fn)" t nil)

(autoload 'gxref-create-db "gxref" "\
Create a GTAGS database in the directory specified as PROJECT-ROOT-DIR.

\(fn PROJECT-ROOT-DIR)" t nil)

(autoload 'gxref-set-project-dir "gxref" "\
Explicitly Set the directory of the current project to PROJECT-DIR.
The given project dir will be used for locating the GTAGS file,
until a different project is selected, or `gxref-clear-project-dir'
is called to clear the project.

This function is provided as a convenience, but there are other
ways to determine the current project, which could sometimes be
more comfortable.  One option is to not set the project at all,
in which case a search is performed upwards from the current
directory, until a GTAGS file is found.  Alternatively, you could
explicitly set the variable `gxref-project-root-dir'.  This has
the same effect as using this function, but can be by setting a
file-local or dir-local variable.

\(fn PROJECT-DIR)" t nil)

(autoload 'gxref-clear-project-dir "gxref" "\
Explicitly clear the project directory.
When no project directory is set, a project directory is
determined by searching upwards from the current directory, until
a GTAGS file is found.  See `gxref-set-project-dir' for more details.

\(fn)" t nil)

(autoload 'gxref-xref-backend "gxref" "\
Gxref backend for Xref.

\(fn)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; gxref-autoloads.el ends here
