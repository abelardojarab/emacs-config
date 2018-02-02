;;; cmake-project-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "cmake-project" "cmake-project.el" (23156 53308
;;;;;;  386268 329000))
;;; Generated autoloads from cmake-project.el

(autoload 'cmake-project-configure-project "cmake-project" "\
Configure or reconfigure a CMake build tree.
BUILD-DIRECTORY is the path to the build-tree directory.  If the
directory does not already exist, it will be created.  The source
directory is found automatically based on the current
buffer. With a prefix argument additional CMake flags can be
specified interactively.

\(fn BUILD-DIRECTORY GENERATOR &optional FLAGS)" t nil)

(autoload 'cmake-project-mode "cmake-project" "\
Minor mode that integrates a CMake-based project with Emacs
build tools such as the CompileCommand and Flymake.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; cmake-project-autoloads.el ends here
