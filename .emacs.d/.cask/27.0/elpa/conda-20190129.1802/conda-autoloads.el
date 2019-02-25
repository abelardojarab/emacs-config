;;; conda-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "conda" "conda.el" (0 0 0 0))
;;; Generated autoloads from conda.el

(autoload 'conda-env-deactivate "conda" "\
Deactivate the current conda env.

\(fn)" t nil)

(autoload 'conda-env-activate "conda" "\
Switch to environment NAME, prompting if called interactively.

\(fn &optional NAME)" t nil)

(autoload 'conda-env-list "conda" "\
List all available conda environments in a temp buffer.

\(fn)" t nil)

(autoload 'conda-with-env-shell-command "conda" "\
With environment NAME active, execute the shell string COMMAND.

\(fn NAME COMMAND)" nil nil)

(autoload 'conda-env-shell-init "conda" "\
Activate the current env in a newly opened shell PROCESS.

\(fn PROCESS)" nil nil)

(autoload 'conda-env-initialize-interactive-shells "conda" "\
Configure interactive shells for use with conda.el.

\(fn)" nil nil)

(autoload 'conda-env-initialize-eshell "conda" "\
Configure eshell for use with conda.el.

\(fn)" nil nil)

(autoload 'conda-env-activate-for-buffer "conda" "\
Activate the conda environment implied by the current buffer.

This can be set by a buffer-local or project-local variable (e.g. a
`.dir-locals.el` that defines `conda-project-env-name`), or inferred from an
`environment.yml` or similar at the project level.

\(fn)" t nil)

(defvar conda-env-autoactivate-mode nil "\
Non-nil if Conda-Env-Autoactivate mode is enabled.
See the `conda-env-autoactivate-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `conda-env-autoactivate-mode'.")

(custom-autoload 'conda-env-autoactivate-mode "conda" nil)

(autoload 'conda-env-autoactivate-mode "conda" "\
Toggle conda-env-autoactivate mode.

This mode automatically tries to activate a conda environment for the current
buffer.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "conda" '("conda-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; conda-autoloads.el ends here
