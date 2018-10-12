;;; setup-python-plugins.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojarab@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; Jedi settings
(use-package python-environment
  :defer t
  :init (setq python-environment-directory (concat (file-name-as-directory
                                                    my/emacs-cache-dir)
                                                   "python-environments")))

(use-package epc
  :defer t)

;; only use Jedi if python interpreter is present
(use-package jedi
  :if (and (executable-find "python")
           (check-python-module "epc")
           (check-python-module "jedi"))
  :commands (jedi:setup)
  :hook (python-mode . jedi:setup)
  :custom ((jedi:setup-keys nil)
	   (jedi:complete-on-dot t)
	   (jedi:tooltip-method t)
	   (jedi:get-in-function-call-delay 0.2))
  :config (use-package company-jedi
            :config (add-hook 'python-mode-hook
                              (lambda () (set (make-local-variable 'company-backends)
                                              '((company-jedi
						 company-capf
						 company-files
						 :with company-abbrev
						 :with company-yasnippet)))))))

;;; NOTE conda is needed to set anaconda virtual environment python process.
;;; Elpy can set the anaconda virtual env, but not the process. conda uses
;;; environment.yml (I think to find the process).
(use-package conda
  :defer t
  :if (file-exists-p "/opt/anaconda3/bin/conda")
  :init (setq conda-anaconda-home "/opt/anaconda3")
  :hook (python-mode . conda-env-autoactivate-mode)
  :commands (conda-env-activate
             conda-env-autoactivate-mode)
  :config (progn
            ;; If you want interactive shell support, include:
            (conda-env-initialize-interactive-shells)
            ;; If you want eshell support, include:
            (conda-env-initialize-eshell)))

;; Anaconda
(use-package anaconda-mode
  :bind (:map anaconda-mode-map
              ("M-." . python-goto-sql-file-or-definition)
              ("M-," . anaconda-mode-find-assignments))
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  :config (defun python-goto-sql-file-or-definition (&optional arg)
            "Call anaconda find-definitions or with prefix ARG find sql file."
            (interactive "P")
            (back-button-push-mark-local-and-global)
            (if arg
                (projectile-find-sql-file)
              (anaconda-mode-find-definitions)
              (recenter))))

;; Jupyter notebook
(use-package ein
  :defer t
  :if (file-exists-p "/opt/anaconda3/bin/jupyter")
  :commands (ein:jupyter-server-start)
  :config (progn
            (setq ein:jupyter-default-notebook-directory "~/Workspace/jupyter")
            (setq ein:jupyter-default-server-command "/opt/anaconda3/bin/jupyter")
            (setq ein:jupyter-server-args (list "--no-browser"))))

;; Automatic formatting according to Python's PEP8
(use-package py-autopep8
  :defer t
  :commands py-autopep8-enable-on-save
  :hook (python-mode . py-autopep8-enable-on-save))

(provide 'setup-python-plugins)
;;; setup-python-plugins.el ends here
