;;; setup-python-plugins.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019  Abelardo Jara-Berrocal

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
  :demand t
  :custom (python-environment-directory (concat (file-name-as-directory
                                                 my/emacs-cache-dir)
                                                "python-environments")))

;; Inter-process communication
(use-package epc
  :demand t)

;; only use Jedi if python interpreter is present
(use-package jedi
  :disabled t
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
            :hook (python-mode . company-jedi-setup)
            :init (defun company-jedi-setup ()
                    "Add company-anaconda to company-backends buffer-locally."
                    (add-to-list (make-local-variable 'company-backends)
                                 '(company-jedi
                                   :with company-yasnippet
                                   :with company-capf)))))

;; Disable flymake when flycheck is present
(use-package elpy
  :after python
  :demand t
  :hook (python-mode . elpy-enable)
  :commands (elpy-enable)
  :config (progn
            (defalias 'workon 'pyenv-workon)
            (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))))

;;; conda is needed to set anaconda virtual environment python process.
;;; Elpy can set the anaconda virtual env, but not the process. conda uses
;;; environment.yml (I think to find the process).
(use-package conda
  :defer t
  :if (file-exists-p "/opt/anaconda3/bin/conda")
  :custom ((conda-anaconda-home           "/opt/anaconda3")
           (cond-env-home-directory       "/opt/anaconda3")
           (python-shell-interpreter      "/opt/anaconda3/bin/python3")
           (python-shell-interpreter-args "-m IPython --simple-prompt -i"))
  :hook (python-mode . conda-env-autoactivate-mode)
  :commands (conda-env-autoactivate-mode)
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

;;;  company-anaconda
(use-package company-anaconda
  :disabled t
  :commands (company-anaconda-setup)
  :hook (python-mode . company-anaconda-setup)
  :init (defun company-anaconda-setup ()
          "Add company-anaconda to company-backends buffer-locally."
          (add-to-list (make-local-variable 'company-backends)
                       '(company-anaconda
                         :with company-lsp
                         :with company-yasnippet
                         :with company-capf))))

;; Microsoft Python Language Server
;; Remember to install pip3 install python-language-server
;; Also install pycodestyle (<2.4.0), python-language-server will break otherwise
(use-package company-lsp
  :custom (company-lsp-async t))

(use-package lsp-python-ms
  :commands (company-lsp-setup)
  :hook ((python-mode . lsp)
         (python-mode . company-lsp-setup))
  :init (defun company-lsp-setup ()
          "Add company-anaconda to company-backends buffer-locally."
          (add-to-list (make-local-variable 'company-backends)
                       '(company-lsp
                         :with company-yasnippet
                         :with company-capf))))

;; Jupyter notebook
;; Usage
;; M-x shell
;; $ conda env list
;; $ source activate someenvironmentname
;; M-x ein:notebooklist-login (enter the token as the password)
;; M-x ein:notebooklist-open
(use-package ein
  :defer t
  :if (file-exists-p "/opt/anaconda3/bin/jupyter")
  :commands (;; Start the jupyter notebook server at the given path.
             ;; This only works if jupyter is in the default conda env.
             ein:jupyter-server-start
             ;; Log in and open a notebooklist buffer for a running jupyter notebook server.
             ein:jupyter-server-login-and-open
             ;; Log on to a jupyterhub server using PAM authentication.
             ;; Requires jupyterhub version 0.8 or greater.
             ein:jupyterhub-connect
             ;; Login to IPython notebook server.
             ;; Use the server token as a password.
             ein:notebooklist-login
             ;; Open notebook list buffer.
             ein:notebooklist-open)
  :config (progn
            (use-package ein-loaddefs)
            (use-package ein-notebook
              :custom (ein:notebook-autosave-frequency 10))
            (use-package ein-subpackages)
            (use-package ein-jupyter
              :custom ((ein:jupyter-server-buffer-name "*ein:jupyter-server*")
                       (ein:jupyter-default-notebook-directory "~/Workspace/jupyter")
                       (ein:jupyter-default-server-command "/opt/anaconda3/bin/jupyter")
                       (ein:jupyter-server-args (list "--no-browser"))))))

;; Automatic formatting according to Python's PEP8
(use-package py-autopep8
  :defer t
  :commands py-autopep8-enable-on-save
  :hook (python-mode . py-autopep8-enable-on-save))

(provide 'setup-python-plugins)
;;; setup-python-plugins.el ends here
