;;; setup-python-plugins.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2022  Abelardo Jara-Berrocal

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
  :disabled t)

;; Use Jedi only if python interpreter is present
(use-package jedi
  :disabled t
  :if (and (executable-find "python")
           (check-python-module "epc")
           (check-python-module "jedi"))
  :commands (jedi:setup)
  :hook (python-mode . jedi:setup)
  :custom ((jedi:setup-keys                 nil)
           (jedi:complete-on-dot            t)
           (jedi:tooltip-method             t)
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
  :demand t
  :after python
  :hook (python-mode . elpy-enable)
  :commands (elpy-enable)
  :custom ((elpy-rpc-python-command      "python3")
           (elpy-rpc-ignored-buffer-size 5000000))
  :config (progn
            (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))
            (defalias 'workon 'pyenv-workon)
            (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))))

;;; conda is needed to set anaconda virtual environment python process.
;;; elpy can set the anaconda virtual env, but not the process. conda uses
;;; environment.yml (I think to find the process).
(use-package conda
  :disabled t
  :if (file-exists-p "/opt/anaconda3/bin/conda")
  :custom ((conda-anaconda-home           "/opt/anaconda3")
           (cond-env-home-directory       "/opt/anaconda3"))
  :hook (python-mode . conda-env-autoactivate-mode)
  :commands (conda-env-autoactivate-mode)
  :config (progn
            ;; If you want interactive shell support, include:
            (conda-env-initialize-interactive-shells)
            ;; If you want eshell support, include:
            (conda-env-initialize-eshell)))

;; Anaconda
(use-package anaconda-mode
  :disabled t
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
  :disabled t)

;; Microsoft Python Language Server
;; Remember to install pip3 install python-language-server
;; Also install pycodestyle (<2.4.0), python-language-server will break otherwise
(use-package lsp-python-ms
  :commands (company-lsp-setup)
  :hook ((python-mode . lsp)
         (python-mode . company-lsp-setup))
  :init (defun company-lsp-setup ()
          "Add company-anaconda to company-backends buffer-locally."
          (add-to-list (make-local-variable 'company-backends)
                       '(company-lsp
                         :with company-yasnippet
                         :with company-capf)))
  :config (progn
            ;; Prefer flake8, faster than pylint
            (setq-default lsp-pyls-configuration-sources ["flake8"])
            (setq-default lsp-pyls-plugins-pylint-enabled nil)))

;; $ npm -g install pyright
(use-package lsp-pyright
  :if (executable-find "pyright")
  :init (defun lsp-pyright/python-mode-hook ()
          ;; lsp-pyright
          (require 'lsp-pyright)
          (when (fboundp 'flycheck-mode)
            ;; we will use flake8 or pyright
            (setq flycheck-disabled-checkers '(python-mypy))))
  :hook (python-mode . lsp-pyright/python-mode-hook))

;; Jupyter notebook
;; Usage
;; M-x shell
;; $ conda env list
;; $ source activate someenvironmentname
;; M-x ein:notebooklist-login (enter the token as the password)
;; M-x ein:notebooklist-open
(use-package ein
  :disabled t
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
  :commands py-autopep8-enable-on-save)

;; I’m using pytest for testing my Python code these days.
(use-package python-pytest
  :after python
  :bind (:map python-mode-map ("C-c C-p" . python-pytest-popup)))

;; Integration with Jupyter
(use-package code-cells
  :if (executable-find "jupytext")
  :load-path (lambda () (expand-file-name "code-cells/" user-emacs-directory))
  :config (progn
            (setq code-cells-convert-ipynb-style '(("jupytext" "--to" "ipynb" "--from" "markdown")
                                                   ("jupytext" "--to" "markdown" "--from" "ipynb")
                                                   markdown-mode))
            (defhydra notebook-hydra (:color red :hint nil)
              "
_j_/_k_: ↓/↑, _h_ome, _l_ast, _q_uit      \
Cell: _e_val, mark and e_x_ecute      \
Kernel: _r_estart, eval _a_bove, _z_: pop to
"
              ("h" beginning-of-buffer)
              ("l" (progn (end-of-buffer)
                          (code-cells-backward-cell)))
              ("j" code-cells-forward-cell)
              ("k" code-cells-backward-cell)
              ("z" jupyter-repl-pop-to-buffer :color blue)
              ("x" (progn (code-cells-mark-cell)
                          (call-interactively 'execute-extended-command)))
              ("C-SPC" code-cells-mark-cell)
              ("r" jupyter-repl-restart-kernel)
              ("a" (code-cells-do (pulse-momentary-highlight-region (point-min) start)
                                  (jupyter-eval-region (point-min) start)))
              ("e" (code-cells-do (pulse-momentary-highlight-region start end)
                                  (jupyter-eval-region start end)
                                  (code-cells-forward-cell)))
              ("M-w" (code-cells-do (kill-ring-save start end)))
              ("C-w" (code-cells-do (kill-region start end)))
              ("q" nil :exit t))))

;; Sphinx-styled documentation generation
(use-package sphinx-doc
  :hook (python-mode . sphinx-doc-mode))

;; Numpy-styled documentation generation
(use-package numpydoc
  :bind (:map python-mode-map
              ("C-c C-n" . numpydoc-generate)))

(provide 'setup-python-plugins)
;;; setup-python-plugins.el ends here
