;;; setup-python.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2020  Abelardo Jara-Berrocal

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

;; Built-in python package
(use-package python
  :demand t
  ;; :ensure-system-package ((python-language-server . "pip3 install --user python-language-server")
  ;;                         (epc                    . "pip3 install --user epc")
  ;;                         (jedi                   . "pip3 install --user jedi")
  ;;                         (pyflakes               . "pip3 install --user pyflakes")
  ;;                         (pylint                 . "pip3 install --user pylint")
  ;;                         (pycodestyle            . "pip3 install --user pycodestyle")
  ;;                         (virtualenv             . "pip3 install --user virtualenv")
  ;;                         (pep8                   . "pip3 install --user pep8")
  ;;                         (autopep8               . "pip3 install --user autopep8")
  ;;                         (elpy                   . "pip3 install --user elpy")
  ;;                         (rope                   . "pip3 install --user rope")
  ;;                         (yapf                   . "pip3 install --user yapf")
  ;;                         (jedi                   . "pip3 install --user jedi"))
  :custom ((python-shell-interpreter      "python3")
           (python-shell-interpreter-args "-m IPython --simple-prompt -i"))
  :config (progn
            ;; Update imenu
            (defun python-reset-imenu ()
              (interactive)
              (if (fboundp 'setq-mode-local)
                  (setq-mode-local python-mode
                                   imenu-create-index-function 'python-imenu-create-index))
              (setq imenu-create-index-function 'python-imenu-create-index))

            ;; Stop cedet semantic-python-get-system-include-path to start the python interpreter
            (defun python-shell-internal-send-string (string) "")

            ;; Use ipython3 as default interpreter
            (if (executable-find "ipython3")
                (setq-default python-shell-interpreter "ipython3"
                              python-shell-interpreter-args "--colors=Linux --profile=default"
                              python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
                              python-shell-prompt-regexp "In \\[[0-9]+\\]: "
                              python-shell-completion-setup-code
                              "from IPython.core.completerlib import module_completion"
                              python-shell-completion-module-string-code
                              "';'.join(module_completion('''%s'''))\n"
                              python-shell-completion-string-code
                              "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

            (if (file-exists-p "/opt/anaconda3/bin/conda")
                (setq python-shell-virtualenv-root "/opt/anaconda3")
              (setq python-shell-virtualenv-root nil))

            ;; Remove wisent, python becomes unusuable slow
            (remove-hook 'python-mode-hook 'wisent-python-default-setup)))

;; by default, the function 'python-mode is associated with
;; the package python.el. The following changes that to python-mode.el:
(use-package python-mode
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :commands python-mode
  :interpreter ("python3" . python-mode)
  :bind (:map python-mode-map
              ("TAB" . py-indent-line))
  :preface (progn
             (defun check-python-module (&optional module)
               (and (executable-find "python3")
                    (= 0 (call-process "python3"  nil nil nil "-c"
                                       (concat "import "
                                               (if module module "jedi"))))))

             (defun run-python-once ()
               (remove-hook 'python-mode-hook 'run-python-once)
               (run-python (python-shell-parse-command))))
  :custom ((py-shell-name                        "python3")
           (py-shell-switch-buffers-on-execute-p t)
           (py-switch-buffers-on-execute-p       t)
           (py-smart-indentation                 t)
           (py-split-windows-on-execute-function 'split-window-horizontally))
  :config (add-hook 'python-mode-hook
                    (function (lambda ()
                                (progn
                                  ;; Assure there is an inferior Python process
                                  (if (not (version< emacs-version "24.5"))
                                      (run-python-once))
                                  ;; disable annoying "Python-Help" buffer
                                  (eldoc-mode -1)
                                  ;; disable flymake, we will use flycheck
                                  (flymake-mode -1)
                                  (setq-default python-indent-offset 4
                                                py-indent-offset     4
                                                py-shell-name        "python3")
                                  (my/tabs-setup nil 4))))))

(provide 'setup-python)
;;; setup-python.el ends here
