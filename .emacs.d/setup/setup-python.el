;;; setup-python.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2017  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojara@Abelardos-MacBook-Pro.local>
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

;; by default, the function 'python-mode is associated with
;; the package python.el. The following changes that to python-mode.el:

(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :delight python-mode "Python"
  :commands python-mode
  :interpreter ("python" . python-mode)
  :load-path (lambda () (expand-file-name "python-mode/" user-emacs-directory))
  :init (progn
          ;; Check for python module
          (defun check-python-module (&optional module)
            (and (executable-find "python")
                 (= 0 (call-process "python"  nil nil nil "-c"
                                    (concat "import "
                                            (if module module "jedi")))))))
  :config (progn

            (add-hook 'python-mode-hook
                      (define-key python-mode-map (kbd "TAB") 'py-indent-line))

            ;; Python configuration
            (add-hook 'python-mode-hook 'autopair-mode)

            ;; When using auto-complete
            (if (featurep 'auto-complete)
                (add-hook 'python-mode-hook 'auto-complete-mode))

            ;; Python hook
            (add-hook 'python-mode-hook
                      (function (lambda ()
                                  (progn
                                    (set-variable 'python-indent-offset 4)
                                    (set-variable 'py-indent-offset 4)
                                    (set-variable 'indent-tabs-mode nil))
                                  (setq indent-tabs-mode nil
                                        python-indent-offset 4
                                        py-indent-offset 4
                                        tab-width 4))))

            ;; Update imenu
            (defun python-reset-imenu ()
              (interactive)
              (if (fboundp 'setq-mode-local)
                  (setq-mode-local python-mode
                                   imenu-create-index-function 'python-imenu-create-index))
              (setq imenu-create-index-function 'python-imenu-create-index))

            ;; Setup Python path
            (setenv "PYTHONPATH" (concat (concat (getenv "HOME")
                                                 "/workspace/pythonlibs/lib/python2.7/site-packages")
                                         ":" (getenv "PYTHONPATH")))

            ;; Python settings
            (setq py-shell-switch-buffers-on-execute-p t
                  py-switch-buffers-on-execute-p t
                  py-smart-indentation t
                  py-split-windows-on-execute-function 'split-window-horizontally)

            ;; Stop cedet semantic-python-get-system-include-path to start the python interpreter
            (defun python-shell-internal-send-string (string) "")

            ;; Use ipython as default interpreter
            (if (executable-find "ipython")
                (setq-default
                 python-shell-interpreter "ipython"
                 python-shell-interpreter-args "--colors=Linux --profile=default"
                 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
                 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
                 python-shell-completion-setup-code
                 "from IPython.core.completerlib import module_completion"
                 python-shell-completion-module-string-code
                 "';'.join(module_completion('''%s'''))\n"
                 python-shell-completion-string-code
                 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

            ;; Remove wisent, python becomes unusuable slow
            (remove-hook 'python-mode-hook 'wisent-python-default-setup)))

(provide 'setup-python)
;;; setup-python.el ends here
