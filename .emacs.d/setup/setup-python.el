;;; setup-python.el ---

;; Copyright (C) 2014, 2015, 2016  abelardo.jara-berrocal

;; Author: abelardo.jara-berrocal <ajaraber@plxc25288.pdx.intel.com>
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
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'load-path (expand-file-name "python-mode/" user-emacs-directory))
(autoload 'python-mode "python-mode" "Python Mode." t)

;; Python configuration
(add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook 'indent-hint-mode)

;; Extra
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

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
                                     "/workspace/pythonlibs/lib/python2.7/site-packages") ":"
                             (getenv "PYTHONPATH")))

;; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)

;; try to automagically figure out indentation
(setq py-smart-indentation t)

;; split horizontally on execution
(setq py-split-windows-on-execute-function 'split-window-horizontally)

;; in cedet semantic-python-get-system-include-path forces starting of the python interpreter
(defun python-shell-internal-send-string (string) "")

;; Add Wisent
(add-hook 'python-mode-hook 'wisent-python-default-setup)

(provide 'setup-python)
;;; setup-python.el ends here
