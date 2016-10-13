;;; setup-python-plugins.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Abelardo Jara

;; Author: Abelardo Jara <abelardojara@Abelardos-MacBook-Pro.local>
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
  :load-path (lambda () (expand-file-name "python-environment/" user-emacs-directory))
  :init (progn
          (setq python-environment-directory "~/.emacs.cache/python-environments")))

(use-package epc
  :defer t
  :load-path (lambda () (expand-file-name "epc/" user-emacs-directory)))

;; only use Jedi if python interpreter is present
(use-package jedi
  :if (executable-find "python")
  :commands (jedi:setup)
  :load-path (lambda () (expand-file-name "jedi/" user-emacs-directory))
  :config (progn
            (add-hook 'python-mode-hook 'jedi:setup)
            (setq jedi:setup-keys nil)
            (setq jedi:complete-on-dot t)
            (setq jedi:tooltip-method t)
            (if (featurep 'auto-complete)
              (ac-flyspell-workaround))))

;; Company backend for Python jedi
(use-package company-jedi
  :after (company jedi python-mode)
  :load-path (lambda () (expand-file-name "company-jedi/" user-emacs-directory))
  :config (progn
            (setq-default jedi:complete-on-dot t
                          jedi:get-in-function-call-delay 0.2)

            (setq-default company-backends '(company-jedi
                                             company-yasnippet
                                             company-semantic
                                             company-gtags
                                             company-dabbrev-code))))

(provide 'setup-python-plugins)
;;; setup-python-plugins.el ends here
