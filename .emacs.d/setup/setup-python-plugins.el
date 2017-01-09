;;; setup-python-plugins.el ---                      -*- lexical-binding: t; -*-

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
  :if (and (executable-find "python")
           (check-python-module "epc")
           (check-python-module "jedi"))
  :commands (jedi:setup)
  :after auto-complete
  :load-path (lambda () (expand-file-name "jedi/" user-emacs-directory))
  :config (progn
            (setq jedi:setup-keys nil
                  jedi:complete-on-dot t
                  jedi:tooltip-method t
                  jedi:get-in-function-call-delay 0.2)

            (add-hook 'python-mode-hook 'jedi:setup)
            (if (featurep 'auto-complete)
                (ac-flyspell-workaround))))

;; Company backend for Python jedi
(use-package company-jedi
  :if (and (executable-find "python")
           (check-python-module "epc")
           (check-python-module "jedi"))
  :after company
  :load-path (lambda () (expand-file-name "company-jedi/" user-emacs-directory))
  :config (add-hook 'python-mode-hook
                    (lambda () (set (make-local-variable 'company-backends)
                               '((company-yasnippet
                                  company-jedi
                                  company-capf
                                  company-files
                                  company-abbrev))))))

(provide 'setup-python-plugins)
;;; setup-python-plugins.el ends here
