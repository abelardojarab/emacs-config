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
  :load-path (lambda () (expand-file-name "python-environment/" user-emacs-directory)))

(use-package epc
  :defer t
  :load-path (lambda () (expand-file-name "epc/" user-emacs-directory)))

(use-package jedi
  :load-path (lambda () (expand-file-name "jedi/" user-emacs-directory))
  :config (progn
			(add-hook 'python-mode-hook 'jedi:setup)
			(setq jedi:setup-keys nil)
			(setq jedi:complete-on-dot t)
			(setq jedi:tooltip-method t)
			(ac-flyspell-workaround)))

(provide 'setup-python-plugins)
;;; setup-python-plugins.el ends here
