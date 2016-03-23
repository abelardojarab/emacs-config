;;; setup-c++.el ---                                 -*- lexical-binding: t; -*-

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

(use-package cc-mode
  :config (progn
            ;; Put c++-mode as default for *.h files (improves parsing)
            (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

            ;; C/C++ style
            (defun my/c-mode-init ()
              (c-set-style "k&r")
              (c-toggle-electric-state -1)
              (setq-default c-basic-offset 4))
            (add-hook 'c-mode-hook #'my/c-mode-init)
            (add-hook 'c++-mode-hook #'my/c-mode-init)))

(use-package function-args
  :load-path (lambda () (expand-file-name "function-args/" user-emacs-directory))
  :config (progn
            (fa-config-default)
            (define-key function-args-mode-map (kbd "M-o") nil)
            (define-key c-mode-map (kbd "C-:") 'moo-complete)
            (define-key c++-mode-map (kbd "C-:") 'moo-complete)))

(use-package srefactor
  :load-path (lambda () (expand-file-name "semantic-refactor/" user-emacs-directory))
  :config (progn
            (define-key c-mode-map (kbd "C-|") 'srefactor-refactor-at-point)
            (define-key c++-mode-map (kbd "C-|") 'srefactor-refactor-at-point)))

(provide 'setup-c++)
;;; setup-c++.el ends here
