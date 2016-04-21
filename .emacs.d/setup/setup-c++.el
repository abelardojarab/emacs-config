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
  :bind (:map c-mode-map
              ("C-c o" . ff-find-other-file)
              :map c++-mode-map
              ("C-c o" . ff-find-other-file))
  :config (progn
            ;; Put c++-mode as default for *.h files (improves parsing)
            (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

            ;; C/C++ style
            (defun my/c-mode-init ()
              (c-set-style "k&r")
              (c-toggle-electric-state -1)
              (setq comment-multi-line t)
              (setq-default c-default-style "k&r")
              (setq-default c-basic-offset 3))
            (add-hook 'c-mode-hook #'my/c-mode-init)
            (add-hook 'c++-mode-hook #'my/c-mode-init)))

;; Insert and delete C++ header files automatically.
(use-package cpp-auto-include
  :defer t
  :commands cpp-auto-include
  :bind (:map c++-mode-map
              ("C-c i" . cpp-auto-include))
  :load-path (lambda () (expand-file-name "cpp-auto-include/" user-emacs-directory)))

;; Show inline arguments hint for the C/C++ function at point
(use-package function-args
  :defer t
  :commands moo-complete
  :load-path (lambda () (expand-file-name "function-args/" user-emacs-directory))
  :bind (:map c-mode-map
              ("C-c c" . moo-complete)
              :map c++-mode-map
              ("C-c c" . moo-complete))
  :config (progn
            (fa-config-default)
            (define-key function-args-mode-map (kbd "M-o") nil)))

;; C/C++ refactoring tool based on Semantic parser framework
(use-package srefactor
  :defer t
  :commands srefactor-refactor-at-point
  :load-path (lambda () (expand-file-name "semantic-refactor/" user-emacs-directory))
  :bind (:map c-mode-map
              ("C-c s" . srefactor-refactor-at-point)
              :map c++-mode-map
              ("C-c s" . srefactor-refactor-at-point)))

;; Irony server
(use-package irony
  :defer t
  :commands (irony-mode irony-install-server)
  :if (executable-find "clang")
  :load-path (lambda () (expand-file-name "irony-mode/" user-emacs-directory))
  :init (progn
          (when (file-exists-p "~/.emacs.cache/irony-server/bin/irony-server")
            (add-hook 'c++-mode-hook 'irony-mode)
            (add-hook 'c-mode-hook 'irony-mode)))
  :config (progn
            (setq irony-server-install-prefix "~/.emacs.cache/irony-server/")
            (push "-std=c++11" irony-additional-clang-options)))

(provide 'setup-c++)
;;; setup-c++.el ends here
