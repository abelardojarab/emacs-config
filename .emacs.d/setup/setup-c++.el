;;; setup-c++.el ---                                 -*- lexical-binding: t; -*-

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

(use-package cc-mode
  :load-path (lambda () (expand-file-name "cc-mode/" user-emacs-directory))
  :bind (:map c-mode-map
              ("C-c C-o" . ff-find-other-file)
              :map c++-mode-map
              ("C-c C-o" . ff-find-other-file))
  :config (progn
            ;; Put c++-mode as default for *.h files (improves parsing)
            (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

            ;; Make C/C++ indentation reliable
            ;; http://stackoverflow.com/questions/663588/emacs-c-mode-incorrect-indentation
            (defun my/c-indent-offset-according-to-syntax-context (key val)
              ;; remove the old element
              (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
              ;; new value
              (add-to-list 'c-offsets-alist '(key . val)))
            (add-hook 'c-mode-common-hook
                      (lambda ()
                        (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                          ;; indent
                          (my/c-indent-offset-according-to-syntax-context 'substatement-open 0))))

            ;; C/C++ style
            (defun my/c-mode-init ()
              (interactive)
              (c-set-style "Linux")
              (c-set-offset 'substatement-open 0)
              (c-toggle-electric-state -1)
              (setq comment-multi-line t)
              (setq-default c-default-style "bsd")
              (setq-default indent-tabs-mode t)

              (setq-default tab-width 8)
              (make-local-variable 'c-basic-offset)
              (setq c-basic-offset tab-width)
              (make-local-variable 'c-indent-level)
              (setq c-indent-level tab-width)

              ;; tab width
              (setq-default tab-stop-list '(8 16 24 32 40 48 56 64 72 80 88 96 108)))
            (add-hook 'c-mode-common-hook 'my/c-mode-init)

            ;; ensure fill-paragraph takes doxygen @ markers as start of new
            ;; paragraphs properly
            (setq paragraph-start "^[ ]*\\(//+\\|\\**\\)[ ]*\\([ ]*$\\|@param\\)\\|^\f")))

;; Insert and delete C++ header files automatically.
(use-package cpp-auto-include
  :defer t
  :commands cpp-auto-include
  :bind (:map c++-mode-map
              ("C-c C-a" . cpp-auto-include))
  :load-path (lambda () (expand-file-name "cpp-auto-include/" user-emacs-directory)))

;; Show inline arguments hint for the C/C++ function at point
(use-package function-args
  :defer t
  :diminish function-args-mode
  :commands (moo-complete moo-jump-local function-args-mode)
  :load-path (lambda () (expand-file-name "function-args/" user-emacs-directory))
  :bind (:map c-mode-map
              ("C-c C-m" . moo-complete)
              :map c++-mode-map
              ("C-c C-m" . moo-complete))
  :init (add-hook 'c-mode-common-hook 'function-args-mode)
  :config (fa-config-default))

;; C/C++ refactoring tool based on Semantic parser framework
(use-package srefactor
  :defer t
  :commands srefactor-refactor-at-point
  :load-path (lambda () (expand-file-name "semantic-refactor/" user-emacs-directory))
  :bind (:map c-mode-map
              ("C-c C-r" . srefactor-refactor-at-point)
              :map c++-mode-map
              ("C-c C-r" . srefactor-refactor-at-point)))

;; Devhelp support
(use-package devhelp
  :defer t
  :commands (devhelp-word-at-point devhelp-toggle-automatic-assistant))

;; Irony server
(use-package irony
  :defer t
  :commands (irony-mode irony-install-server)
  :if (executable-find "irony-server")
  :diminish irony-mode
  :load-path (lambda () (expand-file-name "irony-mode/" user-emacs-directory))
  :after (ggtags eldoc function-args company)
  :init (add-hook 'c-mode-common-hook 'irony-mode)
  :config (progn
            (if (file-exists-p "/usr/local/bin/irony-server")
                (setq irony-server-install-prefix "/usr/local/"))
            (if (file-exists-p "~/.emacs.cache/irony-server/bin/irony-server")
                (setq irony-server-install-prefix "~/.emacs.cache/irony-server/"))

            (push "-std=c++11" irony-additional-clang-options)

            (if (not (file-exists-p "~/.emacs.cache/irony-user-dir"))
                (make-directory "~/.emacs.cache/irony-user-dir") t)
            (setq irony-user-dir "~/.emacs.cache/irony-user-dir/")

            ;; Irony json projects
            (use-package irony-cdb-json)

            ;; Hooks
            (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

;; This implements eldoc support in irony-mode
(use-package irony-eldoc
  :demand t
  :after irony
  :if (executable-find "irony-server")
  :commands irony-eldoc
  :load-path (lambda () (expand-file-name "irony-eldoc/" user-emacs-directory))
  :config (add-hook 'irony-mode-hook #'irony-eldoc))

;; Automatically insert prototype functions from .h
(use-package member-functions
  :config (setq mf--source-file-extension "cpp"))

;; Basic C compile
(use-package basic-c-compile
  :commands (basic-c-compile-file basic-c-compile-run-c basic-c-compile-makefile)
  :load-path (lambda () (expand-file-name "basic-c-compile/" user-emacs-directory))
  :config (setq basic-c-compiler "g++"
        basic-c-compile-all-files nil
        basic-c-compile-compiler-flags "-Wall -Werror -std=c++11"
        basic-c-compile-outfile-extension nil
        basic-c-compile-make-clean "find . -type f -executable -delete"))

(provide 'setup-c++)
;;; setup-c++.el ends here
