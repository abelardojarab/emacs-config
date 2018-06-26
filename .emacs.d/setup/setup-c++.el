;;; setup-c++.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Abelardo Jara-Berrocal

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

(use-package cc-mode
  :demand t
  :load-path (lambda () (expand-file-name "cc-mode/" user-emacs-directory))
  :bind (:map c-mode-map
              ("C-c C-o" . ff-find-other-file)
              :map c++-mode-map
              ("C-c C-o" . ff-find-other-file))
  :init (add-hook 'c-mode-common-hook #'my/c-mode-init)
  :commands my/c-mode-init
  :config (progn
            ;; Put c++-mode as default for *.h files (improves parsing)
            (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

            ;; C/C++ style
            (defun my/c-mode-init ()
              (interactive)
              (c-set-style "Linux")
              (c-set-offset 'substatement-open 0)
              (c-set-offset 'innamespace 0)
              (c-toggle-electric-state -1)

              (setq-default c-default-style "Linux")
              (my/tabs-setup t 8)

              (make-local-variable 'c-basic-offset)
              (setq c-basic-offset tab-width)
              (make-local-variable 'c-indent-level)
              (setq c-indent-level tab-width)

              ;; ensure fill-paragraph takes doxygen @ markers as start of new
              ;; paragraphs properly
              (setq-default comment-multi-line t
                            paragraph-start "^[ ]*\\(//+\\|\\**\\)[ ]*\\([ ]*$\\|@param\\)\\|^\f"))

            ;; Make C/C++ indentation reliable
            (defun my/c-indent-offset-according-to-syntax-context (key val)
              ;; remove the old element
              (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
              ;; new value
              (add-to-list 'c-offsets-alist '(key . val)))
            (add-hook 'c-mode-common-hook
                      (lambda ()
                        (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                          ;; indent
                          (my/c-indent-offset-according-to-syntax-context 'substatement-open 0))))))

;; Show inline arguments hint for the C/C++ function at point
(use-package function-args
  :defer t
  :diminish function-args-mode
  :bind (:map c-mode-map
              ("C-j" . moo-jump-directory)
              :map c++-mode-map
              ("C-j" . moo-jump-directory))
  :commands (moo-complete moo-jump-local moo-jump-directory function-args-mode)
  :load-path (lambda () (expand-file-name "function-args/" user-emacs-directory))
  :bind (:map c-mode-map
              ("C-c C-m" . moo-complete)
              :map c++-mode-map
              ("C-c C-m" . moo-complete))
  :init (add-hook 'c-mode-common-hook #'function-args-mode)
  :config (progn
            (fa-config-default)
            (setq moo-select-method 'ivy)))

;; C/C++ refactoring tool based on Semantic parser framework
(use-package srefactor
  :defer t
  :commands srefactor-refactor-at-point
  :load-path (lambda () (expand-file-name "srefactor/" user-emacs-directory))
  :bind (:map c-mode-map
              ("C-c C-r" . srefactor-refactor-at-point)
              :map c++-mode-map
              ("C-c C-r" . srefactor-refactor-at-point)))

;; Devhelp support
(use-package devhelp
  :defer t
  :commands (devhelp-word-at-point
             devhelp-toggle-automatic-assistant))

;; Automatically insert prototype functions from .h
(use-package member-functions
  :config (setq mf--source-file-extension "cpp"))

;; Basic C compile
(use-package basic-c-compile
  :commands (basic-c-compile-file
             basic-c-compile-run-c
             basic-c-compile-makefile)
  :load-path (lambda () (expand-file-name "basic-c-compile/" user-emacs-directory))
  :config (setq basic-c-compiler "g++"
                basic-c-compile-all-files nil
                basic-c-compile-compiler-flags "-Wall -Werror -std=c++11"
                basic-c-compile-outfile-extension nil
                basic-c-compile-make-clean "find . -type f -executable -delete"))

(provide 'setup-c++)
;;; setup-c++.el ends here
