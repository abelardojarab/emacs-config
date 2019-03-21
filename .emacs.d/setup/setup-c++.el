;;; setup-c++.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojarab@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(use-package google-c-style
  :commands google-set-c-style)

(use-package cc-mode
  :demand t
  :bind (:map c-mode-map
              ("C-c C-o" . ff-find-other-file)
              :map c++-mode-map
              ("C-c C-o" . ff-find-other-file))
  :hook ((c++-mode-common . my/c++-mode-indent-init)
         (find-file-hook . my/c-files-hook))
  :mode (("\\.h\\'"  . c++-mode)
         ("\\.c\\'"  . c-mode))
  :preface (progn

             (setq-default c-basic-offset 4)

             ;; Default C++ style
             (defun my/build-tab-stop-list (width)
               (let ((num-tab-stops (/ 80 width))
                     (counter 1)
                     (ls nil))
                 (while (<= counter num-tab-stops)
                   (setq ls (cons (* width counter) ls))
                   (setq counter (1+ counter)))
                 (set (make-local-variable 'tab-stop-list) (nreverse ls))))
             (defun my/c++-mode-indent-init ()
               (interactive)
               (google-set-c-style)
               (setq tab-width 4) ;; change this to taste, this is what K&R uses
               (make-local-variable 'c-basic-offset)
               (setq c-basic-offset tab-width)
               (make-local-variable 'c-indent-level)
               (setq c-indent-level tab-width)
               (setq indent-tabs-mode nil))

             ;; Default C-style
             (setq my/cc-style
                   '("cc-mode"
                     (c-offsets-alist . ((func-decl-cont . ++)
                                         (member-init-intro . +)
                                         (inher-intro . ++)
                                         (comment-intro . 0)
                                         (arglist-close . c-lineup-arglist)
                                         (topmost-intro . 0)
                                         (block-open . 0)
                                         (inline-open . 0)
                                         (substatement-open . 0)
                                         (label . /)
                                         (case-label . +)
                                         (statement-case-open . +)
                                         (statement-case-intro . +)
                                         (access-label . /)
                                         (innamespace . -)
                                         (label . 0)
                                         (case-label . +)
                                         (inextern-lang . 0)
                                         ))))

             ;; Make C/C++ indentation reliable
             (defun my/c-indent-offset-according-to-syntax-context (key val)
               ;; remove the old element
               (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
               ;; new value
               (add-to-list 'c-offsets-alist '(key . val)))

             ;; Default C-style
             (defun my/c-mode-indent-init ()
               (interactive)

               (c-set-style "Linux")
               (c-set-offset 'substatement-open 0)
               (c-set-offset 'innamespace 0)
               (c-set-offset 'inextern-lang 0)
               (c-toggle-electric-state -1)

               (setq-default c-default-style "Linux")
               (c-add-style "Linux" my/cc-style)
               (my/tabs-setup t 8)

               (make-local-variable 'c-basic-offset)
               (setq c-basic-offset tab-width)
               (make-local-variable 'c-indent-level)
               (setq c-indent-level tab-width)

               (my/c-indent-offset-according-to-syntax-context 'substatement-open 0)

               ;; ensure fill-paragraph takes doxygen @ markers as start of new
               ;; paragraphs properly
               (setq-default comment-multi-line t
                             paragraph-start "^[ ]*\\(//+\\|\\**\\)[ ]*\\([ ]*$\\|@param\\)\\|^\f"))

             (defun my/c-files-hook ()
               (when(string= (file-name-extension buffer-file-name) "c")
                 (my/c-mode-indent-init)
                 ))))

;; C/C++ refactoring tool based on Semantic parser framework
(use-package srefactor
  :defer t
  :commands srefactor-refactor-at-point
  :bind (:map c-mode-map
              ("C-c C-r" . srefactor-refactor-at-point)
              :map c++-mode-map
              ("C-c C-r" . srefactor-refactor-at-point)))

;; Show inline arguments hint for the C/C++ function at point
(use-package function-args
  :defer t
  :diminish function-args-mode
  :bind (:map c-mode-map
              ("C-j" . moo-jump-directory)
              :map c++-mode-map
              ("C-j" . moo-jump-directory))
  :commands (moo-complete moo-jump-local moo-jump-directory function-args-mode)
  :bind (:map c-mode-map
              ("C-c C-m" . moo-complete)
              :map c++-mode-map
              ("C-c C-m" . moo-complete))
  :hook (c-mode-common . function-args-mode)
  :custom (moo-select-method 'ivy)
  :config (fa-config-default))

;; Automatically insert prototype functions from .h
(use-package member-functions
  :defer t
  :commands expand-member-functions
  :custom (mf--source-file-extension "cpp"))

;; cquery language server
(use-package cquery
  :defer t
  :if (executable-find "cquery")
  :commands cquery-enable
  :preface (defun cquery-enable ()
             (interactive)
             (condition-case nil (lsp) (user-error nil)))
  :config (progn
            (setq cquery-extra-init-params '(:index (:comments 2) :cacheFormat "msgpack" :completion (:detailedLabel t)))

            ;; use consolidated cache dir so we don't pollute project trees
            (setq cquery-cache-dir-function #'cquery-cache-dir-consolidated)
            (setq cquery-cache-dir-consolidated-path (expand-file-name "cquery-cache.d" "~/.cache/"))))

;; Coverage reports
(use-package cov
  :defer t
  :diminish cov-mode
  :preface (defun my/cov-mode-setup ()
             "Setup cov-mode."
             (make-local-variable 'cov-coverage-file-paths))
  :hook ((c-mode-common . cov-mode)
         (cov-mode      . my/cov-mode-setup)))

;; Devhelp support
(use-package devhelp
  :defer t
  :commands (devhelp-word-at-point
             devhelp-toggle-automatic-assistant))

;; Basic C compile
(use-package basic-c-compile
  :defer t
  :commands (basic-c-compile-file
             basic-c-compile-run-c
             basic-c-compile-makefile)
  :custom ((basic-c-compiler                  "g++")
           (basic-c-compile-all-files         nil)
           (basic-c-compile-compiler-flags    "-Wall -Werror -std=c++11")
           (basic-c-compile-outfile-extension nil)
           (basic-c-compile-make-clean        "find . -type f -executable -delete")))

;; C/C++ dissassemble
(use-package rmsbolt)

;; C-code formatting using clang-format
(use-package clang-format
  :defer t
  :commands (clang-format
             clang-format-buffer
             clang-format-region
             clang-format-buffer-smart)
  :custom (clang-format-style "file")
  ;; :hook (c++-mode . clang-format-buffer-smart-on-save)
  :init (progn
          (defun clang-format-buffer-smart ()
            "Reformat buffer if .clang-format exists in the projectile root."
            (when (f-exists? (expand-file-name ".clang-format" (projectile-project-root)))
              (clang-format-buffer)))

          (defun clang-format-buffer-smart-on-save ()
            "Add auto-save hook for clang-format-buffer-smart."
            (add-hook 'before-save-hook 'clang-format-buffer-smart nil t))))

(provide 'setup-c++)
;;; setup-c++.el ends here
