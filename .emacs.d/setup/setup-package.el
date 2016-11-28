;;; setup-package.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015, 2016  abelardo.jara-berrocal

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

(setq package-user-dir "~/.emacs.d/site-lisp/package-install")
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

;; Use Package
(eval-when-compile
  (require 'use-package))

;; Baseline packages
(use-package bind-key)
(use-package diminish)
(use-package let-alist)

;; Namespace implementation (baseline package)
(use-package ctable          :defer t :load-path (lambda () (expand-file-name "ctable/" user-emacs-directory)))
(use-package deferred        :defer t :load-path (lambda () (expand-file-name "deferred/" user-emacs-directory)))
(use-package epc             :defer t :load-path (lambda () (expand-file-name "epc/" user-emacs-directory)))
(use-package epl             :defer t :load-path (lambda () (expand-file-name "epl/" user-emacs-directory)))
(use-package f               :defer t :load-path (lambda () (expand-file-name "f/" user-emacs-directory)))
(use-package s               :defer t :load-path (lambda () (expand-file-name "s/" user-emacs-directory)))
(use-package seq             :load-path (lambda () (expand-file-name "seq/" user-emacs-directory)))
(use-package pkg-info        :defer t :load-path (lambda () (expand-file-name "pkg-info/" user-emacs-directory)))
(use-package popup           :defer t :load-path (lambda () (expand-file-name "popup/" user-emacs-directory)))
(use-package popwin          :defer t :load-path (lambda () (expand-file-name "popwin/" user-emacs-directory)))
(use-package pos-tip         :defer t :load-path (lambda () (expand-file-name "pos-tip/" user-emacs-directory)))
(use-package tabbar          :defer t :load-path (lambda () (expand-file-name "tabbar/" user-emacs-directory)))
(use-package names           :defer t :load-path (lambda () (expand-file-name "names/" user-emacs-directory)))
(use-package xml-rpc         :defer t :load-path (lambda () (expand-file-name "xml-rpc/" user-emacs-directory)))
(use-package dash            :defer t :load-path (lambda () (expand-file-name "dash/" user-emacs-directory)))
(use-package buttercup       :defer t :load-path (lambda () (expand-file-name "buttercup/" user-emacs-directory)))
(use-package fringe-helper   :defer t :load-path (lambda () (expand-file-name "fringe-helper" user-emacs-directory)))
(use-package parsebib        :defer t :load-path (lambda () (expand-file-name "parsebib/" user-emacs-directory)))
(use-package ebib            :defer t :load-path (lambda () (expand-file-name "ebib/" user-emacs-directory)))
(use-package ht              :defer t :load-path (lambda () (expand-file-name "ht/" user-emacs-directory)))
(use-package log4e           :defer t :load-path (lambda () (expand-file-name "log4e/" user-emacs-directory)))
(use-package yaxception      :defer t :load-path (lambda () (expand-file-name "yaxception/" user-emacs-directory)))
(use-package dropdown-list   :defer t :load-path (lambda () (expand-file-name "dropdown-list/" user-emacs-directory)))

;; With-editor (emacsclient support)
(use-package with-editor
  :load-path (lambda () (expand-file-name "with-editor/" user-emacs-directory))
  :init (progn
            (add-hook 'shell-mode-hook  'with-editor-export-editor)
            (add-hook 'eshell-mode-hook 'with-editor-export-editor)))

(provide 'setup-package)
;;; setup-package.el ends here
