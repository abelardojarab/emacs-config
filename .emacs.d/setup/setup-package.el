;;; setup-package.el ---                     -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015, 2016, 2017  Abelardo Jara-Berrocal

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

;; Disable package initialize after us.
(setq package-enable-at-startup nil)

;; Ask package.el to not add (package-initialize) to .emacs.
(setq package--init-file-ensured t)

;; use https for both melpa and gelpa
(eval-and-compile
  (setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("org"   . "http://orgmode.org/elpa/"))))

;; Add all sub-directories inside Cask dir
(defun my/add-subfolders-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (add-to-list 'load-path name)
        (my/add-subfolders-to-load-path name)))))

(setq my/vendor-dir (expand-file-name ".cask/25.3/elpa" user-emacs-directory))
(add-to-list 'load-path my/vendor-dir)
(my/add-subfolders-to-load-path my/vendor-dir)

;; Use Package
(eval-when-compile
  (require 'use-package))

;; Baseline packages
(use-package cl)
(use-package cl-lib)
(use-package bind-key)
(use-package diminish)
(use-package let-alist)
(use-package color)

;; Use Cask to fetch packages
(when (file-exists-p "~/.cask/cask.el")
    (require 'cask "~/.cask/cask.el")
    (cask-initialize)

    (use-package pallet
      :load-path (lambda () (expand-file-name "pallet/" user-emacs-directory))
      :config (pallet-mode t)))

;; Essential packages
(use-package async               :defer t :load-path (lambda () (expand-file-name "async/" user-emacs-directory)))
(use-package buttercup           :defer t :load-path (lambda () (expand-file-name "buttercup/" user-emacs-directory)))
(use-package ctable              :defer t :load-path (lambda () (expand-file-name "ctable/" user-emacs-directory)))
(use-package dash                :defer t :load-path (lambda () (expand-file-name "dash/" user-emacs-directory)))
(use-package deferred            :defer t :load-path (lambda () (expand-file-name "deferred/" user-emacs-directory)))
(use-package dropdown-list       :defer t :load-path (lambda () (expand-file-name "dropdown-list/" user-emacs-directory)))
(use-package ebib                :defer t :load-path (lambda () (expand-file-name "ebib/" user-emacs-directory)))
(use-package epc                 :defer t :load-path (lambda () (expand-file-name "epc/" user-emacs-directory)))
(use-package epl                 :defer t :load-path (lambda () (expand-file-name "epl/" user-emacs-directory)))
(use-package f                   :defer t :load-path (lambda () (expand-file-name "f/" user-emacs-directory)))
(use-package fringe-helper       :defer t :load-path (lambda () (expand-file-name "fringe-helper" user-emacs-directory)))
(use-package ht                  :defer t :load-path (lambda () (expand-file-name "ht/" user-emacs-directory)))
(use-package irony               :defer t :load-path (lambda () (expand-file-name "irony-mode/" user-emacs-directory)))
(use-package list-utils          :defer t :load-path (lambda () (expand-file-name "list-utils/" user-emacs-directory)))
(use-package log4e               :defer t :load-path (lambda () (expand-file-name "log4e/" user-emacs-directory)))
(use-package loop                :defer t :load-path (lambda () (expand-file-name "loop/" user-emacs-directory)))
(use-package makey               :defer t :load-path (lambda () (expand-file-name "makey/" user-emacs-directory)))
(use-package math-symbol-lists   :defer t :load-path (lambda () (expand-file-name "math-symbol-lists/" user-emacs-directory)))
(use-package memoize             :defer t :load-path (lambda () (expand-file-name "memoize/" user-emacs-directory)))
(use-package names               :defer t :load-path (lambda () (expand-file-name "names/" user-emacs-directory)))
(use-package parsebib            :defer t :load-path (lambda () (expand-file-name "parsebib/" user-emacs-directory)))
(use-package pkg-info            :defer t :load-path (lambda () (expand-file-name "pkg-info/" user-emacs-directory)))
(use-package popup               :defer t :load-path (lambda () (expand-file-name "popup/" user-emacs-directory)))
(use-package popwin              :defer t :load-path (lambda () (expand-file-name "popwin/" user-emacs-directory)))
(use-package pos-tip             :defer t :load-path (lambda () (expand-file-name "pos-tip/" user-emacs-directory)))
(use-package s                   :defer t :load-path (lambda () (expand-file-name "s/" user-emacs-directory)))
(use-package seq                 :defer t :load-path (lambda () (expand-file-name "seq/" user-emacs-directory)))
(use-package tabbar              :defer t :load-path (lambda () (expand-file-name "tabbar/" user-emacs-directory)))
(use-package xml-rpc             :defer t :load-path (lambda () (expand-file-name "xml-rpc/" user-emacs-directory)))
(use-package yaxception          :defer t :load-path (lambda () (expand-file-name "yaxception/" user-emacs-directory)))

(provide 'setup-package)
;;; setup-package.el ends here
