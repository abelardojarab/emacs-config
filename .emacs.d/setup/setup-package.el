;;; setup-package.el ---                     -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Abelardo Jara-Berrocal

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
        (add-to-list 'custom-theme-load-path name)
        (my/add-subfolders-to-load-path name)))))

(setq my/vendor-dir (expand-file-name ".cask/27.0/elpa" user-emacs-directory))
(add-to-list 'load-path my/vendor-dir)
(my/add-subfolders-to-load-path my/vendor-dir)

;; Use Package
(eval-when-compile
  (require 'use-package))
(setq use-package-expand-minimally   t
      use-package-compute-statistics t
      use-package-verbose            t)

;; Baseline packages
(use-package cl)
(use-package cl-lib)
(use-package bind-key)
(use-package diminish)
(use-package let-alist)
(use-package color)
(use-package ansi-color)

;; Use Cask to fetch packages
(when (file-exists-p "~/.cask/cask.el")
  (require 'cask "~/.cask/cask.el")
  (cask-initialize)

  (use-package pallet
    :commands pallet-mode
    :config (pallet-mode t)))

;; Essential packages
(use-package async               :defer t)
(use-package buttercup           :defer t)
(use-package ctable              :defer t)
(use-package dash                :defer t)
(use-package deferred            :defer t)
(use-package dropdown-list       :defer t)
(use-package ebib                :defer t)
(use-package epc                 :defer t)
(use-package epl                 :defer t)
(use-package f                   :defer t)
(use-package fringe-helper       :defer t)
(use-package ht                  :defer t)
(use-package list-utils          :defer t)
(use-package log4e               :defer t)
(use-package loop                :defer t)
(use-package makey               :defer t)
(use-package math-symbol-lists   :defer t)
(use-package memoize             :defer t)
(use-package names               :defer t)
(use-package parsebib            :defer t)
(use-package pkg-info            :defer t)
(use-package popup               :defer t)
(use-package popwin              :defer t)
(use-package pos-tip             :defer t)
(use-package s                   :defer t)
(use-package seq                 :defer t)
(use-package tabbar              :defer t)
(use-package xml-rpc             :defer t)
(use-package yaxception          :defer t)

;; Just in case
(use-package irony               :defer t :load-path (lambda () (expand-file-name "irony-mode/" user-emacs-directory)))

;; Paradox
(use-package paradox
  :defer t
  :bind (("C-x C-u" . paradox-upgrade-packages))
  :config (progn
            ;; The "paradox-token" file is supposed to contain this line:
            ;;     (setq paradox-github-token "<YOUR_TOKEN>")
            (load (locate-user-emacs-file "paradox-token") :noerror :nomessage)

            ;; preferences
            (setq paradox-execute-asynchronously t
                  paradox-lines-per-entry        1
                  paradox-automatically-star     t
                  paradox-github-token           t)
            (paradox-enable))
  :commands (paradox-enable
             paradox-upgrade-packages
             paradox-list-packages))

(use-package use-package-chords
  :config (key-chord-mode 1))

(provide 'setup-package)
;;; setup-package.el ends here
