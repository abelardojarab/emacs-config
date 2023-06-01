;;; setup-package.el ---                     -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2023  Abelardo Jara-Berrocal

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

;; Remove security vulnerability
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

(setq
 initial-buffer-choice nil
 inhibit-startup-message t
 inhibit-startup-screen t
 inhibit-startup-buffer-menu t
 inhibit-x-resources t)

;; This is a weird one, see
;; https://emacshorrors.com/posts/advertising-your-freedom.html
(fset 'display-startup-echo-area-message 'ignore)

(defun make-obsolete (obsolete-name current-name &optional when)
  "Make the byte-compiler warn that function OBSOLETE-NAME is obsolete.
OBSOLETE-NAME should be a function name or macro name (a symbol).

The warning will say that CURRENT-NAME should be used instead.
If CURRENT-NAME is a string, that is the `use instead' message
\(it should end with a period, and not start with a capital).
WHEN should be a string indicating when the function
was first made obsolete, for example a date or a release number."
  (declare (advertised-calling-convention
            ;; New code should always provide the `when' argument.
            (obsolete-name current-name when) "23.1"))
  (put obsolete-name 'byte-obsolete-info
       ;; The second entry used to hold the `byte-compile' handler, but
       ;; is not used any more nowadays.
       (purecopy (list current-name nil when)))
  obsolete-name)

(defmacro define-obsolete-function-alias (obsolete-name current-name
                                                        &optional when docstring)
  "Set OBSOLETE-NAME's function definition to CURRENT-NAME and mark it obsolete.

\(define-obsolete-function-alias \\='old-fun \\='new-fun \"22.1\" \"old-fun's doc.\")

is equivalent to the following two lines of code:

\(defalias \\='old-fun \\='new-fun \"old-fun's doc.\")
\(make-obsolete \\='old-fun \\='new-fun \"22.1\")

WHEN should be a string indicating when the function was first
made obsolete, for example a date or a release number.

See the docstrings of `defalias' and `make-obsolete' for more details."
  (declare (doc-string 4)
           (advertised-calling-convention
            ;; New code should always provide the `when' argument.
            (obsolete-name current-name when &optional docstring) "23.1"))
  `(progn
     (defalias ,obsolete-name ,current-name ,docstring)
     (make-obsolete ,obsolete-name ,current-name ,when)))

(defun make-obsolete-variable (obsolete-name current-name &optional when access-type)
  "Make the byte-compiler warn that OBSOLETE-NAME is obsolete.
The warning will say that CURRENT-NAME should be used instead.
If CURRENT-NAME is a string, that is the `use instead' message.
WHEN should be a string indicating when the variable
was first made obsolete, for example a date or a release number.
ACCESS-TYPE if non-nil should specify the kind of access that will trigger
  obsolescence warnings; it can be either `get' or `set'."
  (declare (advertised-calling-convention
            ;; New code should always provide the `when' argument.
            (obsolete-name current-name when &optional access-type) "23.1"))
  (put obsolete-name 'byte-obsolete-variable
       (purecopy (list current-name access-type when)))
  obsolete-name)

(defmacro define-obsolete-variable-alias (obsolete-name current-name
                                                        &optional when docstring)
  "Make OBSOLETE-NAME a variable alias for CURRENT-NAME and mark it obsolete.
This uses `defvaralias' and `make-obsolete-variable' (which see).
See the Info node `(elisp)Variable Aliases' for more details.

If CURRENT-NAME is a defcustom or a defvar (more generally, any variable
where OBSOLETE-NAME may be set, e.g. in an init file, before the
alias is defined), then the define-obsolete-variable-alias
statement should be evaluated before the defcustom, if user
customizations are to be respected.  The simplest way to achieve
this is to place the alias statement before the defcustom (this
is not necessary for aliases that are autoloaded, or in files
dumped with Emacs).  This is so that any user customizations are
applied before the defcustom tries to initialize the
variable (this is due to the way `defvaralias' works).

WHEN should be a string indicating when the variable was first
made obsolete, for example a date or a release number.

For the benefit of Customize, if OBSOLETE-NAME has
any of the following properties, they are copied to
CURRENT-NAME, if it does not already have them:
`saved-value', `saved-variable-comment'."
  (declare (doc-string 4)
           (advertised-calling-convention
            ;; New code should always provide the `when' argument.
            (obsolete-name current-name when &optional docstring) "23.1"))
  `(progn
     (defvaralias ,obsolete-name ,current-name ,docstring)
     ;; See Bug#4706.
     (dolist (prop '(saved-value saved-variable-comment))
       (and (get ,obsolete-name prop)
            (null (get ,current-name prop))
            (put ,current-name prop (get ,obsolete-name prop))))
     (make-obsolete-variable ,obsolete-name ,current-name ,when)))

(defvar debian-aspell-only-dictionary-alist nil)
(defvar ivy-completing-read-handlers-alist nil)

(defvar org-directory "~/workspace/Documents/Org")
(if (not (file-exists-p org-directory))
    (make-directory org-directory t))

(setq package-user-dir "~/.emacs.d/site-lisp/package-install")
(require 'package)

;; Disable checking signature from elpa
(setq package-check-signature nil)

;; in ~/.emacs.d/init.el (or ~/.emacs.d/early-init.el in Emacs 27)
(setq package-enable-at-startup nil ; don't auto-initialize!
      ;; don't add that `custom-set-variables' block to my init.el!
      package--init-file-ensured t)

;; use https for both melpa and gelpa
(eval-and-compile
  (setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("org"   . "http://orgmode.org/elpa/"))))

;; Since I'm managing packages with `use-package' I don't ever want to
;; save `package-selected-packages'.
(defun pd-package--save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE."
  (when value
    (setq package-selected-packages value)))
(fset 'package--save-selected-packages 'pd-package--save-selected-packages)

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

(setq my/vendor-dir (expand-file-name ".cask/27.2/elpa" user-emacs-directory))
(add-to-list 'load-path my/vendor-dir)
(my/add-subfolders-to-load-path my/vendor-dir)

(defun my/byte-recompile-elpa ()
  "Force byte-compile every `.el' file in `my/vendor-dir'.
The `.el' files are re-compiled even if the corresponding `.elc' files exist,
in all the sub-directories under `my/vendor-dir'.
If the `.elc' file does not exist, this function *does not* compile the
corresponding `.el' file."
  (interactive)
  (byte-recompile-directory my/vendor-dir nil :force))

;; Use Package
(eval-when-compile
  (require 'use-package))

(use-package use-package
  :demand t
  :custom ((use-package-expand-minimally     t)
           (use-package-compute-statistics   t)
           (use-package-verbose              t)
           (use-package-enable-imenu-support t)))

;; Use-package plugins
(use-package use-package-hydra
  :demand t)
;; (use-package use-package-el-get
;;   :demand t)

;; Baseline packages
(use-package cl)
(use-package cl-lib)
(use-package bind-key)
(use-package diminish :demand t)
(use-package delight :demand t)
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

;; Asynchronous bytecode compilation
(use-package async
  :defer t
  :init (use-package async-bytecomp
          :commands async-bytecomp-package-mode
          :config (async-bytecomp-package-mode 1))
  :custom (async-bytecomp-allowed-packages '(all))
  :config (progn
            ;; For native-comp branch
            (when (fboundp 'native-compile-async)
              (unless (fboundp 'subr-native-lambda-list)
                (defun subr-native-lambda-list (x)
                  nil))
              (setq comp-async-jobs-number 4 ;; not using all cores
                    comp-deferred-compilation t
                    comp-deferred-compilation-black-list '()))

            ;; Bug workaround, 2023-02-26:
            (if (boundp 'native-comp-enable-subr-trampolines)
                (if (not (boundp 'comp-enable-subr-trampolines))
                    (setq comp-enable-subr-trampolines native-comp-enable-subr-trampolines)))))

;; Needed
(require 'dash-functional)

;; Essential packages
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
(use-package popup               :defer t)
(use-package popwin              :defer t)
(use-package pos-tip             :defer t)
(use-package s                   :defer t)
(use-package seq                 :defer t)
(use-package xml-rpc             :defer t)
(use-package yaxception          :defer t)

;; Tabbar mode
(use-package tabbar
  :defer t
  :commands tabbar-mode)

;; Just in case
(use-package irony
  :defer t
  :load-path (lambda () (expand-file-name "irony-mode/" user-emacs-directory)))

;; Package information
(use-package pkg-info
  :demand t)

;; Paradox
(use-package paradox
  :defer t
  :bind (("C-x C-u" . paradox-upgrade-packages))
  :custom ((paradox-execute-asynchronously t)
           (paradox-lines-per-entry        1)
           (paradox-automatically-star     t)
           (paradox-github-token           t))
  :config (progn
            ;; The "paradox-token" file is supposed to contain this line:
            ;; (setq paradox-github-token "<YOUR_TOKEN>")
            (load (locate-user-emacs-file "paradox-token") :noerror :nomessage)
            (paradox-enable))
  :commands (paradox-enable
             paradox-upgrade-packages
             paradox-list-packages))

;; Enable chord keys for packages
(use-package use-package-chords
  :defer t
  :commands key-chord-mode
  :config (key-chord-mode 1))

;; Get ensure package support
(use-package use-package-ensure-system-package
  :demand t)

;; Auto-compile .el files on load
(use-package auto-compile
  :demand t
  :custom ((auto-compile-display-buffer               nil)
           (auto-compile-mode-line-counter            t)
           (auto-compile-source-recreate-deletes-dest t)
           (auto-compile-toggle-deletes-nonlib-dest   t)
           (auto-compile-update-autoloads             t))
  :hook (auto-compile-inhibit-compile . auto-compile-inhibit-compile-detached-git-head)
  :config (progn
            (auto-compile-on-load-mode)
            (auto-compile-on-save-mode)))

(provide 'setup-package)
;;; setup-package.el ends here
