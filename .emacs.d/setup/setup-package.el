;;; setup-package.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2016  abelardo.jara-berrocal

;; Author: abelardo.jara-berrocal <ajaraber@plxc20122.pdx.intel.com>
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
(require 'use-package)

;; Baseline packages
(use-package bind-key)
(use-package diminish)
(use-package let-alist)

;; Namespace implementation (baseline package)
(use-package ctable          :defer t :load-path "~/.emacs.d/ctable")
(use-package deferred        :defer t :load-path "~/.emacs.d/deferred")
(use-package epc             :defer t :load-path "~/.emacs.d/epc")
(use-package epl             :defer t :load-path "~/.emacs.d/epl")
(use-package f               :defer t :load-path "~/.emacs.d/f")
(use-package s               :defer t :load-path "~/.emacs.d/s")
(use-package seq             :defer t :load-path "~/.emacs.d/seq")
(use-package pkg-info        :defer t :load-path "~/.emacs.d/pkg-info")
(use-package popup           :defer t :load-path "~/.emacs.d/popup")
(use-package popwin          :defer t :load-path "~/.emacs.d/popwin")
(use-package pos-tip         :defer t :load-path "~/.emacs.d/pos-tip")
(use-package tabbar          :defer t :load-path "~/.emacs.d/tabbar")
(use-package names           :defer t :load-path "~/.emacs.d/names")
(use-package xml-rpc         :defer t :load-path "~/.emacs.d/xml-rpc")
(use-package dash            :defer t :load-path "~/.emacs.d/dash")
(use-package emacs-buttercup :defer t :load-path "~/.emacs.d/emacs-buttercup")

(provide 'setup-package)
;;; setup-package.el ends here
