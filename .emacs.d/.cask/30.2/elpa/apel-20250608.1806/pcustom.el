;;; pcustom.el -- load custom  -*- lexical-binding: t -*-

;; Copyright (C) 1999 Free Software Foundation, Inc.
;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;;	Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; Keywords: emulating, custom

;; This file is part of APEL (A Portable Emacs Library).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This compatibility library was written in 1999.  At that time it
;; determined whether "new custom" was available, and if it instead
;; found "old custom", then it loaded `tinycustom'.

;; I don't know in what Emacs release "new custom" was introduced,
;; but I am guessing it was either 21.1 (released in 2001) or 20.1
;; (released in 1997).

;; Approximately two decades have passes since then, so nowadays
;; this library require `custom' unconditionally.  It also provides
;; the feature `pcustom', which three packages (mhc, mu-cite, and
;; w3m) still depend on.

;;; Code:

(require 'custom)

(require 'product)
(product-provide (provide 'pcustom) (require 'apel-ver))

;;; pcustom.el ends here
