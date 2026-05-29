;;; ecb-face.el --- all face-options of ECB

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;;         Kevin A. Burton <burton@openprivacy.org>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools
;; Created: 2001

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; $Id: ecb-face.el,v 1.26 2009/06/04 08:38:15 berndl Exp $

;;; Commentary:

;; This file contains all options with type 'face and all face-definitions of
;; ECB.

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.

;;; Code

(eval-when-compile
  (require 'silentcomp))

(silentcomp-defun set-face-parent)
(silentcomp-defun make-face-bold)
(silentcomp-defun make-face)
(silentcomp-defun set-face-foreground)

(defgroup ecb-face-options nil
  "Settings for all faces used in ECB."
  :group 'ecb
  :prefix "ecb-")

(defgroup ecb-faces nil
  "Definitions of all ECB-faces"
  :group 'ecb-face-options 
  :group 'faces
  :prefix "ecb-")

(defmacro ecb-face-default (&optional height bold-p italic-p
                                      inherit
                                      fg-light-col fg-dark-col
                                      bg-light-col bg-dark-col
                                      fg-rest bg-rest
                                      reverse-video-p)
  "Macro for setting default values for an ECB face.
The parameters are set for the following display-types:
- ((class color) (background light)): HEIGHT, BOLD-P, ITALIC-P, INHERIT
                                      FG-LIGHT-COL, BG-LIGHT-COL
- ((class color) (background dark)): HEIGHT, BOLD-P, ITALIC-P, INHERIT
                                     FG-DARK-COL, BG-DARK-COL
- t: HEIGHT, BOLD-P, ITALIC-P, INHERIT, FG-REST, BG-REST, REVERSE-VIDEO."
  `(list (list '((class color) (background light))
               (append (if (and ,height (not ecb-running-xemacs)) (list :height ,height))
                       (if ,bold-p (if (not ecb-running-xemacs)
                                       (list :weight 'bold)
                                     (list :bold t)))
                       (if ,italic-p (if (not ecb-running-xemacs)
                                         (list :slant 'italic)
                                       (list :italic t)))
                       (if (and ,inherit (not ecb-running-xemacs)) (list :inherit ,inherit))
                       (if ,fg-light-col (list :foreground ,fg-light-col))
                       (if ,bg-light-col (list :background ,bg-light-col))))
         (list '((class color) (background dark))
               (append (if (and ,height (not ecb-running-xemacs)) (list :height ,height))
                       (if ,bold-p (if (not ecb-running-xemacs)
                                       (list :weight 'bold)
                                     (list :bold t)))
                       (if ,italic-p (if (not ecb-running-xemacs)
                                         (list :slant 'italic)
                                       (list :italic t)))
                       (if (and ,inherit (not ecb-running-xemacs)) (list :inherit ,inherit))
                       (if ,fg-dark-col (list :foreground ,fg-dark-col))
                       (if ,bg-dark-col (list :background ,bg-dark-col))))
         (list 't (append (if (and ,height (not ecb-running-xemacs)) (list :height ,height))
                          (if ,bold-p (if (not ecb-running-xemacs)
                                          (list :weight 'bold)
                                        (list :bold t)))
                          (if ,italic-p (if (not ecb-running-xemacs)
                                            (list :slant 'italic)
                                          (list :italic t)))
                          (if (and ,inherit (not ecb-running-xemacs)) (list :inherit ,inherit))
                          (if ,fg-rest (list :foreground ,fg-rest))
                          (if ,bg-rest (list :foreground ,bg-rest))
                          (if ,reverse-video-p (list :reverse-video t))))))

(defface ecb-default-general-face (ecb-face-default 1.0)
  "*Basic face for all ECB tree-buffers.
