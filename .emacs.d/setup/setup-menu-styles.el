;;; setup-menu-styles.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Abelardo Jara-Berrocal

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

(require 'markdown-mode)
(require 'org)

(require 'setup-menu-macros)

;; (defun cc/org-emphasize-reset ()
;;   ;; this won't work when org-hide-emphasis-markers is turned on.
;;   (interactive)
;;   (org-emphasize ?\s))

;;; Code:

(defun my/emphasize-bold ()
  "Mark region bold for Org or Markdown modes."
  (interactive)
  (cond ((derived-mode-p 'org-mode)
         (org-emphasize ?*))
        ((derived-mode-p 'markdown-mode)
         (markdown-insert-bold))
        (t nil)))

(defun my/emphasize-italic ()
  "Mark region italic for Org or Markdown modes."
  (interactive)
  (cond ((derived-mode-p 'org-mode)
         (org-emphasize ?/))
        ((derived-mode-p 'markdown-mode)
         (markdown-insert-italic))
        (t nil)))

(defun my/emphasize-code ()
  "Mark region code for Org or Markdown modes."
  (interactive)
  (cond ((derived-mode-p 'org-mode)
         (org-emphasize ?~))
        ((derived-mode-p 'markdown-mode)
         (markdown-insert-code))
        (t nil)))

(defun my/emphasize-underline ()
  "Mark region underline for Org mode."
  (interactive)
  (cond ((derived-mode-p 'org-mode)
         (org-emphasize ?_))
        (t nil)))

(defun my/emphasize-verbatim ()
  "Mark region verbatim for Org mode."
  (interactive)
  (cond ((derived-mode-p 'org-mode)
         (org-emphasize ?=))
        (t nil)))

(defun my/emphasize-strike-through ()
  "Mark region strike-through for Org or Markdown modes."
  (interactive)
  (cond ((derived-mode-p 'org-mode)
         (org-emphasize ?+))
        ((derived-mode-p 'markdown-mode)
         (markdown-insert-strike-through))
        (t nil)))

(easy-menu-define my/emphasize-menu nil
  "Keymap for Emphasize Menu."
  '("Style"
    :visible (region-active-p)
    ["Bold" my/emphasize-bold
     :enable (region-active-p)
     :visible (or (derived-mode-p 'org-mode) (derived-mode-p 'markdown-mode))
     :help "Bold selected region"]
    ["Italic" my/emphasize-italic
     :enable (region-active-p)
     :visible (or (derived-mode-p 'org-mode) (derived-mode-p 'markdown-mode))
     :help "Italic selected region"]
    ["Code" my/emphasize-code
     :enable (region-active-p)
     :visible (or (derived-mode-p 'org-mode) (derived-mode-p 'markdown-mode))
     :help "Code selected region"]
    ["Underline" my/emphasize-underline
     :enable (region-active-p)
     :visible (derived-mode-p 'org-mode)
     :help "Underline selected region"]
    ["Verbatim" my/emphasize-verbatim
     :enable (region-active-p)
     :visible (derived-mode-p 'org-mode)
     :help "Verbatim selected region"]
    ["Strike Through" my/emphasize-strike-through
     :enable (region-active-p)
     :visible (or (derived-mode-p 'org-mode) (derived-mode-p 'markdown-mode))
     :help "Strike-through selected region"]))

(provide 'setup-menu-styles)
;;; setup-menu-styles.el ends here
