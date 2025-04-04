;;; setup-bookmarks.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2021  Abelardo Jara-Berrocal

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

;; Bookmark Plus
(use-package bookmark
  :defer 10
  :custom (bookmark-save-flag 1)
  :config (progn
            (defadvice bookmark-load (around bar activate)
              (ignore-errors add-do-it))

            (setq-default bookmark-default-file (concat (file-name-as-directory my/emacs-cache-dir)
                                                      "bookmarks"))))

;; Visible bookmarks
(use-package bm
  :defer 10
  :commands (bm-repository-load
             bm-buffer-restore
             bm-buffer-save
             bm-buffer-save-all
             bm-repository-save
             bm-toggle
             bm-next)
  :bind (;; bind left mouse clicks and scrolls in left margin/fringe
         ("<left-fringe> <mouse-5>" . bm-next-mouse)
         ("<left-margin> <mouse-5>" . bm-next-mouse)
         ("<left-fringe> <mouse-4>" . bm-previous-mouse)
         ("<left-margin> <mouse-4>" . bm-previous-mouse)
         ("<left-fringe> <mouse-1>" . bm-toggle-mouse)
         ("<left-margin> <mouse-1>" . bm-toggle-mouse)
         ("C-<f2>"                  . bm-toggle)
         ("C-<f1>"                  . bm-next))
  :hook ((on-first-input   . bm-repository-load)
         (find-file        . bm-buffer-restore)
         (kill-buffer      . bm-buffer-save)
         (kill-emacs       . bm-repository-save))
  :custom ((bm-highlight-style             'bm-highlight-line-and-fringe)
           (bm-restore-repository-on-load  t)
           (bm-cycle-all-buffers           t))
  :init (setq bm-repository-file (concat (file-name-as-directory
                                          my/emacs-cache-dir) "bm-repository"))
  :custom (bm-buffer-persistence t)
  :config (when (display-graphic-p)
            (define-fringe-bitmap 'bm-marker-left [#xF8    ; ▮ ▮ ▮ ▮ ▮ 0 0 0
                                                   #xFC    ; ▮ ▮ ▮ ▮ ▮ ▮ 0 0
                                                   #xFE    ; ▮ ▮ ▮ ▮ ▮ ▮ ▮ 0
                                                   #x0F    ; 0 0 0 0 ▮ ▮ ▮ ▮
                                                   #x0F    ; 0 0 0 0 ▮ ▮ ▮ ▮
                                                   #xFE    ; ▮ ▮ ▮ ▮ ▮ ▮ ▮ 0
                                                   #xFC    ; ▮ ▮ ▮ ▮ ▮ ▮ 0 0
                                                   #xF8])) ; ▮ ▮ ▮ ▮ ▮ 0 0 0
  )

(provide 'setup-bookmarks)
;;; setup-bookmarks.el ends here
