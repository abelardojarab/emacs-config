;;; setup-undoandredo.el ---                         -*- lexical-binding: t; -*-

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

;; Redo
(use-package redo+
  :bind ("C-y" . redo))

;; Better undo
(use-package undo-tree
  :defer 10
  :bind (("C-S-z"    . undo-tree-redo)
         ("C-z"      . undo-tree-undo)
         ("<undo>"   . undo-tree-undo)
         :map ctl-x-map
         ("u"        . undo-tree-undo))
  :diminish undo-tree-mode
  :init (global-undo-tree-mode t)
  :commands global-undo-tree-mode
  :config (progn
            (setq undo-no-redo                        t
                  undo-tree-visualizer-diff           t
                  undo-tree-visualizer-timestamps     t
                  undo-tree-auto-save                 t
                  undo-tree-auto-save-history         t
                  undo-tree-history-directory-alist   (list
                                                       (cons "."
                                                             (concat my/emacs-cache-dir
                                                                     "/undo-tree-hist/"))))))

(provide 'setup-undoandredo)
;;; setup-undoandredo.el ends here
