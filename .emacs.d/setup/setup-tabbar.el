;;; setup-tabbar.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2020  Abelardo Jara-Berrocal

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

;; Tabbar ruler pre-requisites
(use-package mode-icons
  :if (display-graphic-p)
  :demand t)

(defun tabbar-mode (mode)
  nil)

(use-package centaur-tabs
  :demand t
  :bind (("C-<prior>"   . centaur-tabs-backward)
         ("C-<next>"    . centaur-tabs-forward)
         ("C-c <right>" . centaur-tabs-forward)
         ("C-c <left>"  . centaur-tabs-backward))
  :init (tabbar-mode -1)
  :defines (centaur-tabs-style
            centaur-tabs-set-icons
            centaur-tabs-set-bar
            centaur-tabs-set-modified-marker
            centaur-tabs-modified-marker)
  :commands centaur-tabs-mode
  :custom ((centaur-tabs-set-bar             'above)
           (centaur-tabs-set-modified-marker t)
           (centaur-tabs-modified-marker     "o")
           (centaur-tabs-close-button        "×")
           (centaur-tabs-gray-out-icons      'buffer)
           (centaur-tabs-set-icons           t)
           (centaur-tabs-height              24)
           (x-underline-at-descent-line      t))
  :hook (after-init . centaur-tabs-mode)
  :config (progn
            (centaur-tabs-mode t)
            (centaur-tabs-group-by-projectile-project)
            (centaur-tabs-change-fonts "P22 Underground Book" 130)
            (add-hook 'centaur-tabs-mode (lambda () (tabbar-mode -1)))))

(provide 'setup-tabbar)
;;; setup-tabbar.el ends here
