;;; setup-imenu.el ---                               -*- lexical-binding: t; -*-

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

;; imenu integration with helm/ivy
(use-package imenu-anywhere)

;; imenu list
(use-package imenu-list
  :defer t
  :commands (imenu-list
             imenu-list-smart-toggle)
  :custom ((imenu-list-size                   0.3)
           (imenu-list-focus-after-activation t)
           (imenu-list-auto-resize            t)
           (imenu-list-position               'right)))

;; imenus
(use-package imenus)

(provide 'setup-imenu)
;;; setup-imenu.el ends here
