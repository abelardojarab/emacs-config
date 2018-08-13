;;; setup-imenu.el ---                               -*- lexical-binding: t; -*-

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

;; imenu
(use-package imenu-anywhere
  :load-path (lambda () (expand-file-name "imenu-anywhere/" user-emacs-directory)))

;; imenu list
(use-package imenu-list
  :load-path (lambda () (expand-file-name "imenu-list/" user-emacs-directory))
  :config (progn
            (setq imenu-list-size 0.2)
            (setq imenu-list-focus-after-activation t)
            (setq imenu-list-auto-resize t)
            (setq imenu-list-position 'right)))

;; imenus
(use-package imenus
  :load-path (lambda () (expand-file-name "imenus/" user-emacs-directory)))

(provide 'setup-imenu)
;;; setup-imenu.el ends here
