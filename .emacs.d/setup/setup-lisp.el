;;; setup-lisp.el ---

;; Copyright (C) 2014, 2015, 2016  abelardo.jara-berrocal

;; Author: Abelardo Jara-Berrocal <abelardojara@Abelardos-MacBook-Pro.local>
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

;; Enable Lisp auto-complete
(if (featurep 'auto-complete)
  (add-to-list 'ac-modes 'lisp-mode))

;; imenu support
(add-hook 'lisp-mode-hook
          (lambda ()
            (setq imenu-create-index-function 'imenu-example--create-lisp-index)
            (setq imenu-generic-expression scheme-imenu-generic-expression)))

(provide 'setup-lisp)
;;; setup-lisp.el ends here
