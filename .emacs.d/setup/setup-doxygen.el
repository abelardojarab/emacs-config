;;; setup-doxygen.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2022  Abelardo Jara-Berrocal

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

(use-package doxymacs
  :defer t
  :commands (doxymacs-mode doxymacs-insert-file-comment)
  :bind (:map c-mode-map
              ("C-c TAB" . doxymacs-insert-function-comment)
              :map c++-mode-map
              ("C-c TAB" . doxymacs-insert-function-comment))
  :if (executable-find "doxymacs_parser")
  :diminish doxymacs-mode
  :load-path (lambda () (expand-file-name "doxymacs/build/lisp" user-emacs-directory))
  ;; :hook (c-mode-common . doxymacs-mode)
  )

(provide 'setup-doxygen)
;;; setup-doxygen.el ends here
