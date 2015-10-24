;;; setup-dash.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Abelardo Jara

;; Author: Abelardo Jara <abelardojara@Abelardos-MacBook-Pro.local>
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

;; helm dash
(add-to-list 'load-path "~/.emacs.d/helm-dash")
(require 'helm-dash)
(setq helm-dash-min-length 2)
(setq helm-dash-docsets-path (expand-file-name "~/.emacs.d/docsets"))
(setq helm-dash-common-docsets '(
                                 "Markdown"
                                 "LaTeX"
                                 "Python_2"
                                 "Perl"
                                 "C++"
                                 "JavaScript"
                                 "Bash"
                                 "Tcl"
                                 "R"
                                 "Emacs_Lisp"))

(defun c-doc-hook ()
  (interactive)
  (setq-local helm-dash-docsets '("C" "C++" "Qt")))
(add-hook 'c-mode-common-hook 'c-doc-hook)

(defun python-doc-hook ()
  (interactive)
  (setq-local helm-dash-docsets '("Python_2" "NumPy" "SciPy")))
(add-hook 'python-mode-hook 'python-doc-hook)

(defun js2-doc-hook ()
  (interactive)
  (setq-local helm-dash-docsets '("JavaScript" "NodeJS" "HTML")))
(add-hook 'js2-mode-hook 'js2-doc-hook)

(defun java-doc-hook ()
  (interactive)
  (setq-local helm-dash-docsets '("Java")))
(add-hook 'java-mode-hook 'java-doc-hook)

(provide 'setup-dash)
;;; setup-dash.el ends here
