;;; setup-polymode.el ---

;; Copyright (C) 2016  Abelardo Jara-Berrocal

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
(use-package polymode
  :commands polymode-mode
  :load-path (lambda () (expand-file-name "polymode/" user-emacs-directory))
  :init (add-to-list 'load-path (expand-file-name "polymode/modes" user-emacs-directory))
  :config (progn
            (require 'poly-R)
            (require 'poly-markdown)
            (require 'poly-org)
            (setq pm-weaver "knitR-ESS" ;; Default weaver
                  pm-exporter "pandoc")))

(provide 'setup-polymode)
