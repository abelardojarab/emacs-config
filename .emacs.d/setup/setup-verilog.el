;;; setup-verilog.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2017  Abelardo Jara-Berrocal

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

;; Verilog mode
(use-package verilog-mode
  :mode ("\\.v\\'" . verilog-mode)
  :load-path (lambda () (expand-file-name "verilog-mode/" user-emacs-directory))
  :config (progn
            ;; Any files in verilog mode should have their keywords colorized
            (add-hook 'verilog-mode-hook 'font-lock-mode)))

(provide 'setup-verilog)
;;; setup-verilog.el ends here
