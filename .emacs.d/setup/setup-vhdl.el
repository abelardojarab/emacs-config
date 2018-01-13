;;; setup-vhdl.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2017, 2018  Abelardo Jara-Berrocal

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

;; VHDL mode
(use-package vhdl-mode
  :mode ("\\.vhdl?\\'" "\\.vho\\'")
  :commands vhdl-mode
  :load-path (lambda () (expand-file-name "vhdl-mode/" user-emacs-directory))
  :config (progn
            ;; Compilation flag for ModelSim
            (defvar vhdl-compiler-options  "-work work")

            ;; Recognizing errors reported by ModelSim
            ;; For Modelsim: vcom error format:
            ;;** Error: switch.vhd(68): near ")": expecting: IDENTIFIER
            (eval-after-load "compile"
              '(setq compilation-error-regexp-alist
                     (cons '("^\*\* Error: \\([^(]+\\)\(\\([0-9]+\\)\):" 1 2)
                           compilation-error-regexp-alist)))

            ;; Fixing the problems with compile in VHDL/C/C++
            (require 'compile)
            (add-hook 'vhdl-mode-hook
                      (lambda ()
                        (unless (file-exists-p "Makefile")
                          (set (make-local-variable 'compile-command)
                               ;; $(VHDL) -work work $<
                               (let ((file (file-name-nondirectory buffer-file-name)))
                                 (format "%s -work work %s.vhd"
                                         (or (getenv "VHDL") "vcom")
                                         (file-name-sans-extension file)
                                         file))))))

            ;; VHDL simulation
            (add-hook 'vhdl-mode-hook
                      (lambda ()
                        (setq explicit-shell-file-name "bash")
                        (setq shell-file-name explicit-shell-file-name)
                        (defun vhdl-simulate () (interactive)
                               ;; $(VHDL) -work work $<
                               (let ((file (file-name-nondirectory buffer-file-name))))
                               (shell-command (concat "vsim -c " (file-name-sans-extension (file-name-nondirectory buffer-file-name)) " -do \"vcd file " (file-name-sans-extension (file-name-nondirectory buffer-file-name)) ".vcd; vcd add -r /*; run -all; vcd checkpoint; quit -f;\"")))))))

(provide 'setup-vhdl)
;;; setup-vhdl.el ends here
