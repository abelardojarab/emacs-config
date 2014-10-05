;;; setup-vhdl.el ---

;; Copyright (C) 2014  Abelardo

;; Author: Abelardo <abelardo@abelardo-Aspire-7720>
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

;; Load verilog mode only when needed
(add-to-list 'load-path "~/.emacs.d/verilog-mode")
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )

;; Any files that end in .v should be in verilog mode
(setq auto-mode-alist (cons '("\\.v\\'" . verilog-mode) auto-mode-alist))

;; Any files in verilog mode should have their keywords colorized
(add-hook 'verilog-mode-hook 'font-lock-mode)

(defvar vhdl-compiler-options   "-work work")

(autoload 'vhdl-mode "vhdl-mode" "VHDL Editing Mode" t);
(setq auto-mode-alist (append '(("\\.vhd$"  . vhdl-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.vhdl$" . vhdl-mode)) auto-mode-alist))

(add-hook 'vhdl-mode-hook
          '(lambda ()
             ;;vhdl-electric enables templates
             (setq vhdl-electric-mode t)
             (setq vhdl-compiler 'v-system)
             (setq vhdl-stutter-mode t)
             (setq vhdl-intelligent-tab t)
             (setq vhdl-indent-tabs-mode nil)
             (setq-default vhdl-end-comment-column 120)
             (setq-default vhdl-standard '(93 nil))
             (setq vhdl-standard '(93 nil))
             (setq-default vhdl-underscore-is-part-of-word t)
             (setq vhdl-self-insert-comments nil)
             (setq vhdl-actual-port-name '("\"\\(.*\\)$\"" . "s\\1"))
             (setq vhdl-argument-list-indent nil)
             (setq vhdl-insert-empty-lines 'none)
             (setq vhdl-instance-name '(".*" . "\\&_inst_%d"))))

;; Turn on VHDL automatic templates
(add-hook 'vhdl-mode-hook (function (lambda () (abbrev-mode t))))

;; General configurations
(setq vhdl-argument-list-indent t)
(setq vhdl-basic-offset 4)
(setq vhdl-clock-edge-condition (quote function))
(setq vhdl-clock-name "clk")
(setq vhdl-copyright-string "-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor
-- Boston, MA  02110-1301, USA.
--
-- You can dowload a copy of the GNU General Public License here:
-- http://www.gnu.org/licenses/gpl.txt
--
-- Copyright (c) <year>
")
(setq vhdl-file-header "-------------------------------------------------------------------------------
<copyright>-------------------------------------------------------------------------------
-- Title      : <title string>
-- Project    : <project>
-------------------------------------------------------------------------------
-- File       : <filename>
-- Author     : <author>
-- Company    : <company>
-- Created    : <date>
-- Last update: <date>
-- Platform   : <platform>
-- Standard   : <standard>
<projectdesc>-------------------------------------------------------------------------------
-- Description: <cursor>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- <date>  1.0      <login>     Created
-------------------------------------------------------------------------------

")
(setq vhdl-file-name-case (quote downcase))
(setq vhdl-highlight-case-sensitive t)
(setq vhdl-highlight-special-words t)
(setq vhdl-index-menu t)
(setq vhdl-optional-labels (quote none))
(setq vhdl-platform-spec "Xilinx ISE")
(setq vhdl-reset-name "rst")
(setq vhdl-standard (quote (93 nil)))
(setq vhdl-testbench-declarations "    -- constants declaration
constant clock_period : time := 20 ns;  --! clock period
constant reset_after : time := 100 ns;  --! length of reset pulse
constant simulation_time : time := 1 ms;  --! when the simulation stops with standard \"OK\" assertion
")
(setq vhdl-testbench-include-configuration nil)
(setq vhdl-testbench-initialize-signals t)
(setq vhdl-testbench-statements "    -- clock generation
clock: process
BEGIN
clk <= '0';
wait for clock_period/2;
clk <= '1';
wait for clock_period/2;
end process;(add-hook 'makefile-mode-hook
(lambda()
  (setq show-trailing-whitespace t)))

rst <= '1' after reset_after;

-- stimulus generation
tb : PROCESS
BEGIN
-- Place stimulus here
rst <= '0';
wait until clk'event and clk = '1';
rst <= '1';
wait until clk'event and clk = '1';
wait for simulation_time;
assert false report \"NONE. End of simulation.\" severity failure;
end process;")
(setq vhdl-upper-case-constants t)
(setq vhdl-use-direct-instantiation (quote never))

;; Detecting error in Modelsim
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
                             (or (getenv "VHDL") "/opt/modeltech/bin/vcom")
                             (file-name-sans-extension file)
                             file))))))

(provide 'setup-vhdl)
;;; setup-vhdl.el ends here
