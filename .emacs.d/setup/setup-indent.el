;;; setup-indent.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2016  abelardo.jara-berrocal

;; Author: abelardo.jara-berrocal <ajaraber@plxcj9063.pdx.intel.com>
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

;; Auto-indent mode
(use-package auto-indent-mode
  :pin manual
  :load-path "~/.emacs.d/auto-indent-mode"
  :init (progn
          (setq auto-indent-indent-style 'conservative)
          (setq auto-indent-on-visit-file nil) ;; do not indent when a file is visit
          (setq auto-indent-blank-lines-on-move nil)
          (setq auto-indent-next-pair-timer-geo-mean (quote ((default 0.0005 0))))
          (setq auto-indent-disabled-modes-list (list (quote vhdl-mode))))
  :config (progn
            (auto-indent-global-mode)))

;; Permanent indentation guide
(use-package indent-hint
  :load-path "~/.emacs.d/indent-hint"
  :init (progn
          (setq indent-hint-background-overlay t)
          (setq indent-hint-bg nil))
  :config (progn
            (add-hook 'prog-mode-hook 'indent-hint-mode)
            (add-hook 'lisp-mode-hook 'indent-hint-lisp)))

(provide 'setup-indent)
;;; setup-indent.el ends here
