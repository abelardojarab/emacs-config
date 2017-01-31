;;; setup-diff.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojara@ubuntu03>
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

;; ediff
(use-package ediff
  :init (progn
          (defun my/setup-ediff ()
            (interactive)
            (ediff-setup-keymap)
            (define-key ediff-mode-map (kbd "<down>") #'ediff-next-difference)
            (define-key ediff-mode-map (kbd "<up>") #'ediff-previous-difference)))
          (add-hook 'ediff-mode-hook 'my/setup-ediff))
  :config (progn
            (setq ediff-window-setup-function 'ediff-setup-windows-plain
                  ;; Always split nicely for wide screens
                  ediff-split-window-function 'split-window-horizontally
                  ;; Ignore whitespace
                  ediff-diff-options "-w")))

(use-package ediff-wind
  :config
  (setq-default
   ediff-split-window-function #'split-window-horizontally
   ediff-window-setup-function #'ediff-setup-windows-plain))

(provide 'setup-diff)
;;; setup-diff.el ends here
