;;; setup-appearance.el ---

;; Copyright (C) 2014, 2015, 2016  abelardo.jara-berrocal

;; Author: abelardo.jara-berrocal <ajaraber@plxc25288.pdx.intel.com>
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

;; Fringe helper
(use-package fringe-helper
  :load-path "~/.emacs.d/fringe-helper")

;; Highlight the line
(use-package hl-line
  :config (progn
            (defun local-hl-line-mode-off ()
              (interactive)
              (make-local-variable 'global-hl-line-mode)
              (setq global-hl-line-mode t))

            ;; hl-line overrides the background of hi-lock’ed text, this will provide a fix
            (defadvice hi-lock-set-pattern (around use-overlays activate)
              (let ((font-lock-fontified nil))
                ad-do-it))
            (add-hook 'org-mode-hook 'local-hl-line-mode-off)))

;; Highlight the latest changes in the buffer (like text inserted from: yank, undo, etc.) until the next command is run
(use-package volatile-highlights
  :load-path "~/.emacs.d/volatile-highlights"
  :diminish volatile-highlights-mode
  :config (progn
            (volatile-highlights-mode t)))

;; Highlight blocks
(use-package highlight-blocks
  :load-path "~/.emacs.d/highlight-blocks"
  :diminish highlight-blocks-mode)

;; Permanent indentation guide
(use-package indent-hint
  :load-path "~/.emacs.d/indent-hint"
  :init (progn
          (setq indent-hint-background-overlay t)
          (setq indent-hint-bg nil))
  :config (progn
            (add-hook 'prog-mode-hook 'indent-hint-mode)
            (add-hook 'lisp-mode-hook 'indent-hint-lisp)))

;; Highlight symbol
(use-package highlight-symbol
  :load-path "~/.emacs.d/highlight-symbol"
  :config (progn (mapc (lambda (mode)
                         (add-hook mode 'highlight-symbol-mode))
                       '(prog-mode-hook
                         org-mode-hook
                         markdown-mode-hook
                         text-mode-hook))
                 (setq highlight-symbol-on-navigation-p t))
  :diminish highlight-symbol-mode)

(provide 'setup-appearance)
;;; setup-appearance.el ends here
