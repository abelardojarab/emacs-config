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

;; Monokai theme
(add-to-list 'load-path "~/.emacs.d/monokai-emacs")
(add-to-list 'custom-theme-load-path "~/.emacs.d/monokai-emacs")

;; Monokai theme
(add-to-list 'load-path "~/.emacs.d/monokai-extended-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/monokai-extended-theme")

;; Atom theme
(add-to-list 'load-path "~/.emacs.d/atom-dark-theme-emacs")
(add-to-list 'custom-theme-load-path "~/.emacs.d/atom-dark-theme-emacs")

;; Zenburn theme
(add-to-list 'load-path "~/.emacs.d/zenburn-emacs")
(add-to-list 'custom-theme-load-path "~/.emacs.d/zenburn-emacs")

;; Faff theme
(add-to-list 'load-path "~/.emacs.d/emacs-faff-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-faff-theme")

;; Material theme
(add-to-list 'load-path "~/.emacs.d/emacs-matherial-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-material-theme")

;; Leuven theme
(add-to-list 'load-path "~/.emacs.d/emacs-leuven-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-leuven-theme")

;; FlatUI theme
(add-to-list 'load-path "~/.emacs.d/emacs-flatui-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-flatui-theme")

;; FlatUI theme
(add-to-list 'load-path "~/.emacs.d/pastelmac-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/pastelmac-theme")

;; Zerodark theme
(add-to-list 'load-path "~/.emacs.d/zerodark-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/zerodark-theme")

;; Apropospriate theme
;; (add-to-list 'load-path "~/.emacs.d/apropospriate-theme")
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/apropospriate-theme")
;; (require 'apropospriate)

;; Solarized theme
(add-to-list 'load-path "~/.emacs.d/solarized-emacs")
(add-to-list 'custom-theme-load-path "~/.emacs.d/solarized-emacs")
(require 'solarized)
(setq solarized-scale-org-headlines nil)

;; Different possible themes
;; (load-theme 'atom-dark t)
;; (load-theme 'zenburn t)
;; (load-theme 'leuven t)
;; (load-theme 'zerodark t)
;; (load-theme 'material t)
;; (load-theme 'FlatUI t)
;; (load-theme 'faff t)
(load-theme 'monokai t)
;; (load-theme 'monokai-extended t)
;; (load-theme 'pastelmac t)
;; (load-theme 'solarized-dark t)
;; (load-theme 'material-light t)
;; (load-theme 'apropospriate-light t)

;; Fringe helper
(use-package fringe-helper
  :pin manual
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
  :pin manual
  :load-path "~/.emacs.d/volatile-highlights"
  :diminish volatile-highlights-mode
  :config (progn
            (volatile-highlights-mode t)))

;; Highlight blocks
(use-package highlight-blocks
  :load-path "~/.emacs.d/highlight-blocks"
  :pin manual
  :diminish highlight-blocks-mode
  :config (add-hook 'prog-mode-hook 'highlight-blocks-mode))

;; Permanent indentation guide
(use-package indent-hint
  :pin manual
  :load-path "~/.emacs.d/indent-hint"
  :init (progn
          (setq indent-hint-background-overlay t)
          (setq indent-hint-bg nil))
  :config (progn
            (add-hook 'prog-mode-hook 'indent-hint-mode)
            (add-hook 'lisp-mode-hook 'indent-hint-lisp)))

;; Highlight symbol
(add-to-list 'load-path "~/.emacs.d/highlight-symbol")
(use-package highlight-symbol
  :load-path "~/.emacs.d/highlight-symbol"
  :pin manual
  :config (progn (mapc (lambda (mode)
                         (add-hook mode 'highlight-symbol-mode))
                       '(prog-mode-hook
                         org-mode-hook
                         markdown-mode-hook
                         text-mode-hook))
                 (setq highlight-symbol-on-navigation-p t))
  :diminish highlight-symbol-mode)

;; Blinking cursor
(use-package heartbeat-cursor
  :pin manual
  :config (progn
            (add-hook 'prog-mode-hook (lambda () (heartbeat-cursor-mode)))
            (add-hook 'org-mode-hook (lambda () (heartbeat-cursor-mode)))))

(provide 'setup-appearance)
;;; setup-appearance.el ends here
