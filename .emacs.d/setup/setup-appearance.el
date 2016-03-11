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

;; Fix appearance of Windows Unicode characters
(standard-display-ascii ?\200 [15])
(standard-display-ascii ?\201 [21])
(standard-display-ascii ?\202 [24])
(standard-display-ascii ?\203 [13])
(standard-display-ascii ?\204 [22])
(standard-display-ascii ?\205 [25])
(standard-display-ascii ?\206 [12])
(standard-display-ascii ?\210 [23])
(standard-display-ascii ?\211 [14])
(standard-display-ascii ?\212 [18])
(standard-display-ascii ?\214 [11])
(standard-display-ascii ?\221 [?\'])
(standard-display-ascii ?\222 [?\'])
(standard-display-ascii ?\223 [?\"])
(standard-display-ascii ?\224 [?\"])
(standard-display-ascii ?\225 [?\*])
(standard-display-ascii ?\226 "---")
(standard-display-ascii ?\227 "--")

;; Fringe helper
(add-to-list 'load-path "~/.emacs.d/fringe-helper")
(require 'fringe-helper)

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

;; Highlight the line
(require 'hl-line)
(defun local-hl-line-mode-off ()
  (interactive)
  (make-local-variable 'global-hl-line-mode)
  (setq global-hl-line-mode t))

;; hl-line overrides the background of hi-lock’ed text, this will provide a fix
(defadvice hi-lock-set-pattern (around use-overlays activate)
  (let ((font-lock-fontified nil))
    ad-do-it))
(add-hook 'org-mode-hook 'local-hl-line-mode-off)

;; Highlight the latest changes in the buffer (like text inserted from: yank, undo, etc.) until the next command is run
(add-to-list 'load-path "~/.emacs.d/volatile-highlights")
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; Highlight blocks
(add-to-list 'load-path "~/.emacs.d/highlight-blocks")
(require 'highlight-blocks)

;; Permanent indentation guide
(add-to-list 'load-path "~/.emacs.d/indent-hint")
(setq indent-hint-background-overlay t)
(setq indent-hint-bg nil)
(require 'indent-hint)
(add-hook 'prog-mode-hook 'indent-hint-mode)
(add-hook 'lisp-mode-hook 'indent-hint-lisp)

;; Highlight symbol
(add-to-list 'load-path "~/.emacs.d/highlight-symbol")
(require 'highlight-symbol)
(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
(add-hook 'org-mode-hook (lambda () (highlight-symbol-mode)))
(add-hook 'markdown-mode-hook (lambda () (highlight-symbol-mode)))
(add-hook 'text-mode-hook (lambda () (highlight-symbol-mode)))
(setq highlight-symbol-on-navigation-p t)

;; Blinking cursor
(require 'heartbeat-cursor)
(add-hook 'prog-mode-hook (lambda () (heartbeat-cursor-mode)))
(add-hook 'org-mode-hook (lambda () (heartbeat-cursor-mode)))

(provide 'setup-appearance)
;;; setup-appearance.el ends here
