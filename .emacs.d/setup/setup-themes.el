;;; setup-themes.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Abelardo Jara

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

(provide 'setup-themes)
;;; setup-themes.el ends here
