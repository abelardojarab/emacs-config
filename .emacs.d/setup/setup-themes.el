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
(add-to-list 'load-path (expand-file-name "monokai-emacs/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "monokai-emacs/" user-emacs-directory))

;; Monokai theme
(add-to-list 'load-path (expand-file-name "monokai-extended-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "monokai-extended-theme/" user-emacs-directory))

;; Atom theme
(add-to-list 'load-path (expand-file-name "atom-dark-theme-emacs/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "atom-dark-theme-emacs/" user-emacs-directory))

;; Zenburn theme
(add-to-list 'load-path (expand-file-name "zenburn-emacs/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "zenburn-emacs/" user-emacs-directory))

;; Faff theme
(add-to-list 'load-path (expand-file-name "emacs-faff-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "emacs-faff-theme/" user-emacs-directory))

;; Material theme
(add-to-list 'load-path (expand-file-name "emacs-material-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "emacs-material-theme/" user-emacs-directory))

;; Leuven theme
(add-to-list 'load-path (expand-file-name "emacs-leuven-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "emacs-leuven-theme/" user-emacs-directory))

;; FlatUI theme
(add-to-list 'load-path (expand-file-name "emacs-flatui-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "emacs-flatui-theme" user-emacs-directory))

;; FlatUI theme
(add-to-list 'load-path (expand-file-name "pastelmac-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "pastelmac-theme/" user-emacs-directory))

;; Zerodark theme
(add-to-list 'load-path (expand-file-name "zerodark-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "zerodark-theme/" user-emacs-directory))

;; E-Ink theme
(add-to-list 'load-path (expand-file-name "eink-emacs/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "eink-emacs/" user-emacs-directory))

;; Twilight bright theme
(add-to-list 'load-path (expand-file-name "twilight-bright-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "twilight-bright-theme/" user-emacs-directory))

;; Plan9 theme
(add-to-list 'load-path (expand-file-name "plan9-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "plan9-theme/" user-emacs-directory))

;; Minimal theme
(add-to-list 'load-path (expand-file-name "minimal-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "minimal-theme/" user-emacs-directory))

;; Solarized theme
(add-to-list 'load-path (expand-file-name "solarized-emacs/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "solarized-emacs" user-emacs-directory))
(require 'solarized)
(setq solarized-scale-org-headlines nil)

;; Different possible themes
;; (load-theme 'atom-dark t)
;; (load-theme 'zenburn t)
;; (load-theme 'leuven t)
(load-theme 'zerodark t)
;; (load-theme 'material t)
;; (load-theme 'FlatUI t)
;; (load-theme 'faff t)
;; (load-theme 'twilight-brigh)
;; (load-theme 'plan9)
;; (load-theme 'minimal-light) ;; best ink theme
;; (load-theme 'monokai t) ;; best dark color scheme
;; (load-theme 'eink)
;; (load-theme 'monokai-extended t)
;; (load-theme 'pastelmac t)
;; (load-theme 'solarized-dark t)
;; (load-theme 'material-light t)

(provide 'setup-themes)
;;; setup-themes.el ends here
