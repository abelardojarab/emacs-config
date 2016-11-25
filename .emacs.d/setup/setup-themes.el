;;; setup-themes.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Abelardo Jara-Berrocal

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

;; intellij theme
(add-to-list 'load-path (expand-file-name "intellij-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "intellij-theme/" user-emacs-directory))

;; darcula theme
(add-to-list 'load-path (expand-file-name "darcula-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "darcula-theme/" user-emacs-directory))

;; faff theme
(add-to-list 'load-path (expand-file-name "faff-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "faff-theme/" user-emacs-directory))

;; Plan9 theme
(add-to-list 'load-path (expand-file-name "plan9-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "plan9-theme/" user-emacs-directory))

;; Creamsody theme
(add-to-list 'load-path (expand-file-name "creamsody-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "creamsody-theme/" user-emacs-directory))

;; Ample theme
(add-to-list 'load-path (expand-file-name "ample-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "ample-theme/" user-emacs-directory))

;; Monokai theme
(add-to-list 'load-path (expand-file-name "monokai-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "monokai-theme/" user-emacs-directory))

;; Atom One Dark theme
(add-to-list 'load-path (expand-file-name "atom-one-dark-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "atom-one-dark-theme/" user-emacs-directory))

;; Zenburn theme
(add-to-list 'load-path (expand-file-name "zenburn-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "zenburn-theme/" user-emacs-directory))

;; Material theme
(add-to-list 'load-path (expand-file-name "material-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "material-theme/" user-emacs-directory))

;; Leuven theme
(add-to-list 'load-path (expand-file-name "leuven-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "leuven-theme/" user-emacs-directory))

;; Zerodark theme
(add-to-list 'load-path (expand-file-name "zerodark-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "zerodark-theme/" user-emacs-directory))

;; E-Ink theme
(add-to-list 'load-path (expand-file-name "eink-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "eink-theme/" user-emacs-directory))

;; Minimal theme
(add-to-list 'load-path (expand-file-name "minimal-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "minimal-theme/" user-emacs-directory))

;; Goose theme
(add-to-list 'load-path (expand-file-name "goose-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "goose-theme/" user-emacs-directory))

;; Tao theme
(add-to-list 'load-path (expand-file-name "tao-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "tao-theme/" user-emacs-directory))

;; White sand theme
(add-to-list 'load-path (expand-file-name "white-sand-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "white-sand-theme/" user-emacs-directory))

;; Monotropic theme
(add-to-list 'load-path (expand-file-name "monotropic-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "monotropic-theme/" user-emacs-directory))

;; Spike theme
(add-to-list 'load-path (expand-file-name "spike-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "spike-theme/" user-emacs-directory))

;; Dracula theme
(add-to-list 'load-path (expand-file-name "dracula-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "dracula-theme/" user-emacs-directory))

;; Grandshell theme
(add-to-list 'load-path (expand-file-name "grandshell-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "grandshell-theme/" user-emacs-directory))

;; Monochrome theme
(add-to-list 'load-path (expand-file-name "sexy-monochrome-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "sexy-monochrome-theme/" user-emacs-directory))

;; Spacemacs theme
(add-to-list 'load-path (expand-file-name "spacemacs-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "spacemacs-theme/" user-emacs-directory))

;; Apropospiate theme
(add-to-list 'load-path (expand-file-name "apropospriate-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "apropospriate-theme/" user-emacs-directory))

;; Tomorrow theme
(add-to-list 'load-path (expand-file-name "sanityinc-tomorrow-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "sanityinc-tomorrow-theme/" user-emacs-directory))
(require 'color-theme-sanityinc-tomorrow)

;; Solarized theme
(add-to-list 'load-path (expand-file-name "solarized-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "solarized-theme" user-emacs-directory))
(require 'solarized)
(setq solarized-scale-org-headlines nil)

;; So, fringe is nice actually, but the background for it kind of sucks in leuven
;; so I set it to the same color as the background
(defun my/set-face-fringe ()
  "Set the fringe background to the same color as the regular background."
  (interactive)
  (setq my/fringe-background-color
        (face-background 'default))
  (custom-set-faces
   `(fringe ((t (:background ,my/fringe-background-color))))))
(add-hook 'after-init-hook #'my/set-face-fringe)

;; Disable theme before setting a new one
(defun disable-themes (&optional themes)
  (mapc #'disable-theme (or themes custom-enabled-themes)))

;; Advice the load theme function
(defadvice load-theme (around load-theme-around)
  (let ()
    (disable-themes)
    ad-do-it
    (my/set-face-fringe)))
(ad-activate 'load-theme)

;; Choose different themes depending if we are using GUI or not
;; Console colors are enabled if "export TERM=xterm-256color" is added into .bashrc
(if (display-graphic-p)
    (load-theme 'monokai t)
  (load-theme 'monokai t))

;; Inherit theme for new frames
(setq frame-inherited-parameters '(width height face background-mode
                                         tool-bar-lines menu-bar-lines
                                         scroll-bar-width right-fringe left-fringe
                                         vertical-scroll-bars
                                         background-color foreground-color
                                         font alpha))

(provide 'setup-themes)
;;; setup-themes.el ends here
