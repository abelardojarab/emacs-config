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

;; Tomorrow theme
(add-to-list 'load-path (expand-file-name "sanityinc-tomorrow-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "sanityinc-tomorrow-theme/" user-emacs-directory))
(require 'color-theme-sanityinc-tomorrow)

;; Solarized theme
(add-to-list 'load-path (expand-file-name "solarized-theme/" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "solarized-theme" user-emacs-directory))
(require 'solarized)
(setq solarized-scale-org-headlines nil)

;; Different possible themes
;; (load-theme 'atom-one-dark t)
;; (load-theme 'zenburn t)
;; (load-theme 'leuven t)
;; (load-theme 'zerodark t)
;; (load-theme 'material t)
;; (load-theme 'white-sand) ;; best light theme
;; (load-theme 'minimal-light) ;; best ink theme
;; (load-theme 'monokai t) ;; best dark color scheme
;; (load-theme 'goose t)
;; (load-theme 'tao-yang t)
;; (load-theme 'tao-yin t)
;; (load-theme 'eink)
;; (load-theme 'solarized-dark t)
;; (load-theme 'material-light t)
;; (load-theme 'sanityinc-tomorrow-night t)
;; (load-theme 'spike t)
;; (load-theme 'dracula t)

;; Choose different themes depending if we are using GUI or not
;; Console colors are enabled if "export TERM=xterm-256color" is added into .bashrc
(if (display-graphic-p)
    (load-theme 'monokai t)
  (load-theme 'monokai t))

;; Assure theme loading for client frames
;; http://sachachua.com/blog/2016/04/keep-emacs-alive-x-crashes-running-background-daemon/
(defun my/setup-color-theme ()
  (interactive)
  (if (display-graphic-p)
      (load-theme 'monokai t)
    (load-theme 'monokai)))
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (my/setup-color-theme)))

;; So, fringe is nice actually, but the background for it kind of sucks in leuven
;; so I set it to the same color as the background
(defun my/set-fringe-background ()
  "Set the fringe background to the same color as the regular background."
  (interactive)
  (setq my/fringe-background-color
        (face-background 'default))
  (custom-set-faces
   `(fringe ((t (:background ,my/fringe-background-color))))))

(add-hook 'after-init-hook #'my/set-fringe-background)

(provide 'setup-themes)
;;; setup-themes.el ends here
