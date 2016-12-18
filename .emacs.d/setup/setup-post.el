;;; setup-post.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015, 2016  abelardo.jara-berrocal

;; Author: Abelardo Jara-Berrocal <abelardojara@Abelardos-MacBook-Pro.local>
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

;; Add all diminished modes here

;; Dashboard
(use-package dashboard
  :load-path (lambda () (expand-file-name "dashboard/" user-emacs-directory))
  :disabled t
  :config (progn
            (dashboard-setup-startup-hook)))

;; So, fringe is nice actually, but the background for it kind of sucks in leuven
;; so I set it to the same color as the background
(defun my/set-face-fringe ()
  "Set the fringe background to the same color as the regular background."
  (interactive)
  (setq my/fringe-background-color
        (face-background 'default))
  (custom-set-faces
   `(fringe ((t (:background ,my/fringe-background-color))))))

;; Disable theme before setting a new one
(defun disable-themes (&optional themes)
  (mapc #'disable-theme (or themes custom-enabled-themes)))

;; Default tabbar theme-ing
(use-package color)
(defun my/set-face-tabbar ()
  "Set the tabbar background to the same color as the regular background."
  (interactive)
  (setq tabbar-separator '(0.0))
  (setq my/tabbar-foreground-color
        (face-foreground 'default))
  (setq my/tabbar-background-color
        (face-background 'default))
  (setq my/tabbar-back-color
        (color-lighten-name (face-background 'default) 12))
  (custom-set-faces
   ;; tabbar background
   `(tabbar-default ((t (:inherit fixed-pitch :background ,my/tabbar-back-color :foreground ,my/tabbar-foreground-color))))
   `(tabbar-button ((t (:inherit tabbar-default :foreground ,my/tabbar-background-color))))
   `(tabbar-button-highlight ((t (:inherit tabbar-default))))
   `(tabbar-highlight ((t (:underline t))))
   ;; selected tab
   `(tabbar-selected ((t (:inherit tabbar-default :background ,my/tabbar-background-color))))
   `(tabbar-separator ((t (:inherit tabbar-default :background ,my/tabbar-back-color))))
   `(tabbar-unselected ((t (:inherit tabbar-default))))))


(add-hook 'after-init-hook #'my/set-face-fringe)
(add-hook 'after-init-hook #'my/set-face-tabbar)

;; Advice the load theme function
(defadvice load-theme (around load-theme-around)
  (let ()
    (disable-themes)
    ad-do-it

    ;; Add required faces
    (my/set-face-fringe)
    (my/set-face-tabbar)
    (spaceline-spacemacs-theme)))
(ad-activate 'load-theme)

;; Inherit theme for new frames
(setq frame-inherited-parameters '(width height face background-mode
                                         tool-bar-lines menu-bar-lines
                                         scroll-bar-width right-fringe left-fringe
                                         vertical-scroll-bars
                                         background-color foreground-color
                                         font alpha))

;; http://sachachua.com/blog/2016/04/keep-emacs-alive-x-crashes-running-background-daemon/
(defun on-frame-open (&optional frame)
  (set-face-background 'default "unspecified-bg" frame)
  (select-frame frame)
  (load-theme 'material t))
(add-hook 'after-make-frame-functions 'on-frame-open)

(provide 'setup-post)
;;; setup-post.el ends here
