;;; setup-post.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015, 2016, 2017, 2018  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojarab@gmail.com>
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

;; Disable theme before setting a new one
(defun disable-themes (&optional themes)
  (mapc #'disable-theme (or themes custom-enabled-themes)))

;; So, fringe is nice actually, but the background for it kind of sucks in leuven
;; so I set it to the same color as the background
(defun my/set-face-fringe ()
  "Set the fringe background to the same color as the regular background."
  (interactive)
  (setq my/fringe-background-color
        (face-background 'default))
  (custom-set-faces
   `(fringe ((t (:background ,my/fringe-background-color))))))

;; Default ECB theme-ing
(defun my/set-face-ecb ()
  "Set the ecb background to the same color as the regular background."
  (interactive)
  (setq my/ecb-background-color
        (face-background 'default))
  (custom-set-faces
   `(ecb-default-general-face ((t (:background ,my/ecb-background-color :inherit fixed-pitch))))
   `(ecb-default-highlight-face ((t (:inherit helm-selection-line))))))

;; Default tabbar theme-ing
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
(add-hook 'after-init-hook #'my/set-face-ecb)

;; Advice the load theme function
(defadvice load-theme (around load-theme-around)
  (let ()
    (disable-themes)
    ad-do-it

    ;; Add required faces
    (my/set-face-fringe)
    (my/set-face-tabbar)
    (my/set-face-ecb)
    (setq-default mode-line-format '("%e" (:eval (spaceline-ml-custom))))
    ))
(ad-activate 'load-theme)

(if (file-exists-p custom-file-x)
    (add-hook 'after-init-hook (lambda ()
				 (load custom-file-x :noerror :nomessage)

				 ;; Add required faces
				 (my/set-face-fringe)
				 (my/set-face-tabbar)
				 (my/set-face-ecb)
				 (setq-default mode-line-format '("%e" (:eval (spaceline-ml-custom))))
				 )))

(provide 'setup-post)
;;; setup-post.el ends here
