;;; setup-modeline.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Abelardo Jara-Berrocal

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

;; Nyan cat
(use-package nyan-mode
  :commands nyan-mode
  :custom (nyan-cat-face-number 4)
  :hook (doom-modeline-mode . nyan-mode))

;; Powerline
(use-package powerline
  :after projectile
  :config (unless (display-graphic-p)
            (powerline-default-theme)))

;; Spaceline
(use-package spaceline
  :if (display-graphic-p)
  :after powerline
  :custom ((powerline-default-separator 'slant)
           (spaceline-display-default-perspective t)
           (spaceline-highlight-face-func 'spaceline-highlight-face-modified)
           (spaceline-flycheck-bullet "• %s")
           (spaceline-separator-dir-left '(left . left))
           (spaceline-separator-dir-right '(right . right))))

;; Spaceline configuration
(use-package spaceline-config
  :if (display-graphic-p)
  :after spaceline
  :config (spaceline-helm-mode))

(use-package spaceline-all-the-icons
  :if (display-graphic-p)
  :after spaceline-config)

;; Customize Emacs lighters
(use-package delight
  :config (progn
            (defadvice powerline-major-mode (around delight-powerline-major-mode activate)
              (let ((inhibit-mode-name-delight nil)) ad-do-it))
            (defadvice powerline-minor-modes (around delight-powerline-minor-modes activate)
              (let ((inhibit-mode-name-delight nil)) ad-do-it))))

;; Doom modeline
(use-package doom-modeline
  :defer t
  :commands doom-modeline-mode
  :hook (after-init . doom-modeline-mode)
  :custom ((doom-modeline-buffer-file-name-style 'truncate-with-project)
           (doom-modeline-icon            t)
           (doom-modeline-major-mode-icon t)
           (doom-modeline-minor-modes     nil)))

(provide 'setup-modeline)
;;; setup-modeline.el ends here
