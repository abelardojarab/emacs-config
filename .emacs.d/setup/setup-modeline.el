;;; setup-modeline.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2023 Abelardo Jara-Berrocal

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
  :custom (nyan-cat-face-number 4))

;; Powerline
(use-package powerline
  :disabled t
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
  :disabled t
  :if (display-graphic-p)
  :after spaceline
  :config (progn
            (spaceline-helm-mode 1)
            (spaceline-emacs-theme)
            (spaceline-toggle-org-clock-on)
            (spaceline-toggle-minor-modes-off)
            (spaceline-toggle-version-control-off)))

(use-package spaceline-all-the-icons
  :disabled t
  :if (display-graphic-p)
  :after spaceline-config)

;; Improved list of minor-modes and related menus
(use-package minions
  :demand t
  :commands minions-mode
  :custom (minions-mode-line-lighter "#")
  :config (minions-mode 1))

;; Doom modeline
(use-package doom-modeline
  :defer t
  :commands doom-modeline-mode
  :hook (after-init   . doom-modeline-mode)
  :custom ((doom-modeline-buffer-file-name-style 'truncate-with-project)
           (doom-modeline-icon              t)
           (doom-modeline-major-mode-icon   t)
           (doom-modeline-minor-modes       nil)
           (doom-modeline-lsp               t)
           (doom-modeline-height            24)
           (doom-modeline-env-version       t)
           (doom-modeline-persp-name             t)
           (doom-modeline-buffer-file-name-style 'truncate-upto-project)
           (doom-modeline-project-detection      'projectile))
  :config (progn
            ;; Whether display minor modes in mode-line or not.
            (setq doom-modeline-minor-modes (featurep 'minions))
            ))

(provide 'setup-modeline)
;;; setup-modeline.el ends here
