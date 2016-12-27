;;; setup-modeline.el ---

;; Copyright (C) 2014, 2015, 2016 Abelardo Jara-Berrocal

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

;; Nyan cat
(use-package nyan-mode
  :if (display-graphic-p)
  :commands nyan-mode
  :load-path (lambda () (expand-file-name "nyan-mode/" user-emacs-directory))
  :config (progn
            (nyan-mode t)
            (nyan-start-animation)))

;; Powerline
(use-package powerline
  :after projectile
  :load-path (lambda () (expand-file-name "powerline/" user-emacs-directory)))

(use-package spaceline
  :after powerline
  :load-path (lambda () (expand-file-name "spaceline/" user-emacs-directory))
  :config (progn
            (require 'spaceline-config)
            (spaceline-spacemacs-theme)
            (spaceline-helm-mode)
            (spaceline-toggle-buffer-size-off)))

(use-package airline-themes
  :disabled t
  :after powerline
  :load-path (lambda () (expand-file-name "airline-themes/" user-emacs-directory))
  :config (progn
            (kill-local-variable 'mode-line-format)
            (airline-themes-set-modeline)
            (load-theme 'airline-dark)))

(provide 'setup-modeline)
;;; setup-modeline.el ends here
