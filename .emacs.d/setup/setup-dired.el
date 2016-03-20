;;; setup-dired.el ---                               -*- lexical-binding: t; -*-

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

(use-package dired
  :config  (setq dired-auto-revert-buffer t
                 dired-dwim-target t
                 dired-listing-switches "-alhF --group-directories-first"))

(use-package dired-x
  :init (progn
          (global-set-key (kbd "C-x C-j") 'dired-jump)
          (add-hook 'dired-mode-hook 'projectile-mode)
          (setq-default dired-omit-mode t)))

(use-package direx
  :load-path (lambda () (expand-file-name "direx/" user-emacs-directory))
  :config (progn
    (setq direx:closed-icon "+ ")
    (setq direx:leaf-icon "| ")
    (setq direx:open-icon "> ")
    (define-key direx:direx-mode-map [mouse-1] 'direx:mouse-2)
    (define-key direx:direx-mode-map [mouse-3] 'direx:mouse-1)
    (push '(direx:direx-mode :position left :width 30 :dedicated t :stick t :noselect t) popwin:special-display-config)))

(use-package direx-project
  :load-path (lambda () (expand-file-name "direx/" user-emacs-directory))
  :bind ("C-x C-j" . direx-project:jump-to-project-root-other-window))

(use-package dired-k
 :load-path (lambda () (expand-file-name "dired-k/" user-emacs-directory))
 :config (progn
           (define-key dired-mode-map (kbd "K") 'dired-k)
           (add-hook 'dired-initial-position-hook 'dired-k)))

(provide 'setup-dired)
;;; setup-dired.el ends here
