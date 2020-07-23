;;; setup-dired-plugins.el ---                       -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2020  Abelardo Jara-Berrocal

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

;; Narrow dired to match filter
(use-package dired-narrow
  :defer t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package dired-sidebar
  :defer t
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :after dired
  :commands (dired-sidebar-toggle-sidebar)
  :config  (progn
             (if (display-graphic-p)
                 (setq dired-sidebar-theme 'icons)
               (setq dired-sidebar-theme 'nerd))

             (defadvice dired-sidebar-point-at-file (around bar activate)
               (ignore-errors add-do-it))

             (defadvice dired-sidebar-refresh-buffer (around bar activate)
               (ignore-errors add-do-it))

             (setq dired-sidebar-use-term-integration t
                   dired-sidebar-use-all-the-icons    t
                   dired-sidebar-use-custom-font      t)))

;; neotree side bar
(use-package neotree
  :defer t
  :commands (neotree-toggle)
  :bind (:map neotree-mode-map
              (("<C-return>" . neotree-change-root)
               ("C"          . neotree-change-root)
               ("c"          . neotree-create-node)
               ("+"          . neotree-create-node)
               ("d"          . neotree-delete-node)
               ("r"          . neotree-rename-node)
               ("q"          . kill-buffer-and-window)))
  :custom ((neo-window-width 32)
       (neo-create-file-auto-open t)
       (neo-banner-message nil)
       (neo-show-updir-line t)
       (neo-window-fixed-size nil)
       (neo-vc-integration nil)
       (neo-mode-line-type 'neotree)
       (neo-smart-open t)
       (neo-dont-be-alone t)
       (neo-show-hidden-files t)
       (neo-mode-line-type 'none)
       (neo-auto-indent-point t))
  :config (progn
        (setq neo-theme (if (display-graphic-p) 'nerd 'arrow))
        (setq neo-hidden-regexp-list '("venv" "\\.pyc$" "~$" "\\.git" "__pycache__" ".DS_Store"))

        ;; Fix neotree to not collide with ecb
        (defun neo-global--create-window ()
          "Create global neotree window."
          (let ((window nil)
            (split-width-threshold 100)
            (buffer (neo-global--get-buffer t))
            (window-pos (if (eq neo-window-position 'left) 'left 'right)))
        (setq window
              (select-window
               ;; (split-window
               ;;  (frame-root-window (window-frame (selected-window)))
               ;;  nil window-pos)
               (split-window)))
        (neo-window--init window buffer)
        (neo-global--attach)
        (neo-global--reset-width)
        window))))

(provide 'setup-dired-plugins)
;;; setup-dired-plugins.el ends here
