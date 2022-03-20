;;; setup-dired-plugins.el ---                       -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2022  Abelardo Jara-Berrocal

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

(use-package dired-sidebar
  :defer t
  :after dired
  :commands (dired-sidebar-toggle-sidebar)
  :custom ((dired-sidebar-use-term-integration t)
           (dired-sidebar-use-all-the-icons    t)
           (dired-sidebar-use-custom-font      t))
  :config  (progn
             (if (display-graphic-p)
                 (setq dired-sidebar-theme 'icons)
               (setq dired-sidebar-theme 'nerd))

             (defadvice dired-sidebar-point-at-file (around bar activate)
               (ignore-errors add-do-it))

             (defadvice dired-sidebar-refresh-buffer (around bar activate)
               (ignore-errors add-do-it))))

;; neotree side bar
(use-package neotree
  :defer t
  :bind* ("C-e" . neotree-toggle)
  :bind (("C-e" . neotree-project-dir))
  :commands (neotree-toggle
             neotree-show
             neotree-project-dir)
  :bind (:map neotree-mode-map
              (("<C-return>" . neotree-change-root)
               ("C"          . neotree-change-root)
               ("c"          . neotree-create-node)
               ("+"          . neotree-create-node)
               ("d"          . neotree-delete-node)
               ("r"          . neotree-rename-node)
               ("q"          . kill-buffer-and-window)))
  :custom ((neo-window-width          32)
           (neo-create-file-auto-open t)
           (neo-banner-message        nil)
           (neo-show-updir-line       t)
           (neo-window-fixed-size     nil)
           (neo-vc-integration        nil)
           (neo-mode-line-type        'neotree)
           (neo-smart-open            t)
           (neo-dont-be-alone         t)
           (neo-show-hidden-files     t)
           (neo-mode-line-type        'none)
           (neo-auto-indent-point     t))
  :init (defun neotree-project-dir ()
          "Open NeoTree using the project root, using find-file-in-project,
or the current buffer directory."
          (interactive)
          (let ((project-dir
                 (ignore-errors
                   (ffip-project-root)))
                (file-name (buffer-file-name))
                (neo-smart-open t))
            (if (and (fboundp 'neo-global--window-exists-p)
                     (neo-global--window-exists-p))
                (neotree-hide)
              (progn
                (neotree-show)
                (if project-dir
                    (neotree-dir project-dir))
                (if file-name
                    (neotree-find file-name))))))
  :config (progn
            (setq neo-theme (if (display-graphic-p) 'icons 'nerd))
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

;; Modern dired
(use-package dirvish
  :defer t
  :commands dirvish)

(provide 'setup-dired-plugins)
;;; setup-dired-plugins.el ends here
