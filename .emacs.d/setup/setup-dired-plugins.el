;;; setup-dired-plugins.el ---                       -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2017  Abelardo Jara-Berrocal

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


;; Simple directory explorer. It also works as a generic tree explore library
(use-package direx
  :after dired
  :load-path (lambda () (expand-file-name "direx/" user-emacs-directory))
  :bind (:map dired-mode-map
              ("b"       . direx:jump-to-directory)
              :map direx:direx-mode-map
              ("b"       . dired-jump)
              ([mouse-1] . direx:mouse-2)
              ([mouse-3] . direx:mouse-1))
  :config (setq direx:closed-icon "+ "
                direx:leaf-icon   "| "
                direx:open-icon   "> "))

(use-package direx-project
  :after direx
  :load-path (lambda () (expand-file-name "direx/" user-emacs-directory)))

;; Enable copying and pasting files
(use-package dired-ranger
  :defer t
  :after dired
  :load-path (lambda () (expand-file-name "dired-hacks/" user-emacs-directory))
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)))

;; Highlight dired buffer with K-shell coloring
(use-package dired-k
  :defer t
  :after dired
  :load-path (lambda () (expand-file-name "dired-k/" user-emacs-directory))
  :bind (:map dired-mode-map
              ("K" . dired-k))
  :commands (dired-k dired-k-no-revert)
  :init (progn
          (add-hook 'dired-initial-position-hook 'dired-k)
          (add-hook 'dired-after-readin-hook 'dired-k-no-revert)))

;; Display file icons in dired
(use-package dired-icon
  :defer t
  :after dired
  :load-path (lambda () (expand-file-name "dired-icon/" user-emacs-directory))
  :if (display-graphic-p)
  :commands (dired-icon-mode)
  :init (add-hook 'dired-mode-hook 'dired-icon-mode))

;; Facility to see images inside dired
(use-package image-dired
  :disabled t ;; issue with emacs server
  :after dired
  :config (progn
            (setq image-dired-cmd-create-thumbnail-options
                  (replace-regexp-in-string "-strip" "-auto-orient -strip" image-dired-cmd-create-thumbnail-options)
                  image-dired-cmd-create-temp-image-options
                  (replace-regexp-in-string "-strip" "-auto-orient -strip" image-dired-cmd-create-temp-image-options))))

(use-package helm-dired-history
  :defer t
  :after (dired savehist helm)
  :bind (:map dired-mode-map
              ("," . helm-dired-history-view))
  :load-path (lambda () (expand-file-name "helm-dired-history/" user-emacs-directory))
  :config (progn
            (savehist-mode 1)
            (add-to-list 'savehist-additional-variables 'helm-dired-history-variable)))

;; Preview files in dired
(use-package peep-dired
  :after dired
  :load-path (lambda () (expand-file-name "peep-dired/" user-emacs-directory))
  :defer t ;; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

;; All the icons, dired plugin
(use-package all-the-icons-dired
  :defer t
  :if (display-graphic-p)
  :after (dired all-the-icons)
  :diminish all-the-icons-dired-mode
  :load-path (lambda () (expand-file-name "all-the-icons-dired/" user-emacs-directory))
  :commands all-the-icons-dired-mode
  :config (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; neotree side bar
(use-package neotree
  :disabled t
  :defer t
  :commands (neotree-toggle)
  :bind (:map ctl-x-map
              ("C-t" . neotree-toggle)
              :map neotree-mode-map
              (("<C-return>" . neotree-change-root)
               ("C"          . neotree-change-root)
               ("c"          . neotree-create-node)
               ("+"          . neotree-create-node)
               ("d"          . neotree-delete-node)
               ("r"          . neotree-rename-node)))
  :load-path (lambda () (expand-file-name "neotree/" user-emacs-directory))
  :config (progn
            ;; every time when the neotree window is
            ;; opened, it will try to find current
            ;; file and jump to node.
            (setq-default neo-smart-open t)
            ;; Don't allow neotree to be the only open window
            (setq-default neo-dont-be-alone t)))

;; Sunrise Commander
(use-package sunrise-commander
  :disabled t
  :defer t
  :commands (sunrise sr-dired cb-sunrise-commander/dired-this-dir)
  :load-path (lambda () (expand-file-name "sunrise-commander/" user-emacs-directory))
  :bind (("C-;" . cb-sunrise-commander/dired-this-dir)
         :map sr-mode-map
         ("J" . sr-goto-dir)
         ("j" . dired-next-line)
         ("k" . dired-previous-line)
         ("n" . sr-goto-dir)
         ("C-k" . dired-do-kill-lines))
  :init (progn
          (defun cb-sunrise-commander/dired-this-dir ()
            (interactive)
            (sr-dired default-directory)))
  :config (progn
            (setq sr-windows-locked nil)
            (setq sr-cursor-follows-mouse nil)
            (setq sr-windows-default-ratio 60)
            (setq sr-use-commander-keys nil)))

(provide 'setup-dired-plugins)
;;; setup-dired-plugins.el ends here
