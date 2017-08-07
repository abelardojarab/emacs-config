;;; setup-projectile.el ---                          -*- lexical-binding: t; -*-

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

(use-package projectile
  :demand t
  :diminish projectile-mode
  :defines (projectile-ignored-projects
            projectile-enable-caching)
  :commands (projectile-global-mode
             projectile-compile-project
             projectile-find-file)
  :load-path (lambda () (expand-file-name "projectile/" user-emacs-directory))
  :init (projectile-global-mode)
  :bind (("C-x C-m" . projectile-compile-project)
         ("C-x C-g" . projectile-find-file))
  :config (progn
            (add-to-list 'projectile-project-root-files "configure.ac")
            (add-to-list 'projectile-project-root-files ".clang_complete")
            (add-to-list 'projectile-project-root-files ".clang_complete.in")
            (add-to-list 'projectile-project-root-files "AndroidManifest.xml")

            (setq projectile-known-projects-file "~/.emacs.cache/projectile-bookmarks.eld"
                  projectile-cache-file "~/.emacs.cache/projectile.cache"
                  projectile-enable-caching t
                  projectile-sort-order 'recently-active
                  projectile-indexing-method 'alien
                  projectile-globally-ignored-files (quote ("TAGS" "*.log" "*DS_Store" "node-modules")))))

(use-package helm-projectile
  :demand t
  :after (projectile helm-config)
  :load-path (lambda () (expand-file-name "helm-projectile/" user-emacs-directory))
  :config (progn
            (defun helm-projectile-on ()
              "Turn on helm-projectile key bindings."
              (interactive)
              (helm-projectile-toggle 1))

            (defun helm-projectile-off ()
              "Turn off helm-projectile key bindings."
              (interactive)
              (helm-projectile-toggle -1))

            (defun my/projectile-setup ()
              (helm-projectile-on)
              (setq projectile-switch-project-action 'helm-projectile)
              (setq projectile-completion-system 'helm))
            (my/projectile-setup)
            (add-hook 'projectile-find-file-hook 'my/projectile-setup)
            (add-hook 'projectile-mode-hook 'my/projectile-setup)))

(provide 'setup-projectile)
;;; setup-projectile.el ends here
