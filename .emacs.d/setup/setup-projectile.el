;;; setup-projectile.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2017, 2018  Abelardo Jara-Berrocal

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

(use-package projectile
  :demand t
  :diminish projectile-mode
  :defines (projectile-ignored-projects
            projectile-enable-caching)
  :commands (projectile-global-mode
             projectile-compile-project
             projectile-find-file
             projectile-project-root)
  :load-path (lambda () (expand-file-name "projectile/" user-emacs-directory))
  :bind (("C-x C-m" . projectile-compile-project)
         ("C-x C-g" . projectile-find-file))
  :config (progn
            (add-to-list 'projectile-project-root-files "configure.ac")
            (add-to-list 'projectile-project-root-files ".clang_complete")
            (add-to-list 'projectile-project-root-files ".clang_complete.in")
            (add-to-list 'projectile-project-root-files "AndroidManifest.xml")

            (setq projectile-known-projects-file (concat (file-name-as-directory
                                                          my/emacs-cache-dir)
                                                         "projectile-bookmarks.eld")
                  projectile-cache-file          (concat (file-name-as-directory
                                                          my/emacs-cache-dir)
                                                         "projectile.cache")
                  projectile-enable-caching      t
                  projectile-sort-order          'recently-active
                  projectile-indexing-method     'alien
                  projectile-globally-ignored-files (quote ("TAGS" "*.log" "*DS_Store" "node-modules")))
            (projectile-global-mode)))

(provide 'setup-projectile)
;;; setup-projectile.el ends here
