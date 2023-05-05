;;; setup-projectile.el ---                          -*- lexical-binding: t; -*-

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

(use-package projectile
  :demand t
  :diminish projectile-mode
  :defines (projectile-ignored-projects
            projectile-enable-caching)
  :commands (projectile-global-mode
             projectile-compile-project
             projectile-find-file
             projectile-project-root
             projectile-mode)
  :init (setq projectile-known-projects-file
              (concat (file-name-as-directory
                       my/emacs-cache-dir) "projectile-bookmarks.eld"))
  :hook (on-first-buffer . projectile-mode)
  :custom ((projectile-mode-line-prefix "")
           (projectile-sort-order       'recentf)
           (projectile-use-git-grep     t))
  :bind (("C-x C-m" . projectile-compile-project)
         ("C-x C-g" . projectile-find-file))
  :config (progn
            (setq projectile-mode-line
                  '(:eval (format " Projectile[%s]"
                                  (projectile-project-name))))

            (add-to-list 'projectile-project-root-files "configure.ac")
            (add-to-list 'projectile-project-root-files ".clang_complete")
            (add-to-list 'projectile-project-root-files ".clang_complete.in")
            (add-to-list 'projectile-project-root-files "AndroidManifest.xml")

            ;; Use the faster searcher to handle project files: ripgrep `rg'.
            (when (and (not (executable-find "fd"))
                       (executable-find "rg"))
              (setq projectile-generic-command
                    (let ((rg-cmd ""))
                      (dolist (dir projectile-globally-ignored-directories)
                        (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
                      (concat "rg -0 --files --color=never --hidden" rg-cmd))))

            ;; Support Perforce project
            (let ((val (or (getenv "P4CONFIG") ".p4config")))
              (add-to-list 'projectile-project-root-files-bottom-up val))

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

;; Integration with ripgrep
(use-package projectile-ripgrep
  :disabled t
  :if (executable-find "rg")
  :after projectile
  :commands projectile-ripgrep)

(provide 'setup-projectile)
;;; setup-projectile.el ends here
