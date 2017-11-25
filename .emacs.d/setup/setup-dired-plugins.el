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

;; neotree side bar
(use-package neotree
  :defer t
  :commands (neotree-toggle)
  :bind (:map ctl-x-map
              ("t" . neotree-toggle)
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

(provide 'setup-dired-plugins)
;;; setup-dired-plugins.el ends here
