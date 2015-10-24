;;; setup-project.el ---

;; Copyright (C) 2014, 2015  abelardo.jara-berrocal

;; Author: abelardo.jara-berrocal <ajaraber@plxc25288.pdx.intel.com>
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

;; Project management
(add-to-list 'load-path "~/.emacs.d/ack-and-a-half")
(add-to-list 'load-path "~/.emacs.d/perspective")
(add-to-list 'load-path "~/.emacs.d/projectile")
(require 'ack-and-a-half)
(require 'projectile)
(setq projectile-cache-file "~/.emacs.cache/projectile.cache")
(setq projectile-known-projects-file "~/.emacs.cache/projectile-bookmarks.eld")
(setq projectile-enable-caching t)
(setq projectile-keymap-prefix (kbd "C-c C-p"))
(setq projectile-remember-window-configs t)
(unless (string-equal system-type "windows-nt")
  (setq projectile-indexing-method 'git)) ;; unless
(projectile-global-mode t)

;; Projectile integration with speedbar
(add-to-list 'load-path "~/.emacs.d/projectile-speedbar")
(require 'projectile-speedbar)

;; Org projectile
(add-to-list 'load-path "~/.emacs.d/org-projectile")
(require 'org-projectile)
(setq org-projectile:projects-file "~/workspace/Documents/projects.org")
(add-to-list 'org-capture-templates (org-projectile:project-todo-entry))

;; cmake autocomplete/flycheck
(add-to-list 'load-path "~/.emacs.d/cpputils-cmake")
(require 'cpputils-cmake)

;; cmake IDE
(add-to-list 'load-path "~/.emacs.d/cmake-ide")
(require 'cmake-ide)
(cmake-ide-setup)

(provide 'setup-project)
;;; setup-project.el ends here
