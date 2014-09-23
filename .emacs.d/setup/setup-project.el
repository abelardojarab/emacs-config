;;; setup-project.el ---

;; Copyright (C) 2014  abelardo.jara-berrocal

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

;; Helm
(add-to-list 'load-path "~/.emacs.d/helm")
(require 'helm-config)

;; Helm ls git
(add-to-list 'load-path "~/.emacs.d/helm-ls-git")
(require 'helm-ls-git)
(global-set-key (kbd "C-<f6>") 'helm-ls-git-ls)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)

;; Async
(add-to-list 'load-path "~/.emacs.d/async")
(when (require 'dired-aux)
  (require 'dired-async))

;; iMenu
(set-default 'imenu-auto-rescan t)
(add-hook 'lisp-mode-hook
          (lambda ()
            (setq imenu-create-index-function 'imenu-example--create-lisp-index)
            (setq imenu-generic-expression scheme-imenu-generic-expression)))

(add-hook 'emacs-lisp-mode-hook 'imenu-add-menubar-index)
(add-hook 'lisp-mode-hook 'imenu-add-menubar-index)
(add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'reftex-mode-hook 'imenu-add-menubar-index)
(add-hook 'latex-mode-hook 'imenu-add-menubar-index)
(add-hook 'org-mode-hook 'imenu-add-menubar-index)
(add-hook 'python-mode-hook 'imenu-add-menubar-index)

;; Enable which-function-mode for selected major modes
(setq which-func-modes '(ecmascript-mode python-mode emacs-lisp-mode lisp-mode
                                         c-mode c++-mode makefile-mode sh-mode))
(which-function-mode t)
(add-hook 'js2-mode-hook
          (lambda () (which-function-mode t)))
(add-hook 'python-mode-hook
          (lambda () (which-function-mode t)))
(add-hook 'c-mode-common-hook
          (lambda () (which-function-mode t)))
(add-hook 'lisp-mode-hook
          (lambda () (which-function-mode t)))
(add-hook 'emacs-lisp-mode-hook
          (lambda () (which-function-mode t)))

;; Project management
(add-to-list 'load-path "~/.emacs.d/ack-and-a-half")
(add-to-list 'load-path "~/.emacs.d/projectile")
(add-to-list 'load-path "~/.emacs.d/perspective")
(require 'ack-and-a-half)
;; (require 'perspective)
(require 'projectile)
(setq projectile-cache-file "~/.emacs.cache/projectile.cache")
(setq projectile-known-projects-file "~/.emacs.cache/projectile-bookmarks.eld")
(setq projectile-indexing-method 'git)
(setq projectile-enable-caching t)
(setq projectile-keymap-prefix (kbd "C-c C-p"))
(setq projectile-remember-window-configs t)
(projectile-global-mode t)

;; Automatically invoke magit-status
(setq projectile-switch-project-action 'projectile-dired)

(provide 'setup-project)
;;; setup-project.el ends here
