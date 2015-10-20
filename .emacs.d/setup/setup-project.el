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

;; Helm
(add-to-list 'load-path "~/.emacs.d/helm")
(require 'helm-config)

;; Helm-flyspell
(add-to-list 'load-path "~/.emacs.d/helm-flyspell")
(require 'helm-flyspell)

;; Helm-flycheck
(add-to-list 'load-path "~/.emacs.d/helm-flycheck")
(require 'helm-flycheck)

;; Helm ls git
(add-to-list 'load-path "~/.emacs.d/helm-ls-git")
(require 'helm-ls-git)

;; Helm bm support
(add-to-list 'load-path "~/.emacs.d/helm-bm")
(require 'helm-bm)

;; Helm themes
(add-to-list 'load-path "~/.emacs.d/helm-themes")
(require 'helm-themes)

;; Helm etags plus
(add-to-list 'load-path "~/.emacs.d/helm-etags+")
(require 'helm-etags+)

;; Helm swoop
(add-to-list 'load-path "~/.emacs.d/helm-swoop")
(require 'helm-swoop)

;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
(define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)

;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color nil)

;; ;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)

;; iMenu
(set-default 'imenu-auto-rescan t)
(add-hook 'lisp-mode-hook
          (lambda ()
            (setq imenu-create-index-function 'imenu-example--create-lisp-index)
            (setq imenu-generic-expression scheme-imenu-generic-expression)))

(mapc (lambda (mode)
        (add-hook mode 'imenu-add-menubar-index))
      '(c-mode-hook
        c++-mode-hook
        lisp-mode-hook
        python-mode-hook
        js2-mode-hook
        java-mode-hook
        reftex-mode-hook
        reftex-load-hook
        org-mode-hook))

;; iMenus
(add-to-list 'load-path "~/.emacs.d/imenus")
(autoload 'imenus "imenus" nil t)
(autoload 'imenus-mode-buffers "imenus" nil t)

;; Enable which-function-mode for selected major modes
(setq which-func-modes '(ecmascript-mode emacs-lisp-mode lisp-mode java-mode
                                         c-mode c++-mode makefile-mode sh-mode))
(which-function-mode t)
(mapc (lambda (mode)
        (add-hook mode (lambda () (which-function-mode t))))
      '(c-mode-hook
        c++-mode-hook
        lisp-mode-hook
        python-mode-hook
        js2-mode-hook
        java-mode-hook
        org-mode-hook))

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

;; helm dash
(add-to-list 'load-path "~/.emacs.d/helm-dash")
(require 'helm-dash)
(setq helm-dash-min-length 2)
(setq helm-dash-docsets-path (expand-file-name "~/.emacs.d/docsets"))
(setq helm-dash-common-docsets '("C++" "Qt"))

(defun my/dash-path (docset)
  (if (string= docset "Python_2")
      (concat (concat helm-dash-docsets-path "/") "Python_2.docset")
    (if (string= docset "Qt_4")
        (concat (concat helm-dash-docsets-path "/") "Qt.docset")
      (concat
       (concat
        (concat
         (concat helm-dash-docsets-path "/")
         (nth 0 (split-string docset "_")))) ".docset"))))

(defun my/dash-install (docset)
  (unless (file-exists-p (my/dash-path docset))
    (helm-dash-install-docset docset)))

(defun c-doc-hook ()
  (interactive)
  (setq-local helm-dash-docsets '("C" "C++" "Qt")))
(add-hook 'c-mode-common-hook 'c-doc-hook)

(defun python-doc-hook ()
  (interactive)
  (setq-local helm-dash-docsets '("Python_2")))
(add-hook 'python-mode-hook 'python-doc-hook)

(defun java-doc-hook ()
  (interactive)
  (setq-local helm-dash-docsets '("Java")))
(add-hook 'java-mode-hook 'java-doc-hook)

(defun js2-doc-hook ()
  (interactive)
  (setq-local helm-dash-docsets '("JavaScript" "NodeJS")))
(add-hook 'js2-mode-hook 'js2-doc-hook)

(provide 'setup-project)
;;; setup-project.el ends here
