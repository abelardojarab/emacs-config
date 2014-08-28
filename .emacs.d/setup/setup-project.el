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
(projectile-global-mode t)
(setq projectile-cache-file "~/.emacs.cache/projectile.cache")
(setq projectile-known-projects-file "~/.emacs.cache/projectile-bookmarks.eld")
(setq projectile-enable-caching t)
(setq projectile-keymap-prefix (kbd "C-c C-p"))

;; Code Browser
(setq stack-trace-on-error t)
(setq after-find-file-from-revert-buffer t)
(add-to-list 'load-path "~/.emacs.d/ecb")



;; Enable ecb
(require 'ecb)
(setq ecb-show-sources-in-directories-buffer 'always)
(set-face-foreground 'ecb-default-general-face "#ffffff")
(setq ecb-tip-of-the-day nil)
(setq ecb-auto-compatibility-check nil)
(if (ecb--semantic-active-p)
    (ecb-update-methods-buffer--internal nil nil t)
  (ecb-rebuild-methods-buffer-for-non-semantic))

;; reference path-to-ecb/ecb-layout-defs.el
(ecb-layout-define "leftright-sa-m" left-right
  "This function creates the following layout:

   --------------------------------------------------------------
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |  Sources     |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |--------------|             Edit              |  Methods    |
   |              |                               |             |
   |              |                               |             |
   |  Analyse     |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   --------------------------------------------------------------
   |                                                            |
   |                    Compilation                             |
   |                                                            |
   --------------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (ecb-set-sources-buffer)
  (ecb-split-ver 0.4)
  (ecb-set-analyse-buffer)
  (select-window (next-window (next-window)))
  (ecb-set-methods-buffer)
  (select-window (previous-window (previous-window (selected-window) 0) 0)))

(when (not (memq 'leftright-sa-m ecb-layout-window-sizes))
  (add-to-list 'ecb-layout-window-sizes
               '("leftright-sa-m"
                 (0.10 . 0.3)
                 (0.10 . 0.4)
                 (0.10 . 0.7))))

(ecb-layout-define "left-speedbar" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |   Speedbar   |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place. "
  (ecb-set-speedbar-buffer)
  (select-window (next-window)))


(when (not (memq 'left-speedbar ecb-layout-window-sizes))
  (add-to-list 'ecb-layout-window-sizes
               '("left-speedbar"
                 (0.3 . 0.8)
                 )))

(provide 'setup-project)
;;; setup-project.el ends here
