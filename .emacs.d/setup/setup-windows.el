;;; setup-windows.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015, 2016  abelardo.jara-berrocal

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

;; http://stackoverflow.com/questions/25469319/prevent-emacs-from-opening-a-buffer-in-a-new-frame
(setq menu-bar-select-buffer-function 'switch-to-buffer)

;; Always split horizontally
(setq split-width-threshold 3000)
(setq split-height-threshold nil)

;; Helper function for horizontal splitting
(defun split-horizontally-for-temp-buffers ()
  "Split the window horizontally for temp buffers."
  (when (and (one-window-p t)
             (not (active-minibuffer-window)))
    (split-window-horizontally)))
(add-hook 'temp-buffer-setup-hook 'split-horizontally-for-temp-buffers)

;; horizontal splitting - when opening files or buffers with C-x4b or C-x4f
(defun split-window-prefer-horizonally (window)
  "If there's only one window (excluding any possibly active
         minibuffer), then split the current window horizontally."
  (if (and (one-window-p t)
           (not (active-minibuffer-window)))
      (let ((split-height-threshold nil))
        (split-window-sensibly window))
    (split-window-sensibly window)))
(setq split-window-preferred-function 'split-window-prefer-horizonally)

;; Avoid multiple windows when opening multiple files
;; http://stackoverflow.com/questions/1144729/how-do-i-prevent-emacs-from-horizontally-splitting-the-screen-when-opening-multip
;; (add-hook 'window-setup-hook 'delete-other-windows)

;; Manage popup windows
(use-package popwin
  :defer t
  :commands popwin-mode
  :load-path (lambda () (expand-file-name "popwin/" user-emacs-directory))
  :config (progn
            (defvar popwin:special-display-config-backup popwin:special-display-config)
            (setq display-buffer-function 'popwin:display-buffer)

            ;; basic
            (push '("*Help*" :stick t :noselect t) popwin:special-display-config)
            (push '("*Cedet Global*" :stick t :noselect t) popwin:special-display-config)

            ;; magit
            (push '("*magit-process*" :stick t) popwin:special-display-config)

            ;; dictionaly
            (push '("*dict*" :stick t) popwin:special-display-config)
            (push '("*sdic*" :stick t) popwin:special-display-config)

            ;; Elisp
            (push '("*ielm*" :stick t) popwin:special-display-config)
            (push '("*eshell pop*" :stick t) popwin:special-display-config)

            ;; python
            (push '("*Python*"   :stick t) popwin:special-display-config)
            (push '("*Python Help*" :stick t :height 20) popwin:special-display-config)
            (push '("*jedi:doc*" :stick t :noselect t) popwin:special-display-config)

            ;; ecb
            (push '("*ECB History*" :position left :width 0.3 :stick t)
                  popwin:special-display-config)
            (push '("*ECB Source*" :position left :width 0.3 :stick t)
                  popwin:special-display-config)
            (push '("*ECB History*" :position right :width 0.3 :stick t)
                  popwin:special-display-config)

            ;; sgit
            (push '("*sgit*" :position right :width 0.5 :stick t)
                  popwin:special-display-config)

            ;; git-gutter
            (push '("*git-gutter:diff*" :width 0.5 :stick t)
                  popwin:special-display-config)

            ;; es-results-mode
            (push '(es-result-mode :stick t :width 0.5)
                  popwin:special-display-config)

            ;; direx
            (push '(direx:direx-mode :position left :width 40 :dedicated t)
                  popwin:special-display-config)

            (push '("*Occur*" :stick t) popwin:special-display-config)

            ;; compilation
            (push '("*compilation*" :stick t :height 30)
                  popwin:special-display-config)

            ;; org-mode
            (push '("*Org tags*" :stick t :height 30)
                  popwin:special-display-config)

            ;; Completions
            (push '("*Completions*" :stick t :noselect t) popwin:special-display-config)

            ;; ggtags
            (push '("*ggtags-global*" :stick t :noselect t :height 30) popwin:special-display-config)

            ;; async shell commands
            (push '("*Async Shell Command*" :stick t) popwin:special-display-config)

            ;; helm
            (push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
            (push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config)

            ;; popwin conflicts with ecb
            (popwin-mode -1)))

;; Window purpose
(use-package window-purpose
  :defer t
  :commands (purpose-mode purpose-switch-buffer purpose-pop-buffer)
  :load-path (lambda () (expand-file-name "window-purpose/" user-emacs-directory))
  :init (progn
          ;; overriding `purpose-mode-map' with empty keymap, so it doesn't conflict
          ;; with original `C-x C-f', `C-x b', etc. and `semantic' key bindings. must
          ;; be done before `window-purpose' is loaded
          (setq purpose-mode-map (make-sparse-keymap)))
  :config (progn
            ;; Enable purpose mode
            (purpose-mode)

            ;; Configuration
            (setq purpose-user-name-purposes
                  '(("*ag*"               . search)))

            (setq purpose-user-regexp-purposes
                  '(("^\\*elfeed"         . admin)))

            (setq purpose-user-mode-purposes
                  '((
                     (circe-chat-mode     . comm)
                     (circe-query-mode    . comm)
                     (circe-lagmon-mode   . comm)
                     (circe-server-mode   . comm)

                     (ess-mode            . edit)
                     (gitconfig-mode      . edit)
                     (conf-xdefaults-mode . edit)
                     (inferior-ess-mode   . interactive)

                     (mu4e-main-mode      . admin)
                     (mu4e-view-mode      . admin)
                     (mu4e-about-mode     . admin)
                     (mu4e-headers-mode   . admin)
                     (mu4e-compose-mode   . edit)

                     (pdf-view-mode       . view)
                     (doc-view-mode       . view))))

            (purpose-compile-user-configuration)

            ;; Extra configuration
            (require 'window-purpose-x)

            ;; Single window magit
            (purpose-x-magit-single-on)

            ;; when killing a purpose-dedicated buffer that is displayed in a window,
            ;; ensure that the buffer is replaced by a buffer with the same purpose
            ;; (or the window deleted, if no such buffer)
            (purpose-x-kill-setup)

            ;; Prefer helm
            (setq purpose-preferred-prompt 'helm)))

;; Helm interface to purpose
(use-package helm-purpose
  :after (helm window-purpose)
  :commands (helm-purpose-switch-buffer-with-purpose helm-purpose-switch-buffer-with-some-purpose)
  :load-path (lambda () (expand-file-name "helm-purpose/" user-emacs-directory)))

;; Resize windows
(use-package resize-window
  :defer t
  :commands resize-window
  :load-path (lambda () (expand-file-name "resize-window/" user-emacs-directory)))

;; Perspective
(use-package perspective
  :defer t
  :commands persp-mode
  :load-path (lambda () (expand-file-name "perspective/" user-emacs-directory))
  :config (persp-mode))

;; Switch window
(use-package switch-window
  :defer t
  :bind (:map ctl-x-map
              ("o" . switch-window))
  :if (display-graphic-p)
  :commands switch-window
  :load-path (lambda () (expand-file-name "switch-window/" user-emacs-directory)))

;; Window numbering
(use-package window-numbering
  :load-path (lambda () (expand-file-name "window-numbering/" user-emacs-directory))
  :config (window-numbering-mode 1))

(provide 'setup-windows)
;;; setup-windows.el ends here
