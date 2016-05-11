;;; setup-windows.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Abelardo Jara

;; Author: Abelardo Jara <abelardojara@Abelardos-MacBook-Pro.local>
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
  :disabled t ;; caused issues with mode-icons and powerline
  :load-path (lambda () (expand-file-name "window-purpose/" user-emacs-directory))
  :init (progn
          ;; overriding `purpose-mode-map' with empty keymap, so it doesn't conflict
          ;; with original `C-x C-f', `C-x b', etc. and `semantic' key bindings. must
          ;; be done before `window-purpose' is loaded
          (setq purpose-mode-map (make-sparse-keymap)))
  :config (progn
            (require 'window-purpose-x)
            (setq purpose-preferred-prompt 'helm)
            (defalias 'window-purpose/helm-mini-ignore-purpose
              (without-purpose-command #'helm-mini)
              "Same as `helm-mini', but disable window-purpose while this command executes.")
            (purpose-mode)

            ;; Enable for popwin compatibility
            (purpose-x-popwin-setup)

            ;; when killing a purpose-dedicated buffer that is displayed in a window,
            ;; ensure that the buffer is replaced by a buffer with the same purpose
            ;; (or the window deleted, if no such buffer)
            (purpose-x-kill-setup)))

(provide 'setup-windows)
;;; setup-windows.el ends here
