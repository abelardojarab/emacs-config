;;; setup-mouse.el ---                               -*- lexical-binding: t; -*-

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

;; Enable mouse support
(use-package mouse
  :unless (display-graphic-p)
  :config (progn
            (xterm-mouse-mode t)
            (defun track-mouse (e))
            (setq mouse-sel-mode t)))

(use-package pixel-scroll
  :if (fboundp 'pixel-scroll-mode)
  :config (pixel-scroll-mode 1))

;; Do not copy region use mouse dragging
(setq mouse-drag-copy-region nil)

;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)

;; Mouse wheel scroll support
(mouse-wheel-mode t)

;; scroll window under mouse
(setq mouse-wheel-follow-mouse 't)

;; scroll one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)

;; Zoom in/out like feature, with mouse wheel
(global-unset-key (kbd "<C-wheel-up>"))
(global-unset-key (kbd "<C-wheel-down>"))
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

;; Get the scroll wheel to work
(global-set-key [(shift button5)] '(lambdas () (interactive) (scroll-up-line)))
(global-set-key [(shift button4)] '(lambda () (interactive) (scroll-down-line)))
(global-set-key [(control button5)] 'text-scale-decrease)
(global-set-key [(control button4)] 'text-scale-increase)

(global-set-key [(shift mouse-5)] '(lambda () (interactive) (scroll-up-line)))
(global-set-key [(shift mouse-4)] '(lambda () (interactive) (scroll-down-line)))
(global-set-key [(control mouse-5)] 'text-scale-decrease)
(global-set-key [(control mouse-4)] 'text-scale-increase)

;; Right click mouse
(use-package mouse3
  :init (global-unset-key (kbd "<mouse-3>"))
  :bind ("<mouse-3>" . mouse3-popup-menu)
  :config (defalias 'mouse3-region-popup-menu 'mouse3-popup-menu))

;; Cancel minibuffer operation if you click outside
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook #'stop-using-minibuffer)

(provide 'setup-mouse)
;;; setup-mouse.el ends here
