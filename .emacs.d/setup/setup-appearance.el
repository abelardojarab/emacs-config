;;; setup-appearance.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2021  Abelardo Jara-Berrocal

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

;; Disable tool-bar and scroll-bar
(when (display-graphic-p)
  (tool-bar-mode -1)
  (set-scroll-bar-mode 'right)
  (scroll-bar-mode -1)
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode 0)))

;; Toggle menu bar
(use-package menu-bar
  :demand t
  :config (menu-bar-mode 1)
  :bind ([S-f10] . menu-bar-mode))

;; Fringe configuration
(use-package fringe
  :config (if (display-graphic-p)
              (fringe-mode '(28 . 12))))

;; Enable tooltips
(use-package tooltip
  :defer t
  :if (display-graphic-p)
  :commands tooltip-mode
  :hook (prog-mode . tooltip-mode)
  :custom (tooltip-delay 1)
  :config (if (equal system-type 'gnu/linux)
              (setq x-gtk-use-system-tooltips t)))

;; Marker if the line goes beyond the end of the screen (arrows)
(use-package simple
  :demand t
  :commands (global-visual-line-mode
             visual-line-mode
             toggle-truncate-lines)
  :diminish visual-line-mode
  :custom ((visual-line-fringe-indicators '(nil right-curly-arrow))
           (line-spacing                  2))
  :hook ((prog-mode text-mode) . my/enable-truncate-lines)
  :preface (defun my/enable-truncate-lines ()
             (visual-line-mode -1)
             (toggle-truncate-lines t)
             (setq truncate-lines t))
  :config (defun toggle-truncate-lines (&optional arg)
            "Toggle truncating of long lines for the current buffer.
When truncating is off, long lines are folded.
With prefix argument ARG, truncate long lines if ARG is positive,
otherwise fold them.  Note that in side-by-side windows, this
command has no effect if `truncate-partial-width-windows' is
non-nil."
            (interactive "P")
            (setq truncate-lines
                  (if (null arg)
                      (not truncate-lines)
                    (> (prefix-numeric-value arg) 0)))
            (force-mode-line-update)
            (unless truncate-lines
              (let ((buffer (current-buffer)))
                (walk-windows (lambda (window)
                                (if (eq buffer (window-buffer window))
                                    (set-window-hscroll window 0)))
                              nil t)))
            t))

;; Get centered text when visual-line-mode is on
(use-package visual-fill-column
  :defer t
  :commands visual-fill-column-mode
  :custom ((visual-fill-column-center-text             t)
           (visual-fill-column-fringes-outside-margins nil))
  :config (setq-default visual-fill-column-width
                        ;; take Emacs 26 line numbers into account
                        (+ (if (boundp 'display-line-numbers) 6 0)
                           fill-column)))

;; Faster line numbers
(use-package linum-ex
  :demand t
  :commands linum-mode
  :init (dolist (hook my/linum-modes)
          (add-hook hook (lambda ()
                           ;; turn off `linum-mode' when there are more than 5000 lines
                           (if (and (> (buffer-size)
                                       (* 5000 80)))
                               (linum-mode -1)
                             (if (boundp 'display-line-numbers)
                                 (display-line-numbers-mode 1)
                               (linum-mode 1))))))
  :config (defadvice linum-update-window (around linum-dynamic activate)
            (let* ((w (length (number-to-string
                               (count-lines (point-min) (point-max)))))
                   (linum-format (concat " %" (number-to-string w) "d ")))
              ad-do-it)))

;; Line number (alternative to linum-ex)
(use-package nlinum
  :defer t
  :commands nlinum-mode)

;; Highlight line number
(use-package hlinum
  :defer t
  :commands hlinum-activate)

;; Adaptive scrollbar
(use-package lawlist-scroll-mode
  :defer t
  :if (display-graphic-p)
  :commands lawlist-scroll-bar-mode
  :diminish (lawlist-scroll-bar-mode)
  :init (dolist (hook my/linum-modes)
          (add-hook hook (lambda ()
                           (unless (and (> (buffer-size)
                                           (* 5000 80)))
                             (lawlist-scroll-bar-mode 1))))))

;; All the icons
(use-package all-the-icons
  :defer t
  :if (display-graphic-p)
  :commands all-the-icons-insert)

;; Better characters
(unless standard-display-table
  (setq standard-display-table (make-display-table)))
(when (display-graphic-p)
  (set-display-table-slot standard-display-table 0 ?\s)
  (set-display-table-slot standard-display-table
                          'selective-display (string-to-vector " ⮷ ")))

;; Display vertical line
(use-package fill-column-indicator
  :defer t
  :if (display-graphic-p)
  :commands (fci-mode turn-on-fci-mode)
  :custom ((fci-handle-truncate-lines t)
           (fci-rule-width            1))
  :config (progn
            (dolist (hook my/linum-modes)
              (add-hook hook (lambda ()
                               (turn-on-fci-mode))))

            (defun my/fci-enabled-p ()
              (and (boundp 'fci-mode) fci-mode))

            (defvar my/fci-mode-suppressed nil)
            (defadvice popup-create (before suppress-fci-mode activate)
              "Suspend fci-mode while popups are visible"
              (let ((fci-enabled (my/fci-enabled-p)))
                (when fci-enabled
                  (set (make-local-variable 'my/fci-mode-suppressed) fci-enabled)
                  (turn-off-fci-mode))))

            (defadvice popup-delete (after restore-fci-mode activate)
              "Restore fci-mode when all popups have closed"
              (when (and my/fci-mode-suppressed
                         (null popup-instances))
                (setq my/fci-mode-suppressed nil)
                (turn-on-fci-mode)))

            (defadvice enable-theme (after recompute-fci-face activate)
              "Regenerate fci-mode line images after switching themes"
              (dolist (buffer (buffer-list))
                (with-current-buffer buffer
                  (turn-on-fci-mode))))

            ;; `fci-mode' needs to be disabled/enabled around the
            ;; `shell-command-on-region' command too.
            (defun my/shell--fci-disable-temporarily (orig-fun &rest args)
              "Disable `fci-mode' before calling ORIG-FUN; re-enable afterwards."
              (let ((fci-was-initially-on (when fci-mode
                                            (prog1
                                                fci-mode
                                              (fci-mode -1)))))
                (prog1
                    (apply orig-fun args)
                  (when fci-was-initially-on
                    (fci-mode 1)))))
            (advice-add 'shell-command-on-region :around #'my/shell--fci-disable-temporarily)

            (defun my/fci-redraw-frame-all-buffers ()
              "Redraw the fill-column rule in all buffers on the selected frame.
Running this function after changing themes updates the fci rule color in
all the buffers."
              (interactive)
              (let ((bufs (delete-dups (buffer-list (selected-frame)))))
                (dolist (buf bufs)
                  (with-current-buffer buf
                    (when fci-mode
                      (fci-delete-unneeded)
                      (fci-make-overlay-strings)
                      (fci-update-all-windows t))))))))

;; Just like the previous package, this one is also subtle.
(use-package column-enforce-mode
  :defer t
  :commands column-enforce-mode
  :diminish column-enforce-mode
  :custom (column-enforce-column 120)
  :hook (prog-mode . column-enforce-mode))

;; Visually highlight the selected buffer
(use-package dimmer
  :unless noninteractive
  :defer 10
  :custom (dimmer-fraction 0.25)
  :commands dimmer-mode
  :config (dimmer-mode t))

;; VSCode-style icons
(use-package vscode-icon
  :commands (vscode-icon-for-file))

;; Modern fringes
(use-package modern-fringes
  :disabled t
  :if (display-graphic-p)
  :hook (prog-mode . modern-fringes-mode)
  :config (progn
            (defun modern-fringes--init ()
              "Apply all of the modern fringe bitmaps.  Make your fringes look cool!"
              (interactive)
              (let ((mf-right-arrow [128 192 224 240 248 252 254 252 248 240 224 192 128])
                    (mf-left-arrow [1 3 7 15 31 63 127 63 31 15 7 3 1])
                    (mf-right-curly-arrow [64 32 16 8 68 72 80 96 124])
                    (mf-left-curly-arrow [2 4 8 16 34 18 10 6 62])
                    (mf-right-debug-arrow [128 64 224 16 248 4 254 4 248 16 224 64 128])
                    (mf-left-debug-arrow [1 2 7 8 31 32 127 32 31 8 7 2 1]))
                (define-fringe-bitmap 'right-arrow mf-right-arrow nil nil 'center)
                (define-fringe-bitmap 'left-arrow mf-left-arrow nil nil 'center)
                (define-fringe-bitmap 'right-curly-arrow mf-right-curly-arrow nil nil 'center)
                (define-fringe-bitmap 'left-curly-arrow mf-left-curly-arrow nil nil 'center)
                (define-fringe-bitmap 'right-triangle mf-right-debug-arrow nil nil 'center)
                (define-fringe-bitmap 'left-triange mf-left-debug-arrow nil nil 'center)))
            (modern-fringes-invert-arrows)))

;; Ligatures
(use-package fira-code-mode
  :disabled t
  :if (display-graphic-p)
  :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")) ;; List of ligatures to turn off
  :hook (prog-mode . fira-code-mode))

;; Do not show blank lines at the end of the file:
(setq-default indicate-empty-lines nil)
(setq-default indicate-buffer-boundaries 'left)

(provide 'setup-appearance)
;;; setup-appearance.el ends here
