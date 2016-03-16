;;; setup-appearance.el ---

;; Copyright (C) 2014, 2015, 2016  abelardo.jara-berrocal

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

;; Smoother scrolling
(setq redisplay-dont-pause nil
      scroll-margin 20
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)

;; Optimization
(setq-default bidi-display-reordering nil)

;; Do not redraw entire frame after suspending.
(setq no-redraw-on-reenter t)

;; Modify toggle truncate lines to avoid messages
(defun toggle-truncate-lines (&optional arg)
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
  t)

;; Marker if the line goes beyond the end of the screen (arrows)
(global-visual-line-mode 1)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))
(add-hook 'prog-mode-hook
          (lambda ()
            (visual-line-mode -1)
            (toggle-truncate-lines 1)))
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (visual-line-mode -1)
            (toggle-truncate-lines 1)))

;; Syntax coloring
(global-font-lock-mode t)
(global-hi-lock-mode nil)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size (* 512 512))
(defun global-font-lock-mode-check-buffers () nil)

;; Lazy font lock
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-chunk-size 5000
      jit-lock-context-time 0.2
      jit-lock-defer-time .1
      jit-lock-stealth-nice 0.5
      jit-lock-stealth-time 16
      jit-lock-stealth-verbose nil)
(setq-default font-lock-multiline t)

;; Line numbers
(use-package linum
  :config (progn
            (defadvice linum-update-window (around linum-dynamic activate)
              (let* ((w (length (number-to-string
                                 (count-lines (point-min) (point-max)))))
                     (linum-format (concat " %" (number-to-string w) "d ")))
                ad-do-it))))

;; Put a nice title to the window, including filename
(add-hook 'window-configuration-change-hook
          (lambda ()
            (setq frame-title-format
                  (concat
                   invocation-name "@" system-name ": "
                   (replace-regexp-in-string
                    (concat "/home/" user-login-name) "~"
                    (or buffer-file-name "%b"))))))

;; Scrollbar
(when window-system
  (set-scroll-bar-mode 'right)

  ;; Smart scrollbar
  (defvar regexp-always-scroll-bar '("\\.yes" "\\*Scroll-Bar\\*")
    "Regexp matching buffer names that will always have scroll bars.")

  (defvar regexp-never-scroll-bar '("\\.off" "\\.not")
    "Regexp matching buffer names that will never have scroll bars.")

  (add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
  (modify-all-frames-parameters (list (cons 'vertical-scroll-bars nil)))

  (defun lawlist-scroll-bar ()
    (ignore-errors
      (when (window-live-p (get-buffer-window (current-buffer)))
        (redisplay t)
        (cond
         ;; not regexp matches | not narrow-to-region
         ((and
           (not (regexp-match-p regexp-always-scroll-bar (buffer-name)))
           (not (regexp-match-p regexp-never-scroll-bar (buffer-name)))
           (equal (- (point-max) (point-min)) (buffer-size)))
          (cond
           ;; Lines of text are less-than or equal-to window height,
           ;; and scroll bars are present (which need to be removed).
           ((and
             (<= (- (point-max) (point-min)) (- (window-end) (window-start)))
             (equal (window-scroll-bars) `(15 2 right nil)))
            (set-window-scroll-bars (selected-window) 0 'right nil))
           ;; Lines of text are greater-than window height, and
           ;; scroll bars are not present and need to be added.
           ((and
             (> (- (point-max) (point-min)) (- (window-end) (window-start)))
             (not (equal (window-scroll-bars) `(15 2 right nil))))
            (set-window-scroll-bars (selected-window) 15 'right nil))))
         ;; Narrow-to-region is active, and scroll bars are present
         ;; (which need to be removed).
         ((and
           (not (equal (- (point-max) (point-min)) (buffer-size)))
           (equal (window-scroll-bars) `(15 2 right nil)))
          (set-window-scroll-bars (selected-window) 0 'right nil))
         ;; not narrow-to-region | regexp always scroll-bars
         ((and
           (equal (- (point-max) (point-min)) (buffer-size))
           (regexp-match-p regexp-always-scroll-bar (buffer-name)))
          (set-window-scroll-bars (selected-window) 15 'right nil))
         ;; not narrow-to-region | regexp never scroll-bars
         ((and
           (equal (- (point-max) (point-min)) (buffer-size))
           (regexp-match-p regexp-never-scroll-bar (buffer-name)))
          (set-window-scroll-bars (selected-window) 0 'right nil))))))

  (define-minor-mode lawlist-scroll-bar-mode
    "This is a custom scroll bar mode."
    :lighter " sc"
    (if lawlist-scroll-bar-mode
        (progn
          (add-hook 'post-command-hook 'lawlist-scroll-bar nil t))
      (remove-hook 'post-command-hook 'lawlist-scroll-bar t)
      (remove-hook 'change-major-mode-hook 'lawlist-scroll-bar t)
      (remove-hook 'window-configuration-change-hook 'lawlist-scroll-bar t)))

  (define-globalized-minor-mode global-lawlist-scroll-bar-mode
    lawlist-scroll-bar-mode lawlist-scroll-bar-on)

  (defun lawlist-scroll-bar-on ()
    (unless (minibufferp)
      (lawlist-scroll-bar-mode 1)))

  (global-lawlist-scroll-bar-mode))

(provide 'setup-appearance)
;;; setup-appearance.el ends here
