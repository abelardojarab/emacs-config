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

;; Disable tool-bar
(if (display-graphic-p)
    (tool-bar-mode -1))

;; GUI-specific thing
(if (display-graphic-p)
    (setenv "EMACS_GUI" "t"))

;; Dont pause screen refresh
(setq redisplay-dont-pause t)

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
(diminish 'visual-line-mode)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))
(add-hook 'prog-mode-hook
          (lambda ()
            (visual-line-mode -1)
            (toggle-truncate-lines t)
            (setq truncate-lines t)))
(add-hook 'text-mode-hook
          (lambda ()
            (visual-line-mode 1)
            (toggle-truncate-lines -1)
            (setq truncate-lines nil)))

;; Visual fill column
(use-package visual-fill-column
  :defer t
  :load-path (lambda () (expand-file-name "visual-fill-column/" user-emacs-directory)))

;; Just like the previous package, this one is also subtle.
;; It highlights characters that exceed a particular column margin. Very useful while coding.
(use-package column-enforce-mode
  :diminish column-enforce-mode
  :init (setq column-enforce-column 99)
  :load-path (lambda () (expand-file-name "column-enforce-mode/" user-emacs-directory))
  :config (progn
            (add-hook 'prog-mode-hook 'column-enforce-mode)))

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
(use-package lawlist-scroll-mode
  :if (display-graphic-p)
  :init (set-scroll-bar-mode 'right))

(provide 'setup-appearance)
;;; setup-appearance.el ends here
