;;; lawlist-scroll-mode.el ---                       -*- lexical-binding: t; -*-

;; Copyright (C) 2016  abelardo.jara-berrocal

;; Author: abelardo.jara-berrocal <ajaraber@plxcj9063.pdx.intel.com>
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

(provide 'lawlist-scroll-mode)
;;; lawlist-scroll-mode.el ends here
