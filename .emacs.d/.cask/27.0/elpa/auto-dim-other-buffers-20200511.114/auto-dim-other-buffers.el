;;; auto-dim-other-buffers.el --- Makes non-current buffers less prominent -*- lexical-binding: t -*-

;; Copyright 2013 Steven Degutis
;; Copyright 2013-2017 Google Inc.
;; Copyright 2014 Justin Talbott
;; Copyright 2018-2020 Michał Nazarewicz

;; Author: Steven Degutis
;;	Michal Nazarewicz <mina86@mina86.com>
;; Maintainer: Michal Nazarewicz <mina86@mina86.com>
;; URL: https://github.com/mina86/auto-dim-other-buffers.el
;; Package-Version: 20200511.114
;; Version: 1.9.3

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; The `auto-dim-other-buffers-mode' is a global minor mode which
;; makes non-current buffer less prominent making it more apparent
;; which window has a focus.

;; The preferred way to install the mode is by installing a package
;; form MELPA:
;;
;;     M-x package-install RET auto-dim-other-buffers RET

;; Once installed, the mode can be turned on (globally) with:
;;
;;     M-x auto-dim-other-buffers-mode RET

;; To make the mode enabled every time Emacs starts, add the following
;; to Emacs initialisation file (~/.emacs or ~/.emacs.d/init.el):
;;
;;     (add-hook 'after-init-hook (lambda ()
;;       (when (fboundp 'auto-dim-other-buffers-mode)
;;         (auto-dim-other-buffers-mode t))))

;; To configure how dimmed buffers look like, customise
;; `auto-dim-other-buffers-face'.  This can be accomplished by:
;;
;;     M-x customize-face RET auto-dim-other-buffers-face RET

;;; Code:

(defface auto-dim-other-buffers-face
  '((((background light)) :background "#eff") (t :background "#122"))
  "Face (presumably dimmed somehow) for non-current buffers."
  :group 'auto-dim-other-buffers)

(defcustom auto-dim-other-buffers-dim-on-focus-out t
  "Whether to dim all buffers when a frame looses focus."
  :type 'boolean
  :group 'auto-dim-other-buffers)

(defcustom auto-dim-other-buffers-dim-on-switch-to-minibuffer t
  "Whether to dim last buffer when switching to minibuffer or echo area."
  :type 'boolean
  :group 'auto-dim-other-buffers)

(defvar adob--last-buffer nil
  "Selected buffer before command finished.")

(defun adob--never-dim-p (buffer)
  "Return whether to never dim BUFFER.
Currently only mini buffer and echo areas are ignored."
  (or (null buffer)
      (minibufferp buffer)
      (string-prefix-p " " (buffer-name buffer))))

(defvar-local adob--face-mode-remapping nil
  "Current remapping cookie for `auto-dim-other-buffers-mode'.")

(defun adob--dim-buffer ()
  "Dim current buffer if not already dimmed."
  (when (not adob--face-mode-remapping)
    (setq adob--face-mode-remapping
          (face-remap-add-relative 'default 'auto-dim-other-buffers-face))
    (force-window-update (current-buffer))))

(defun adob--undim-buffer ()
  "Undim current buffer if dimmed."
  (when adob--face-mode-remapping
    (face-remap-remove-relative adob--face-mode-remapping)
    (setq adob--face-mode-remapping nil)
    (force-window-update (current-buffer))))

(defun adob--buffer-list-update-hook ()
  "If buffer has changed, dim the last one and undim the new one."
  (let ((buf (window-buffer)))
    (if (not (eq buf (current-buffer)))
        ;; A new buffer is displayed in some window somewhere.  This is not the
        ;; selected buffer though so dim it.
        (unless (adob--never-dim-p (current-buffer))
          (adob--dim-buffer))
      (unless (or (eq buf adob--last-buffer)
                  (and auto-dim-other-buffers-dim-on-switch-to-minibuffer
                       (minibufferp buf)))
        ;; Buffer has changed.  Dim the old one and undim the new.
        (and (buffer-live-p adob--last-buffer)
             (not (adob--never-dim-p adob--last-buffer))
             (with-current-buffer adob--last-buffer
               (adob--dim-buffer)))
        (adob--undim-buffer)
        (setq adob--last-buffer buf)))))

(defun adob--focus-out-hook ()
  "Dim all buffers if `auto-dim-other-buffers-dim-on-focus-out'."
  (when (and auto-dim-other-buffers-dim-on-focus-out
             adob--last-buffer
             (buffer-live-p adob--last-buffer)
             (not (adob--never-dim-p adob--last-buffer)))
    (with-current-buffer adob--last-buffer
      (adob--dim-buffer))
    (setq adob--last-buffer nil)))

(defun adob--focus-in-hook ()
  "Undim current buffers if `auto-dim-other-buffers-dim-on-focus-out'."
  (when auto-dim-other-buffers-dim-on-focus-out
    (with-current-buffer (window-buffer)
      (adob--undim-buffer)
      (setq adob--last-buffer (current-buffer)))))

;;;###autoload
(define-minor-mode auto-dim-other-buffers-mode
  "Visually makes non-current buffers less prominent"
  :global t
  (let ((callback (if auto-dim-other-buffers-mode #'add-hook #'remove-hook)))
    (dolist (args '((buffer-list-update-hook adob--buffer-list-update-hook)
                    (focus-out-hook adob--focus-out-hook)
                    (focus-in-hook adob--focus-in-hook)))
      (apply callback args)))

  (save-current-buffer
    (if auto-dim-other-buffers-mode
        (progn
          (setq adob--last-buffer (window-buffer))
          (dolist (buffer (buffer-list))
            (unless (or (eq buffer adob--last-buffer)
                        (adob--never-dim-p buffer))
              (set-buffer buffer)
              (adob--dim-buffer))))
      (setq adob--last-buffer nil)
      (dolist (buffer (buffer-list))
        (when (local-variable-p 'adob--face-mode-remapping buffer)
          (set-buffer buffer)
          (when adob--face-mode-remapping
            (face-remap-remove-relative adob--face-mode-remapping))
          (kill-local-variable 'adob--face-mode-remapping))))))

(provide 'auto-dim-other-buffers)

;;; auto-dim-other-buffers.el ends here
