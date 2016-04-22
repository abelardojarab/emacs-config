;;; setup-font-lock.el ---                           -*- lexical-binding: t; -*-

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

;; Syntax coloring
(global-font-lock-mode t)
(global-hi-lock-mode nil)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size (* 512 512))
(setq font-lock-multiline t)
(defun global-font-lock-mode-check-buffers () nil)

;; Lazy font lock
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-chunk-size 25
      jit-lock-context-time 0.05 ;; jit-lock-context-time seconds of Emacs idle time, redisplay will refontify
      jit-lock-defer-time 0.05   ;; improve scrolling speed
      jit-lock-stealth-nice 0.05 ;; time to pause between chunks of stealth fontification
      jit-lock-stealth-time 1   ;; time to wait before starting stealth fontification
      jit-lock-stealth-verbose nil)

;; Do not fontify large files
(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 512 512))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))
(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

;; Displaying image tooltips in Emacs
;; http://kitchingroup.cheme.cmu.edu/blog/2016/03/21/Displaying-image-overlays-on-image-filenames-in-Emacs/
(defvar image-tooltip-re (concat  "\\(?3:'\\|\"\\)\\(?1:.*\\."
                                  (regexp-opt '("png" "PNG" "JPG" "jpeg"
                                                "jpg" "JPEG" "eps" "EPS"
                                                "pdf" "PDF" "ps" "PS"))
                                  "\\)\\(?:\\3\\)")
  "Regexp to match image filenames in quotes")

;; Tooltip creation
(defun image-tooltip (window object position)
  (save-excursion
    (goto-char position)
    (let (beg end imgfile img s)
      (while (not (looking-at image-tooltip-re))
        (forward-char -1))
      (setq imgfile (match-string-no-properties 1))
      (when (file-exists-p imgfile)
        (setq img (create-image (expand-file-name imgfile)
                                'imagemagick nil :width 200))
        (propertize "Look in the minibuffer"
                    'display img)))))

;; Enable tooltip in graphical mode
(if (display-graphic-p)
    (font-lock-add-keywords
     nil
     `((,image-tooltip-re
        0 '(face font-lock-keyword-face
                 help-echo image-tooltip)))))

(provide 'setup-font-lock)
;;; setup-font-lock.el ends here
