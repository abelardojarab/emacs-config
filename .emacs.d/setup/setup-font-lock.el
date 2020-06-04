;;; setup-font-lock.el ---                           -*- lexical-binding: t; -*-

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

;; Font-core library
(use-package font-core)

;; Syntax coloring
(use-package font-lock
  :defer t
  :commands (global-font-lock-mode
             font-lock-mode)
  :init (global-font-lock-mode t)
  :custom ((font-lock-maximum-decoration nil)
           (font-lock-support-mode       'jit-lock-mode)
           (font-lock-multiline          t)
           (jit-lock-defer-time          0.1)
           (jit-lock-defer-contextually  nil)
           (jit-lock-chunk-size          8000)
           (jit-lock-stealth-load        10)
           (jit-lock-stealth-time        0.02)
           (jit-lock-stealth-nice        0.01)
           (jit-lock-stealth-verbose     nil))
  :config (progn
            (defun global-font-lock-mode-check-buffers () nil)

            ;; Displaying image tooltips in Emacs
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
                             help-echo image-tooltip)))))))

;; Colorize color strings
(use-package rainbow-mode
  :defer t
  :commands rainbow-mode
  :diminish rainbow-mode
  :config (mapc (lambda (mode)
                  (add-hook mode #'rainbow-mode))
                my/rainbow-modes))

;; Highlight and navigate TODO keywords
(use-package hl-todo
  :defer t
  :commands global-hl-todo-mode
  :init (global-hl-todo-mode))

(provide 'setup-font-lock)
;;; setup-font-lock.el ends here
