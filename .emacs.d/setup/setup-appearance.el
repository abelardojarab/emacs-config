;;; setup-appearance.el ---

;; Copyright (C) 2014  abelardo.jara-berrocal

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

;; Improve Emacs display engine
(setq redisplay-dont-pause t)

;; Marker if the line goes beyond the end of the screen (arrows)
(global-visual-line-mode 1)

;; Zenburn theme
(add-to-list 'load-path "~/.emacs.d/zenburn-emacs")
(require 'zenburn-theme)
(load-theme 'zenburn t)

;; Extra color tweaks
(set-face-foreground 'variable-pitch "#ffffff")
(set-face-foreground 'font-lock-comment-face "#90f0ff")
(set-face-foreground 'font-lock-comment-delimiter-face "#90f0ff")
(set-face-foreground 'font-lock-string-face "yellow")
(set-face-foreground 'font-lock-doc-face "orange")

;; Syntax coloring
(global-font-lock-mode 1)

;; Use 10-pt Consolas as default font
(if (find-font (font-spec :name "Consolas"))
    (set-face-attribute 'default nil :font "Consolas-10"))

(if (find-font (font-spec :name "Calibri"))
    (set-face-attribute 'variable-pitch nil :font "Cambria-14" :weight 'normal))
(add-hook 'text-mode-hook 'variable-pitch-mode)

(if (find-font (font-spec :name "Consolas"))
    (set-face-attribute 'fixed-pitch nil :font "Consolas-10"))

;; Fallback for Unicode symbols
(if (find-font (font-spec :name "Symbola"))
    (set-fontset-font "fontset-default" nil
                      (font-spec :size 18 :name "Symbola")))

;; Line numbers, vim style
(require 'linum)
(global-linum-mode 1)
(set-face-attribute 'linum nil :height 125)
(setq linum-delay t)

(eval-after-load 'linum
  '(progn
     (defface linum-leading-zero
       `((t :inherit 'linum
            :foreground ,(face-attribute 'linum :background nil t)))
       "Face for displaying leading zeroes for line numbers in display margin."
       :group 'linum)
     (defun linum-format-func (line)
       (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
         (propertize (format (format " %%%dd " w) line) 'face 'linum)))
     (setq linum-format 'linum-format-func)))

;; Dynamic font adjusting based on monitor resolution
(when (find-font (font-spec :name "Consolas"))
  (defun fontify-frame (frame)
    (interactive)
    (if window-system
        (progn
          (if (> (x-display-pixel-width) 1800)
              (if (equal system-type 'windows-nt)
                  (progn ;; HD monitor in Windows
                    (set-face-attribute 'default nil :font "Consolas-12")
                    (set-face-attribute 'variable-pitch nil :font "Cambria-15" :weight 'normal)
                    (set-face-attribute 'fixed-pitch nil :font "Consolas-12")
                    (set-face-attribute 'linum nil :height 130)
                    (set-frame-parameter frame 'font "Consolas-12"))
                (if (> (x-display-pixel-width) 2000)
                    (progn ;; Cinema display
                      (set-face-attribute 'default nil :font "Consolas-16")
                      (set-face-attribute 'variable-pitch nil :font "Cambria-19" :weight 'normal)
                      (set-face-attribute 'fixed-pitch nil :font "Consolas-16")
                      (set-face-attribute 'linum nil :height 160)
                      (set-frame-parameter frame 'font "Consolas-16"))
                  (progn ;; HD monitor in Windows and Mac
                    (set-face-attribute 'default nil :font "Consolas-14")
                    (set-face-attribute 'variable-pitch nil :font "Cambria-17" :weight 'normal)
                    (set-face-attribute 'fixed-pitch nil :font "Consolas-14")
                    (set-face-attribute 'linum nil :height 140)
                    (set-frame-parameter frame 'font "Consolas-14"))))
            (progn ;; Cinema display
              (set-face-attribute 'default nil :font "Consolas-10")
              (set-face-attribute 'variable-pitch nil :font "Cambria-14" :weight 'normal)
              (set-face-attribute 'fixed-pitch nil :font "Consolas-10")
              (set-face-attribute 'linum nil :height 125)
              (set-frame-parameter frame 'font "Consolas-10"))))))

  ;; Fontify current frame
  (fontify-frame nil)

  ;; Fontify any future frames
  (push 'fontify-frame after-make-frame-functions))

;; Change form/shape of emacs cursor
(setq djcb-read-only-color "gray")
(setq djcb-read-only-cursor-type 'hbar)
(setq djcb-overwrite-color "red")
(setq djcb-overwrite-cursor-type 'box)
(setq djcb-normal-color "gray")
(setq djcb-normal-cursor-type 'bar)
(defun djcb-set-cursor-according-to-mode ()
  "change cursor color and type according to some minor modes."
  (cond
   (buffer-read-only
    (set-cursor-color djcb-read-only-color)
    (setq cursor-type djcb-read-only-cursor-type))
   (overwrite-mode
    (set-cursor-color djcb-overwrite-color)
    (setq cursor-type djcb-overwrite-cursor-type))
   (t
    (set-cursor-color djcb-normal-color)
    (setq cursor-type djcb-normal-cursor-type))))
(add-hook 'post-command-hook
          (lambda () (interactive)
            (unless (member
                     major-mode '(pdf-docs doc-view-mode))
              (djcb-set-cursor-according-to-mode))))

;; Put a nice title to the window, including filename
(add-hook 'window-configuration-change-hook
          (lambda ()
            (setq frame-title-format
                  (concat
                   invocation-name "@" system-name ": "
                   (replace-regexp-in-string
                    (concat "/home/" user-login-name) "~"
                    (or buffer-file-name "%b"))))))

;; Next code work with Emacs 21.4, 22.3, 23.1.
(when window-system
  (add-hook 'window-setup-hook
            (let ((px (display-pixel-width))
                  (py (display-pixel-height))
                  (fx (frame-char-width))
                  (fy (frame-char-height))
                  tx ty)
              ;; Next formulas discovered empiric on Windows host with default font.
              (setq tx (- (/ px fx) 3))
              (setq ty (- (/ py fy) 8))
              (setq initial-frame-alist '((top . 2) (left . 2)))
              (add-to-list 'initial-frame-alist (cons 'width tx))
              (add-to-list 'initial-frame-alist (cons 'height ty))
              t)))

;; Adjust Emacs size according to resolution
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
      (progn
        ;; use 130 char wide window for largeish displays
        ;; and smaller 80 column windows for smaller displays
        ;; pick whatever numbers make sense for you
        (if (> (x-display-pixel-width) 1280)
            (add-to-list 'default-frame-alist (cons 'width 140))
          (add-to-list 'default-frame-alist (cons 'width 80)))
        ;; for the height, subtract a couple hundred pixels
        ;; from the screen height (for panels, menubars and
        ;; whatnot), then divide by the height of a char to
        ;; get the height we want
        (add-to-list 'default-frame-alist
                     (cons 'height (/ (- (x-display-pixel-height) 150) (frame-char-height)))))))

;; highlight indentation using vertical lines
(add-hook 'c-mode-common-hook 'indent-vline)
(add-hook 'lisp-mode-hook 'indent-vline)
(add-hook 'python-mode-hook 'indent-vline)
(add-hook 'js2-mode-hook 'indent-vline)
(load "~/.emacs.d/elisp/00_func.el")
(require 'aux-line)

;; In every buffer, the line which contains the cursor will be fully highlighted
(global-hl-line-mode 1)
(defun local-hl-line-mode-off ()
  (interactive)
  (make-local-variable 'global-hl-line-mode)
  (setq global-hl-line-mode nil))
(add-hook 'org-mode-hook 'local-hl-line-mode-off)

;; Pretty lambdas
(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("\\<lambda\\>"
          (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))
(add-hook 'emacs-lisp-mode-hook 'pretty-lambdas)
(add-hook 'lisp-mode-hook 'pretty-lambdas)
(add-to-list 'load-path "~/.emacs.d/pretty-symbols")
(require 'pretty-symbols)
(add-hook 'lisp-mode-hook 'pretty-symbols-mode)
(add-to-list 'load-path "~/.emacs.d/pretty-mode")
(require 'pretty-mode)
(global-pretty-mode t)

(provide 'setup-appearance)
;;; setup-appearance.el ends here
