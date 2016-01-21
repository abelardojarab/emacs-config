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

;; Fix appearance of Windows Unicode characters
(standard-display-ascii ?\200 [15])
(standard-display-ascii ?\201 [21])
(standard-display-ascii ?\202 [24])
(standard-display-ascii ?\203 [13])
(standard-display-ascii ?\204 [22])
(standard-display-ascii ?\205 [25])
(standard-display-ascii ?\206 [12])
(standard-display-ascii ?\210 [23])
(standard-display-ascii ?\211 [14])
(standard-display-ascii ?\212 [18])
(standard-display-ascii ?\214 [11])
(standard-display-ascii ?\221 [?\'])
(standard-display-ascii ?\222 [?\'])
(standard-display-ascii ?\223 [?\"])
(standard-display-ascii ?\224 [?\"])
(standard-display-ascii ?\225 [?\*])
(standard-display-ascii ?\226 "---")
(standard-display-ascii ?\227 "--")

;; Do not redraw entire frame after suspending.
(setq no-redraw-on-reenter t)

;; Enable GUI features
(setq use-file-dialog t)
(setq use-dialog-box t)

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

;; Fringe helper
(add-to-list 'load-path "~/.emacs.d/fringe-helper")
(require 'fringe-helper)

;; Monokai theme
(add-to-list 'load-path "~/.emacs.d/monokai-emacs")
(add-to-list 'custom-theme-load-path "~/.emacs.d/monokai-emacs")

;; Atom theme
(add-to-list 'load-path "~/.emacs.d/atom-dark-theme-emacs")
(add-to-list 'custom-theme-load-path "~/.emacs.d/atom-dark-theme-emacs")

;; Zenburn theme
(add-to-list 'load-path "~/.emacs.d/zenburn-emacs")
(add-to-list 'custom-theme-load-path "~/.emacs.d/zenburn-emacs")

;; Faff theme
(add-to-list 'load-path "~/.emacs.d/emacs-faff-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-faff-theme")

;; Material theme
(add-to-list 'load-path "~/.emacs.d/emacs-matherial-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-matherial-theme")

;; Leuven theme
(add-to-list 'load-path "~/.emacs.d/emacs-leuven-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-leuven-theme")

;; Zerodark
(add-to-list 'load-path "~/.emacs.d/zerodark-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/zerodark-theme")

;; Solarized theme
(add-to-list 'load-path "~/.emacs.d/solarized-emacs")
(add-to-list 'custom-theme-load-path "~/.emacs.d/solarized-emacs")
(require 'solarized)
(setq solarized-scale-org-headlines nil)

;; Load Atom dark theme
;; (load-theme 'atom-dark t)
(load-theme 'zenburn t)
;; (load-theme 'leuven t)
;; (load-theme 'zerodark t)
;; (load-theme 'material t)
;; (load-theme 'leuven t)
;; (load-theme 'faff t)
;; (load-theme 'monokai t)
;; (load-theme 'solarized-dark t)

;; White text
(add-to-list 'default-frame-alist '(foreground-color . "#FFFFFF"))

;; Syntax coloring
(global-font-lock-mode t)
(global-hi-lock-mode nil)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size (* 512 512))
(defun global-font-lock-mode-check-buffers () nil)
;; (remove-hook 'post-command-hook #'global-font-lock-mode-check-buffers nil t)

;; Do not fontify large files
(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 512 256))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))
(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

;; hl-line overrides the background of hi-lock’ed text, this will provide a fix
(defadvice hi-lock-set-pattern (around use-overlays activate)
  (let ((font-lock-fontified nil))
    ad-do-it))

;; Adjust font when using graphical interface
(when window-system
  (let ()

    ;; Use 12-pt Consolas as default font
    (when (find-font (font-spec :name "Consolas"))
      (setq main-programming-font "Consolas-12")
      (set-face-attribute 'default nil :font main-programming-font)
      (set-face-attribute 'fixed-pitch nil :font main-programming-font)
      (add-to-list 'default-frame-alist '(font . "Consolas-12"))) ;; default font, used by speedbar

    (when (find-font (font-spec :name "Calibri"))
      (setq main-writing-font "Calibri-12")
      (set-face-attribute 'variable-pitch nil :font main-writing-font :weight 'normal)
      (add-hook 'text-mode-hook 'variable-pitch-mode))

    ;; Dynamic font adjusting based on monitor resolution
    (when (find-font (font-spec :name "Consolas"))
      ;; Finally, set the fonts as desired
      (defun set-default-font (main-programming-font frame)
        (interactive)
        (set-face-attribute 'default nil :font main-programming-font)
        (set-face-attribute 'fixed-pitch nil :font main-programming-font)
        (set-frame-parameter frame 'font main-programming-font))

      (defun fontify-frame (frame)
        (interactive)
        (let (main-writing-font main-programming-font)
          (setq main-programming-font "Consolas-12")
          (setq main-writing-font "Consolas")
          (if (find-font (font-spec :name "Calibri"))
              (setq main-writing-font "Calibri"))

          (case system-type

            ('windows-nt
             (if (> (x-display-pixel-width) 1800)
                 (progn ;; HD monitor in Windows
                   (setq main-programming-font "Consolas-12:antialias=subpixel")
                   (setq main-writing-font (concat main-writing-font "-13")))
               (progn
                 (setq main-programming-font "Consolas-11:antialias=subpixel")
                 (setq main-writing-font (concat main-writing-font "-12")))))
            ('darwin
             (if (> (x-display-pixel-width) 1800)
                 (if (> (x-display-pixel-width) 2000)
                     (progn ;; Ultra-HD monitor in OSX
                       (setq main-programming-font "Consolas-17:antialias=subpixel")
                       (setq main-writing-font (concat main-writing-font "-17")))
                   (progn ;; HD monitor in OSX
                     (setq main-programming-font "Consolas-14:antialias=subpixel")
                     (setq main-writing-font (concat main-writing-font "-14"))))
               (progn
                 (setq main-programming-font "Consolas-11:antialias=subpixel")
                 (setq main-writing-font (concat main-writing-font "-11")))))
            (t ;; Linux
             (if (> (x-display-pixel-width) 2000)
                 (progn ;; HD monitor in Linux
                   (setq main-programming-font "Consolas-14:antialias=subpixel")
                   (setq main-writing-font (concat main-writing-font "-17")))
               (if (> (x-display-pixel-width) 1800)
                   (progn ;; HD monitor in Linux
                     (setq main-programming-font "Consolas-13:antialias=subpixel")
                     (setq main-writing-font (concat main-writing-font "-16")))
                 (progn
                   (setq main-programming-font "Consolas-11:antialias=subpixel")
                   (setq main-writing-font (concat main-writing-font "-13")))))))

          ;; Apply fonts
          (set-default-font main-programming-font frame)
          (add-to-list 'default-frame-alist (cons 'font main-programming-font))
          (set-face-attribute 'fixed-pitch nil :font main-programming-font)
          (set-face-attribute 'variable-pitch nil :font main-writing-font :weight 'normal)))

      ;; Fontify current frame
      (fontify-frame nil)

      ;; Fontify any future frames for emacsclient
      (push 'fontify-frame after-make-frame-functions)

      ;; hook for setting up UI when not running in daemon mode
      (add-hook 'emacs-startup-hook '(lambda () (fontify-frame nil))))))

;; Highlight the line
(require 'hl-line)
(defun local-hl-line-mode-off ()
  (interactive)
  (make-local-variable 'global-hl-line-mode)
  (setq global-hl-line-mode t))
(add-hook 'org-mode-hook 'local-hl-line-mode-off)

;; Highlight the latest changes in the buffer (like text inserted from: yank, undo, etc.) until the next command is run
(add-to-list 'load-path "~/.emacs.d/volatile-highlights")
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; Fixed pitched for html/nxml
(defun fixed-pitch-mode ()
  (buffer-face-mode -1))
(add-hook 'html-mode-hook 'fixed-pitch-mode)
(add-hook 'nxml-mode-hook 'fixed-pitch-mode)

;; Change form/shape of emacs cursor
(setq djcb-read-only-color "green")
(setq djcb-read-only-cursor-type 'hbar)
(setq djcb-overwrite-color "red")
(setq djcb-overwrite-cursor-type 'box)
(setq djcb-normal-color "yellow")
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

;; Highlight blocks
(add-to-list 'load-path "~/.emacs.d/highlight-blocks")
(require 'highlight-blocks)

;; Permanent indentation guide
(add-to-list 'load-path "~/.emacs.d/indent-hint")
(setq indent-hint-background-overlay t)
(setq indent-hint-bg nil)
(require 'indent-hint)
(add-hook 'prog-mode-hook 'indent-hint-mode)
(add-hook 'lisp-mode-hook 'indent-hint-lisp)

;; Transient indentation guide
(add-to-list 'load-path "~/.emacs.d/indent-guide")
(defvar indent-guide-mode)
(require 'indent-guide)

;; Highlight symbol
(add-to-list 'load-path "~/.emacs.d/highlight-symbol")
(require 'highlight-symbol)
(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
(setq highlight-symbol-on-navigation-p t)

;; higlight changes in documents
(global-highlight-changes-mode t)
(setq highlight-changes-visibility-initial-state nil)

;; Fix highlight bug of marking a file as modified
(defadvice highlight-changes-rotate-faces (around around-rotate-faces)
  (let ((was-modified (buffer-modified-p))
        (buffer-undo-list t))
    ad-do-it
    (unless was-modified
      (set-buffer-modified-p nil))))
(ad-activate 'highlight-changes-rotate-faces)

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

;; Blinking cursor
(require 'heartbeat-cursor)
(add-hook 'prog-mode-hook (lambda () (heartbeat-cursor-mode)))
(add-hook 'org-mode-hook (lambda () (heartbeat-cursor-mode)))

;; Pretty lambdas
(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("\\<lambda\\>"
          (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))
(add-hook 'emacs-lisp-mode-hook 'pretty-lambdas)
(add-hook 'lisp-mode-hook 'pretty-lambdas)

;; Pretty mode
(add-to-list 'load-path "~/.emacs.d/pretty-mode")
(require 'pretty-mode)
(global-pretty-mode t)

(provide 'setup-appearance)
;;; setup-appearance.el ends here
