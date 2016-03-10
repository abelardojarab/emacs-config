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

;; Fringe helper
(add-to-list 'load-path "~/.emacs.d/fringe-helper")
(require 'fringe-helper)

;; Monokai theme
(add-to-list 'load-path "~/.emacs.d/monokai-emacs")
(add-to-list 'custom-theme-load-path "~/.emacs.d/monokai-emacs")

;; Monokai theme
(add-to-list 'load-path "~/.emacs.d/monokai-extended-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/monokai-extended-theme")

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
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-material-theme")

;; Leuven theme
(add-to-list 'load-path "~/.emacs.d/emacs-leuven-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-leuven-theme")

;; FlatUI theme
(add-to-list 'load-path "~/.emacs.d/emacs-flatui-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-flatui-theme")

;; FlatUI theme
(add-to-list 'load-path "~/.emacs.d/pastelmac-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/pastelmac-theme")

;; Zerodark theme
(add-to-list 'load-path "~/.emacs.d/zerodark-theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/zerodark-theme")

;; Apropospriate theme
;; (add-to-list 'load-path "~/.emacs.d/apropospriate-theme")
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/apropospriate-theme")
;; (require 'apropospriate)

;; Solarized theme
(add-to-list 'load-path "~/.emacs.d/solarized-emacs")
(add-to-list 'custom-theme-load-path "~/.emacs.d/solarized-emacs")
(require 'solarized)
(setq solarized-scale-org-headlines nil)

;; Different possible themes
;; (load-theme 'atom-dark t)
;; (load-theme 'zenburn t)
;; (load-theme 'leuven t)
;; (load-theme 'zerodark t)
;; (load-theme 'material t)
;; (load-theme 'FlatUI t)
;; (load-theme 'faff t)
(load-theme 'monokai t)
;; (load-theme 'monokai-extended t)
;; (load-theme 'pastelmac t)
;; (load-theme 'solarized-dark t)
;; (load-theme 'material-light t)
;; (load-theme 'apropospriate-light t)

;; White or black text
;; (add-to-list 'default-frame-alist '(foreground-color . "#ffffff"))

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

    ;; Dynamic font adjusting based on monitor resolution, using Android fonts
    (when (find-font (font-spec :name "Roboto Mono"))

      (defun fontify-frame (frame)
        (interactive)
        (let (main-writing-font main-programming-font)
          (setq main-programming-font "Roboto Mono")
          (setq main-writing-font "Roboto Mono")
          (if (find-font (font-spec :name "Roboto Mono"))
              (setq main-writing-font "Roboto Mono"))

          ;; Adjust text size based on resolution
          (case system-type
            ('windows-nt
             (if (> (x-display-pixel-width) 1800)
                 (progn ;; HD monitor in Windows
                   (setq main-programming-font (concat main-programming-font "-12"))
                   (setq main-writing-font (concat main-writing-font "-13")))
               (progn
                 (setq main-programming-font (concat main-programming-font "-11"))
                 (setq main-writing-font (concat main-writing-font "-12")))))
            ('darwin
             (if (> (x-display-pixel-width) 1800)
                 (if (> (x-display-pixel-width) 2000)
                     (progn ;; Ultra-HD monitor in OSX
                       (setq main-programming-font (concat main-programming-font "-17"))
                       (setq main-writing-font (concat main-writing-font "-17")))
                   (progn ;; HD monitor in OSX
                     (setq main-programming-font (concat main-programming-font "-14"))
                     (setq main-writing-font (concat main-writing-font "-14"))))
               (progn
                 (setq main-programming-font (concat main-programming-font "-11"))
                 (setq main-writing-font (concat main-writing-font "-11")))))
            (t ;; Linux
             (if (> (x-display-pixel-width) 2000)
                 (progn ;; Ultra-HD monitor in Linux
                   (setq main-programming-font (concat main-programming-font "-14"))
                   (setq main-writing-font (concat main-writing-font "-15")))
               (if (> (x-display-pixel-width) 1800)
                   (progn ;; HD monitor in Linux
                     (setq main-programming-font (concat main-programming-font "-13"))
                     (setq main-writing-font (concat main-writing-font "-14")))
                 (progn
                   (setq main-programming-font (concat main-programming-font "-11"))
                   (setq main-writing-font (concat main-writing-font "-12")))))))

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

;; Highlight symbol
(add-to-list 'load-path "~/.emacs.d/highlight-symbol")
(require 'highlight-symbol)
(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
(add-hook 'org-mode-hook (lambda () (highlight-symbol-mode)))
(add-hook 'markdown-mode-hook (lambda () (highlight-symbol-mode)))
(add-hook 'text-mode-hook (lambda () (highlight-symbol-mode)))
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

(provide 'setup-appearance)
;;; setup-appearance.el ends here
