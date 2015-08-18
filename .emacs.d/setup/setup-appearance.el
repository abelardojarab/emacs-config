;;; setup-appearance.el ---

;; Copyright (C) 2014, 2015  abelardo.jara-berrocal

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

;; Fit frame
(require 'autofit-frame)
(add-hook 'after-make-frame-functions 'fit-frame)

;; Non-nil means no need to redraw entire frame after suspending.
(setq no-redraw-on-reenter t)

;; Improve Emacs display engine
(setq redisplay-dont-pause t)

;; Disable bidirectional text support
(setq-default bidi-display-reordering nil)

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

;; Truncate lines
(set-default 'truncate-lines t)
(toggle-truncate-lines 1)

;; Marker if the line goes beyond the end of the screen (arrows)
(global-visual-line-mode 1)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))
(add-hook 'lisp-mode-hook
          (lambda ()
            (visual-line-mode -1)
            (toggle-truncate-lines 1)))
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (visual-line-mode -1)
            (toggle-truncate-lines 1)))

;; Make side by side buffers function the same as the main window
(setq-default truncate-partial-width-windows nil)

;; Fix for visual line mode
(require 'adaptive-wrap)
(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)
(add-hook 'visual-line-mode-hook 'toggle-truncate-lines)
(setq adaptive-wrap-extra-indent 4)

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
;; (load-theme 'zenburn t)
;; (load-theme 'leuven t)
;; (load-theme 'zerodark t)
;; (load-theme 'material t)
;; (load-theme 'material-light t)
(load-theme 'faff t)

;; Use for dark themes only
(set-face-attribute 'region nil :background "#999")

;; Syntax coloring
(require 'font-lock+)
(require 'jit-lock)
(global-font-lock-mode t)
(global-hi-lock-mode nil)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size (* 512 512))
(setq font-lock-support-mode 'jit-lock-mode ;; lazy-lock-mode
      fast-lock-cache-directories '("~/.emacs.cache"))
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-stealth-time 20.0
      jit-lock-stealth-load 300
      jit-lock-chunk-size 20
      jit-lock-defer-time 0.05
      jit-lock-stealth-nice 0.5
      jit-lock-contextually t
      jit-lock-stealth-verbose nil)

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

;; snippet to analyze complex log files
(defun hi-lock-show-all ()
  "Show all lines in the current buffer containing a overlay of hi-lock."
  (interactive)
  (let ((newbuf (format "*hi-lock:%s*" (buffer-name)))
        (hide-start (point-min)))
    (when (get-buffer newbuf) (kill-buffer newbuf))
    (clone-indirect-buffer-other-window newbuf t)
    (with-current-buffer newbuf
      (goto-char (point-min))
      (dolist (bol (save-excursion
                     (sort
                      (mapcar (lambda (ov)
                                (goto-char (overlay-start ov))
                                (point-at-bol))
                              (ee-flatten (overlay-lists))) '<)))
        (goto-char bol)
        (outline-flag-region hide-start bol t)
        (forward-line 1)
        (setq hide-start (point))))
    (outline-flag-region hide-start (point-max) t)
    (goto-char (point-min))
    (view-mode 1)))

(defun hi-lock-overlay-p (overlay)
  "Return the overlay if overlay is a hi-lock overlay."
  (if (and (overlayp overlay)
         (eq (overlay-get overlay 'hi-lock-overlay) t))
      overlay
    nil))

;; if there is size information associated with text, change the text
;; size to reflect it
(size-indication-mode t)

;; Use 10-pt Consolas as default font
(when (find-font (font-spec :name "Consolas"))
  (set-face-attribute 'default nil :font "Consolas-12")
  (set-face-attribute 'fixed-pitch nil :font "Consolas-12:antialias=subpixel")
  (add-to-list 'default-frame-alist '(font . "Consolas-12"))) ;; default font, used by Speedbar

(if (find-font (font-spec :name "Calibri"))
    (set-face-attribute 'variable-pitch nil :font "Calibri-12" :weight 'normal))
(add-hook 'text-mode-hook 'variable-pitch-mode)

;; Fallback for Unicode symbols
(if (find-font (font-spec :name "Symbola"))
    (set-fontset-font "fontset-default" nil
                      (font-spec :size 18 :name "Symbola")))

;; Highlight the line
(require 'hl-line)
(global-hl-line-mode t)
(defun local-hl-line-mode-off ()
  (interactive)
  (make-local-variable 'global-hl-line-mode)
  (setq global-hl-line-mode nil))
(add-hook 'org-mode-hook 'local-hl-line-mode-off)

;; Highlight the latest changes in the buffer (like text inserted from: yank, undo, etc.) until the next command is run
(add-to-list 'load-path "~/.emacs.d/volatile-highlights")
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; Do not use linum, but nlinum instead
(require 'nlinum)

;; Preset width nlinum
(add-hook 'nlinum-mode-hook
          (lambda ()
            (setq nlinum--width
                  (+ 2 (length (number-to-string
                                (count-lines (point-min) (point-max))))))))

;; Dynamic font adjusting based on monitor resolution
(when (find-font (font-spec :name "Consolas"))
  (let ()

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

        (if window-system
            (progn
              (if (> (x-display-pixel-width) 1800)
                  (if (equal system-type 'windows-nt)
                      (progn ;; HD monitor in Windows
                        (setq main-programming-font "Consolas-11:antialias=subpixel")
                        (setq main-writing-font (concat main-writing-font "-13")))
                    (if (> (x-display-pixel-width) 2000)
                        (progn ;; Cinema display
                          (setq main-programming-font "Consolas-15:antialias=subpixel")
                          (setq main-writing-font (concat main-writing-font "-18")))
                      (progn ;; HD monitor
                        (setq main-programming-font "Consolas-13:antialias=subpixel")
                        (setq main-writing-font (concat main-writing-font "-16")))))
                (progn ;; Small display
                  (if (equal system-type 'darwin)
                      (progn
                        (setq main-programming-font "Consolas-12:antialias=subpixel")
                        (setq main-writing-font (concat main-writing-font "-15")))
                    (progn
                      (setq main-programming-font "Consolas-10:antialias=subpixel")
                      (setq main-writing-font (concat main-writing-font "-13"))))))))

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
    (add-hook 'emacs-startup-hook '(lambda () (fontify-frame nil)))))

;; Fixed pitched for html/nxml
(defun fixed-pitch-mode ()
  (buffer-face-mode -1))
(add-hook 'html-mode-hook 'fixed-pitch-mode)
(add-hook 'nxml-mode-hook 'fixed-pitch-mode)

;; Pretty lambdas
(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("\\<lambda\\>"
        (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                  ,(make-char 'greek-iso8859-7 107))
                  nil))))))
(add-hook 'emacs-lisp-mode-hook 'pretty-lambdas)
(add-hook 'lisp-mode-hook 'pretty-lambdas)

;; Pretty symbols mode
(add-to-list 'load-path "~/.emacs.d/pretty-symbols")
(require 'pretty-symbols)
(add-hook 'lisp-mode-hook 'pretty-symbols-mode)

;; Additional Unicode characters
(add-to-list 'load-path "~/.emacs.d/pretty-mode")
(require 'pretty-mode)
(global-pretty-mode t)

;; Change form/shape of emacs cursor
(setq djcb-read-only-color "gray")
(setq djcb-read-only-cursor-type 'hbar)
(setq djcb-overwrite-color "red")
(setq djcb-overwrite-cursor-type 'box)
(setq djcb-normal-color "black")
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

;; Adjust Emacs size according to resolution
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

;; Works for Emacs 24.4 and above
(add-to-list 'default-frame-alist '(fullscreen . fullheight))

;; Highlight blocks
(add-to-list 'load-path "~/.emacs.d/highlight-blocks")
(require 'highlight-blocks)

;; Context indentation guide
(add-to-list 'load-path "~/.emacs.d/indent-guide")
(require 'indent-guide)
(indent-guide-global-mode)
(setq indent-guide-char ":")

;; Permanent indentation guide
(add-to-list 'load-path "~/.emacs.d/indent-hint")
(require 'indent-hint)
(add-hook 'prog-mode-hook 'indent-hint-mode)
(add-hook 'lisp-mode-hook 'indent-hint-lisp)

;; Highlight symbol
(add-to-list 'load-path "~/.emacs.d/highlight-symbol")
(require 'highlight-symbol)
(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
(setq highlight-symbol-on-navigation-p t)

;; higlight changes in documents
(global-highlight-changes-mode t)
(setq highlight-changes-visibility-initial-state nil)

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

;; If you're not already using it for something else...
(set-face-foreground 'highlight-changes nil)
(set-face-background 'highlight-changes "#882020")
(set-face-foreground 'highlight-changes-delete nil)
(set-face-background 'highlight-changes-delete "#916868")

;; Scrollbar
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
        (add-hook 'post-command-hook 'lawlist-scroll-bar nil t)
        ;; (add-hook 'change-major-mode-hook 'lawlist-scroll-bar nil t)
        ;; (add-hook 'window-configuration-change-hook 'lawlist-scroll-bar nil t)
        )
    (remove-hook 'post-command-hook 'lawlist-scroll-bar t)
    (remove-hook 'change-major-mode-hook 'lawlist-scroll-bar t)
    (remove-hook 'window-configuration-change-hook 'lawlist-scroll-bar t)))

(define-globalized-minor-mode global-lawlist-scroll-bar-mode
  lawlist-scroll-bar-mode lawlist-scroll-bar-on)

(defun lawlist-scroll-bar-on ()
  (unless (minibufferp)
    (lawlist-scroll-bar-mode 1)))

(global-lawlist-scroll-bar-mode)

;; https://github.com/kentaro/auto-save-buffers-enhanced
;; `regexp-match-p` function modified by @sds on stackoverflow
;; http://stackoverflow.com/questions/20343048/distinguishing-files-with-extensions-from-hidden-files-and-no-extensions
(defun regexp-match-p (regexps string)
  (and string
     (catch 'matched
       (let ((inhibit-changing-match-data t)) ; small optimization
         (dolist (regexp regexps)
           (when (string-match regexp string)
             (throw 'matched t)))))))

(provide 'setup-appearance)
;;; setup-appearance.el ends here
