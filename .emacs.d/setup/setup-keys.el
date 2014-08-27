;;; setup-keys.el ---

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

;; Ergoemacs
(add-to-list 'load-path "~/.emacs.d/ergoemacs")
(add-to-list 'load-path "~/.emacs.d/ergoemacs-fixed-keybindings")
(setq ergoemacs-ignore-prev-global nil) ;; Will not ignore any globally defined keybinding
(setq ergoemacs-keyboard-layout "us")
(require 'ergoemacs-mode)
(ergoemacs-mode 1)

;; Enter changes lines and auto-indents the new line
(mapc (lambda (mode)
        (add-hook mode '(lambda () (define-key java-mode-map "\C-m" 'newline-and-indent))))
      '(c-mode-hook
        c++-mode-hook
        lisp-mode-hook
        python-mode-hook
        js2-mode-hook
        vhdl-mode-hook
        java-mode-hook))

;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

;; Zoom in/out like feature, without mouse wheel
(ergoemacs-fixed-key '[C-kp-add] 'text-scale-increase)
(ergoemacs-fixed-key '[C-kp-subtract] 'text-scale-decrease)
(ergoemacs-fixed-key '[C-+] 'text-scale-increase)

;; Commands to make my programming environment nice
(ergoemacs-fixed-key (kbd "RET") 'newline-and-indent)
(ergoemacs-fixed-key "\C-l" 'goto-line)
(ergoemacs-fixed-key (kbd "") 'other-window)
(ergoemacs-fixed-key [f5] 'compile)
(ergoemacs-fixed-key [f6] 'next-error)
(ergoemacs-fixed-key [f11] 'djcb-full-screen-toggle)
(ergoemacs-fixed-key [C-tab] 'comment-or-uncomment-region)
(ergoemacs-fixed-key [kp-prior] 'scroll-down)
(ergoemacs-fixed-key [prior] 'scroll-down)
(ergoemacs-fixed-key [kp-next] 'scroll-up)
(ergoemacs-fixed-key [next] 'scroll-up)
(ergoemacs-fixed-key "\M-g" 'goto-line)
(ergoemacs-fixed-key [home] 'beginning-of-line)
(ergoemacs-fixed-key [end] 'end-of-line)
(ergoemacs-fixed-key [delete] 'delete-char)
(ergoemacs-fixed-key [kp-delete] 'delete-char)
(ergoemacs-fixed-key [(meta delete)] '(lambda () (interactive) (backward-or-forward-kill-word -1)))
(ergoemacs-fixed-key [(alt delete)] '(lambda () (interactive) (backward-or-forward-kill-word -1)))
(ergoemacs-fixed-key [M-up] 'enlarge-window)
(ergoemacs-fixed-key [M-down] 'shrink-window)
(ergoemacs-fixed-key [(control o)] 'find-file)
(ergoemacs-fixed-key [(control n)] 'find-file-other-frame)
(ergoemacs-fixed-key [(control s)] 'save-buffer)
(ergoemacs-fixed-key [(meta s)] 'write-file)
(ergoemacs-fixed-key [(control q)] 'save-buffers-kill-emacs)
(ergoemacs-fixed-key [(meta q)] 'kill-this-buffer)
(ergoemacs-fixed-key [(control t)] 'ispell-buffer)
(ergoemacs-fixed-key [(control r)] 'replace-string)
(ergoemacs-fixed-key [(control z)] 'undo)
(ergoemacs-fixed-key "\C-a" 'mark-whole-buffer)
(ergoemacs-fixed-key (kbd "\C-c \C-c") 'kill-ring-save)

;; search forward with Ctrl-f
(ergoemacs-fixed-key [(control f)] 'isearch-forward)
(define-key isearch-mode-map [(control f)] (lookup-key isearch-mode-map "\C-f"))
(define-key minibuffer-local-isearch-map [(control f)]
  (lookup-key minibuffer-local-isearch-map "\C-s"))

;; search backward with Alt-f
(ergoemacs-fixed-key [(meta f)] 'isearch-backward)
(define-key isearch-mode-map [(meta f)] (lookup-key isearch-mode-map "\C-r"))
(define-key minibuffer-local-isearch-map [(meta f)]
  (lookup-key minibuffer-local-isearch-map "\C-r"))

;; Cancel minibuffer operation if you click outside
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; Mouse wheel scroll support
(mouse-wheel-mode t)

;; Moving cursor down at bottom scrolls only a single line, not half page
(setq
 scroll-margin 0                ;; start scrolling when marker at top/bottom
 scroll-conservatively 100000   ;; marker distance from center (don't jump to center)
 scroll-preserve-screen-position 1) ;; try to keep screen position when PgDn/PgUp

;; These ones are buffer local and thus have to be set up by setq-default
(setq-default scroll-up-aggressively 0.01 scroll-down-aggressively 0.01)

;; Moving cursor down at bottom scrolls only a single line, not half page
(setq scroll-step 1)
(setq auto-window-vscroll t)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed t) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Scroll with the mouse
(defun smooth-scroll (number-lines increment)
  (if (= 0 number-lines)
      t
    (progn
      (sit-for (* 0.01 number-lines))
      (scroll-up increment)
      (smooth-scroll (- number-lines 1) increment))))

(ergoemacs-fixed-key [(mouse-5)] '(lambda () (interactive) (smooth-scroll 8 1)))
(ergoemacs-fixed-key [(mouse-4)] '(lambda () (interactive) (smooth-scroll 8 -1)))

;; Zoom in/out like feature, with mouse wheel
(global-unset-key (kbd "<C-wheel-up>")) ;; moved to <mode-line>
(global-unset-key (kbd "<C-wheel-down>"))
(ergoemacs-fixed-key (kbd "<C-wheel-up>") 'text-scale-increase) ;; moved to <mode-line>
(ergoemacs-fixed-key (kbd "<C-wheel-down>") 'text-scale-decrease)

;; Get the scroll wheel to work
(ergoemacs-fixed-key [(shift button5)] '(lambdas () (interactive) (scroll-up-line)))
(ergoemacs-fixed-key [(shift button4)] '(lambda () (interactive) (scroll-down-line)))
(ergoemacs-fixed-key [(control button5)] 'text-scale-decrease)
(ergoemacs-fixed-key [(control button4)] 'text-scale-increase)

(ergoemacs-fixed-key [(shift mouse-5)] '(lambda () (interactive) (scroll-up-line)))
(ergoemacs-fixed-key [(shift mouse-4)] '(lambda () (interactive) (scroll-down-line)))
(ergoemacs-fixed-key [(control mouse-5)] 'text-scale-decrease)
(ergoemacs-fixed-key [(control mouse-4)] 'text-scale-increase)

;; higlight changes in documents
(global-highlight-changes-mode t)
(setq highlight-changes-visibility-initial-state nil)

;; toggle visibility
(ergoemacs-fixed-key (kbd "<f6>") 'highlight-changes-visible-mode) ;; changes

;; remove the change-highlight in region
(ergoemacs-fixed-key (kbd "S-<f6>") 'highlight-changes-remove-highlight)

;; If you're not already using it for something else...
(ergoemacs-fixed-key (kbd "<M-next>") 'highlight-changes-next-change)
(ergoemacs-fixed-key (kbd "<M-prior>")  'highlight-changes-previous-change)
(set-face-foreground 'highlight-changes nil)
(set-face-background 'highlight-changes "#882020")
(set-face-foreground 'highlight-changes-delete nil)
(set-face-background 'highlight-changes-delete "#916868")

;; toggle truncate lines
(ergoemacs-fixed-key (kbd "<f8>") 'toggle-truncate-lines)

;; Bookmark keys
(ergoemacs-fixed-key (kbd "<C-f2>") 'bm-toggle)
(ergoemacs-fixed-key (kbd "<f2>")   'bm-next)
(ergoemacs-fixed-key (kbd "<S-f2>") 'bm-previous)

;; Refresh file on F9
(defun refresh-file ()
  (interactive)
  (revert-buffer t t t))
(ergoemacs-fixed-key [f9] 'refresh-file)

;; Code folding
(defun toggle-selective-display ()
  (interactive)
  (set-selective-display (if selective-display nil 1)))
(ergoemacs-fixed-key [f1] 'toggle-selective-display)

;; Windows-like mouse/arrow movement & selection
(transient-mark-mode 1)
(setq cua-keep-region-after-copy t)
(cua-mode 1)
(setq shift-select-mode t)

;; As in Windows, replace after typing a letter
(require 'delsel)
(delete-selection-mode 1)
(setq mouse-drag-copy-region nil)

;; Redo
(require 'redo+)
(ergoemacs-fixed-key (kbd "C-S-z") 'redo) ; Mac style
(ergoemacs-fixed-key (kbd "C-y") 'redo) ; Microsoft Windows style
(setq undo-no-redo t)

;; Better undo
(require 'undo-tree)
(global-undo-tree-mode)

;; Right click mouse
(global-unset-key [(control mouse-3)])
(require 'mouse3)
(defalias 'mouse3-region-popup-menu 'mouse3-popup-menu)
(ergoemacs-fixed-key [(control mouse-3)] 'mouse3-popup-menu)

;; Trick emacs when opening a file through menu-find-file-existing
(defadvice find-file-read-args (around find-file-read-args-always-use-dialog-box act)
  "Simulate invoking menu item as if by the mouse; see `use-dialog-box'."
  (let ((last-nonmenu-event nil)
        (use-dialog-box t))
    ad-do-it))

;; Mac Key mode
(require 'mac-key-mode)

;; Show guide for shortcuts
(add-to-list 'load-path "~/.emacs.d/guide-key")
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
(guide-key-mode 1) ;; Enable guide-key-mode

;; Guide key tooltips
(add-to-list 'load-path "~/.emacs.d/pos-tip")
(add-to-list 'load-path "~/.emacs.d/guide-key-tip")
(require 'guide-key-tip)
(setq guide-key-tip/enabled t)

(provide 'setup-keys)
;;; setup-keys.el ends here
