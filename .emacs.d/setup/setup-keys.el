;;; setup-keys.el ---

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
;; GNU General Public License for more details.g

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; Windows-like mouse/arrow movement & selection
(transient-mark-mode t)
(setq shift-select-mode t)
(cua-mode 1)

;; As in Windows, replace after typing a letter
(require 'delsel)
(delete-selection-mode 1)
(setq mouse-drag-copy-region nil)

;; Multiple cursors
(add-to-list 'load-path "~/.emacs.d/multiple-cursors")
(require 'multiple-cursors)

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

;; Smart tab
(add-to-list 'load-path "~/.emacs.d/smart-tab")
(when (require 'smart-tab nil 'noerror)
  (global-smart-tab-mode))

;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

;; Zoom in/out like feature, without mouse wheel
(global-set-key '[C-kp-add] 'text-scale-increase)
(global-set-key '[C-kp-subtract] 'text-scale-decrease)
(global-set-key '[C-+] 'text-scale-increase)

;; Commands to make my programming environment nice
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key "\C-l" 'goto-line)
(global-set-key (kbd "") 'other-window)
(global-set-key [f5] 'compile)
(global-set-key [C-tab] 'comment-or-uncomment-region)
(global-set-key [kp-prior] 'scroll-down)
(global-set-key [prior] 'scroll-down)
(global-set-key [kp-next] 'scroll-up)
(global-set-key [next] 'scroll-up)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)
(global-set-key [(control o)] 'find-file)
(global-set-key [(control n)] 'find-file-other-frame)
(global-set-key [(control s)] 'save-buffer)
(global-set-key [(meta s)] 'write-file)
(global-set-key [(control q)] 'save-buffers-kill-emacs)
(global-set-key [(meta q)] 'kill-this-buffer)
(global-set-key [(control t)] 'ispell-buffer)
(global-set-key [(control r)] 'replace-string)
(global-set-key "\C-a" 'mark-whole-buffer)
(global-set-key (kbd "<f7>") 'toggle-line-spacing)
(global-set-key (kbd "<f8>") 'toggle-truncate-lines)
(global-set-key (kbd "<f12>") 'ecb-redraw-layout)

;; Native file opening
(cond
 ;; Windows
 ((equal system-type 'windows-nt)
  (global-set-key "\C-x\C-f" 'dlgopen-open-files)
  (define-key menu-bar-file-menu [open-file] '("Open File..." . dlgopen-open-files))
  ) ;; if

 ;; Linux
 ((and (equal system-type 'gnu/linux)
       (executable-find "kdialog"))
  (global-set-key "\C-x\C-f" 'kde-open-file)
  (define-key menu-bar-file-menu [open-file] '("Open File..." . kde-open-file))
  ) ;; if

 ;; Mac
 ((equal system-type 'darwin)
  (global-set-key "\C-x\C-f" 'mac-open-file)
  (define-key menu-bar-file-menu [open-file] '("Open File..." . mac-open-file))
  ) ;; if

 (t
  nil
  )) ;; cond

;; C-v
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-S-v") 'browse-kill-ring)

;; search forward with Ctrl-f
(global-set-key [(control f)] 'isearch-forward)
(define-key isearch-mode-map [(control f)] (lookup-key isearch-mode-map "\C-s"))
(define-key minibuffer-local-isearch-map [(control f)]
  (lookup-key minibuffer-local-isearch-map "\C-s"))

;; search backward with Alt-f
(global-set-key [(meta f)] 'isearch-backward)
(define-key isearch-mode-map [(meta f)] (lookup-key isearch-mode-map "\C-r"))
(define-key minibuffer-local-isearch-map [(meta f)]
  (lookup-key minibuffer-local-isearch-map "\C-r"))

;; Cancel minibuffer operation if you click outside
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; Escape key in minibuffer
(define-key minibuffer-local-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

;; Smooth scrolling
(add-to-list 'load-path "~/.emacs.d/smooth-scrolling")
(require 'smooth-scrolling)

;; Moving cursor down at bottom scrolls only a single line, not half page
(setq
 scroll-margin 1                ;; start scrolling when marker at top/bottom
 scroll-conservatively 100000   ;; marker distance from center (don't jump to center)
 scroll-preserve-screen-position 1) ;; try to keep screen position when PgDn/PgUp

;; These ones are buffer local and thus have to be set up by setq-default
(setq-default scroll-up-aggressively 0.01 scroll-down-aggressively 0.01)

;; Moving cursor down at bottom scrolls only a single line, not half page
(setq scroll-step 1)
(setq auto-window-vscroll t)

;; Mouse wheel scroll support
(mouse-wheel-mode t)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(0.07)) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Scroll with the mouse
(defun smooth-scroll (increment)
  (scroll-up increment) (sit-for 0.05)
  (scroll-up increment) (sit-for 0.02)
  (scroll-up increment) (sit-for 0.02)
  (scroll-up increment) (sit-for 0.05)
  (scroll-up increment) (sit-for 0.06)
  (scroll-up increment))

(global-set-key [(mouse-5)] '(lambda () (interactive) (smooth-scroll 1)))
(global-set-key [(mouse-4)] '(lambda () (interactive) (smooth-scroll -1)))

;; Zoom in/out like feature, with mouse wheel
(global-unset-key (kbd "<C-wheel-up>")) ;; moved to <mode-line>
(global-unset-key (kbd "<C-wheel-down>"))
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase) ;; moved to <mode-line>
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

;; Get the scroll wheel to work
(global-set-key [(shift button5)] '(lambdas () (interactive) (scroll-up-line)))
(global-set-key [(shift button4)] '(lambda () (interactive) (scroll-down-line)))
(global-set-key [(control button5)] 'text-scale-decrease)
(global-set-key [(control button4)] 'text-scale-increase)

(global-set-key [(shift mouse-5)] '(lambda () (interactive) (scroll-up-line)))
(global-set-key [(shift mouse-4)] '(lambda () (interactive) (scroll-down-line)))
(global-set-key [(control mouse-5)] 'text-scale-decrease)
(global-set-key [(control mouse-4)] 'text-scale-increase)

;; Mac Key mode
(require 'mac-key-mode)

;; Refresh file on F9
(defun refresh-file ()
  (interactive)
  (revert-buffer t t t))
(global-set-key [f9] 'refresh-file)

;; Code folding
(defun toggle-selective-display ()
  (interactive)
  (set-selective-display (if selective-display nil 1)))
(global-set-key [f11] 'toggle-selective-display)

;; Redo
(require 'redo+)
(global-set-key (kbd "C-S-z") 'redo) ; Mac style
(global-set-key (kbd "C-y") 'redo) ; Microsoft Windows style
(setq undo-no-redo t)

;; Better undo
(require 'undo-tree)
(global-undo-tree-mode)
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)
(setq undo-tree-visualizer-diff t)
(setq undo-tree-visualizer-timestamps t)

;; Right click mouse
(global-unset-key [(control mouse-3)])
(require 'mouse3)
(defalias 'mouse3-region-popup-menu 'mouse3-popup-menu)
(global-set-key (kbd "<mouse-3>") 'mouse3-popup-menu)

;; Move text
(require 'move-text)
(global-set-key [M-S-up] 'move-text-up)
(global-set-key [M-S-down] 'move-text-down)

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

;; Faster buffer switching
(add-to-list 'load-path "~/.emacs.d/popup-switcher")
(require 'popup-switcher)
(require 'popup-select-window)

;; Jump between buffers
(defun xah-next-user-buffer ()
  "Switch to the next user buffer.
 (buffer name does not start with *.)"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-equal "*" (substring (buffer-name) 0 1)) (< i 20))
      (setq i (1+ i)) (next-buffer))))

(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer.
 (buffer name does not start with *.)"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-equal "*" (substring (buffer-name) 0 1)) (< i 20))
      (setq i (1+ i)) (previous-buffer))))
(global-set-key (kbd "C-S-<left>") 'popup-select-window)
(global-set-key (kbd "C-S-<right>") 'popup-select-window)

;; Jump between windows
(require 'eassist)
(define-key c-mode-base-map [f4] 'eassist-switch-h-cpp)
(define-key c-mode-base-map [C-f4] 'dts-switch-between-header-and-source)
(global-set-key (kbd "C-0") 'psw-switch-buffer)
(global-set-key (kbd "C-1") 'psw-switch-function)
(global-set-key [(meta left)] 'psw-switch-function)
(global-set-key [(meta right)] 'psw-switch-buffer)

;; Drag stuff
(add-to-list 'load-path "~/.emacs.d/drag-stuff")
(when (require 'drag-stuff nil 'noerror)
  (drag-stuff-global-mode t))

;; Region bindings mode
(add-to-list 'load-path "~/.emacs.d/region-bindings-mode")
(require 'region-bindings-mode)
(region-bindings-mode-enable)
(define-key region-bindings-mode-map (kbd "C-p") 'mc/mark-previous-like-this)
(define-key region-bindings-mode-map (kbd "C-n") 'mc/mark-next-like-this)
(define-key region-bindings-mode-map (kbd "C-a") 'mc/mark-all-like-this)
(define-key region-bindings-mode-map (kbd "C-e") 'mc/edit-lines)
(define-key region-bindings-mode-map (kbd "C-c") 'kill-ring-save)
(define-key region-bindings-mode-map (kbd "C-x") 'kill-region)

;; Overwrite other modes
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
(define-key my-keys-minor-mode-map (kbd "<mouse-3>") 'mouse3-popup-menu)
(define-key my-keys-minor-mode-map [C-tab] 'comment-or-uncomment-region)
(define-key my-keys-minor-mode-map (kbd "<f12>") 'ecb-redraw-layout)
(define-key my-keys-minor-mode-map (kbd "M-.") 'helm-etags-select)
(define-key my-keys-minor-mode-map (kbd "C-S-<left>") 'popup-select-window)
(define-key my-keys-minor-mode-map (kbd "C-S-<right>") 'popup-select-window)
(define-key my-keys-minor-mode-map [(meta left)] 'psw-switch-function)
(define-key my-keys-minor-mode-map [(meta right)] 'psw-switch-buffer)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)
(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))

;; Disable overwrite for some modes
(add-hook 'org-mode-hook 'my-minibuffer-setup-hook)

;; Advice to set proper order for keymaps
(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my-keys-minor-mode))
      (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

;; Selecting line with the mouse
(require 'drag-select-lines)

(provide 'setup-keys)
;;; setup-keys.el ends here
