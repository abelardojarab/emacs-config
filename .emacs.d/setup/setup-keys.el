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
(add-to-list 'load-path "~/.emacs.d/ergoemacs-keybindings")
(setq ergoemacs-keyboard-layout "us")
(require 'ergoemacs-mode)
(ergoemacs-mode 1)

;; Zoom in/out like feature, with mouse wheel
(global-unset-key (kbd "<C-wheel-up>")) ;; moved to <mode-line>
(global-unset-key (kbd "<C-wheel-down>"))
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase) ;; moved to <mode-line>
(global-set-key  (kbd "<C-wheel-down>") 'text-scale-decrease)

;; Zoom in/out like feature, without mouse wheel
(global-set-key '[C-kp-add] 'text-scale-increase)
(global-set-key '[C-kp-subtract] 'text-scale-decrease)
(global-set-key '[C-+] 'text-scale-increase)

;; Commands to make my programming environment nice
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key "\C-l" 'goto-line)
(global-set-key (kbd "") 'other-window)
(global-set-key [f5] 'compile)
(global-set-key [f6] 'next-error)
(global-set-key [f11] 'djcb-full-screen-toggle)
(global-set-key [C-tab] 'comment-or-uncomment-region)
(global-set-key [kp-prior] 'scroll-down)
(global-set-key [prior] 'scroll-down)
(global-set-key [kp-next] 'scroll-up)
(global-set-key [next] 'scroll-up)
(global-set-key "\M-g" 'goto-line)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)
(global-set-key [(meta delete)] '(lambda () (interactive) (backward-or-forward-kill-word -1)))
(global-set-key [(alt delete)] '(lambda () (interactive) (backward-or-forward-kill-word -1)))
(global-set-key [M-up] 'enlarge-window)
(global-set-key [M-down] 'shrink-window)
(global-set-key [(control o)] 'find-file)
(global-set-key [(control n)] 'find-file-other-frame)
(global-set-key [(control s)] 'save-buffer)
(global-set-key [(meta s)] 'write-file)
(global-set-key [(control q)] 'save-buffers-kill-emacs)
(global-set-key [(meta q)] 'kill-this-buffer)
(global-set-key [(control t)] 'ispell-buffer)
(global-set-key [(control r)] 'replace-string)
(global-set-key [(control z)] 'undo)
(global-set-key "\C-a" 'mark-whole-buffer)
(global-set-key (kbd "\C-c \C-c") 'kill-ring-save)

;; search forward with Ctrl-f
(global-set-key [(control f)] 'isearch-forward)
(define-key isearch-mode-map [(control f)] (lookup-key isearch-mode-map "\C-f"))
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

;; Scroll with the mouse
(defun smooth-scroll (number-lines increment)
  (if (= 0 number-lines)
      t
    (progn
      (sit-for (* 0.01 number-lines))
      (scroll-up increment)
      (smooth-scroll (- number-lines 1) increment))))

(global-set-key [(mouse-5)] '(lambda () (interactive) (smooth-scroll 8 1)))
(global-set-key [(mouse-4)] '(lambda () (interactive) (smooth-scroll 8 -1)))

;; to get the scroll wheel work
(global-set-key [(shift button5)] '(lambdas () (interactive) (scroll-up-line)))
(global-set-key [(shift button4)] '(lambda () (interactive) (scroll-down-line)))
(global-set-key [(control button5)] 'text-scale-decrease)
(global-set-key [(control button4)] 'text-scale-increase)

(global-set-key [(shift mouse-5)] '(lambda () (interactive) (scroll-up-line)))
(global-set-key [(shift mouse-4)] '(lambda () (interactive) (scroll-down-line)))
(global-set-key [(control mouse-5)] 'text-scale-decrease)
(global-set-key [(control mouse-4)] 'text-scale-increase)

;; higlight changes in documents
(global-highlight-changes-mode t)
(setq highlight-changes-visibility-initial-state nil)

;; toggle visibility
(global-set-key (kbd "<f6>") 'highlight-changes-visible-mode) ;; changes

;; remove the change-highlight in region
(global-set-key (kbd "S-<f6>") 'highlight-changes-remove-highlight)

;; if you're not already using it for something else...
(global-set-key (kbd "<M-next>") 'highlight-changes-next-change)
(global-set-key (kbd "<M-prior>")  'highlight-changes-previous-change)
(set-face-foreground 'highlight-changes nil)
(set-face-background 'highlight-changes "#882020")
(set-face-foreground 'highlight-changes-delete nil)
(set-face-background 'highlight-changes-delete "#916868")

;; toggle truncate lines
(global-set-key (kbd "<f8>") 'toggle-truncate-lines)

;; Refresh file on F9
(defun refresh-file ()
  (interactive)
  (revert-buffer t t t))
(global-set-key [f9] 'refresh-file)

;; Show guide for shortcuts
(add-to-list 'load-path "~/.emacs.d/guide-key")
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
(guide-key-mode 1) ;; Enable guide-key-mode

;; Code folding
(defun toggle-selective-display ()
  (interactive)
  (set-selective-display (if selective-display nil 1)))
(global-set-key [f1] 'toggle-selective-display)

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
(global-set-key (kbd "C-S-z") 'redo) ; Mac style
(global-set-key (kbd "C-y") 'redo) ; Microsoft Windows style
(setq undo-no-redo t)

;; Better undo
(require 'undo-tree)
(global-undo-tree-mode)

;; Right click mouse
(global-unset-key (kbd "<mouse-3>"))
(require 'mouse3)
(defalias 'mouse3-region-popup-menu 'mouse3-popup-menu)
(global-set-key (kbd "<mouse-3>") 'mouse3-region-popup-menu)

(provide 'setup-keys)
;;; setup-keys.el ends here
