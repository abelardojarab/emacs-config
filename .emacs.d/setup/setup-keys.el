;;; setup-keys.el ---

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
;; GNU General Public License for more details.g

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; xterm through SSH
(defun fix-up-xterm-input-function-key-map ()
  (define-key function-key-map "\e[1;2A" [S-up])
  (define-key function-key-map "\e[1;2B" [S-down])
  (define-key function-key-map "\e[1;2C" [S-right])
  (define-key function-key-map "\e[1;2D" [S-left])
  (define-key function-key-map "\e[2A"   [S-up])
  (define-key function-key-map "\e[2B"   [S-down])
  (define-key function-key-map "\e[2C"   [S-right])
  (define-key function-key-map "\e[2D"   [S-left])

  (define-key function-key-map "\e[1;5A" [C-up])
  (define-key function-key-map "\e[1;5B" [C-down])
  (define-key function-key-map "\e[1;5C" [C-right])
  (define-key function-key-map "\e[1;5D" [C-left])
  (define-key function-key-map "\e[5A"   [C-up])
  (define-key function-key-map "\e[5B"   [C-down])
  (define-key function-key-map "\e[5C"   [C-right])
  (define-key function-key-map "\e[5D"   [C-left])

  (define-key function-key-map "\e[1;3A" [M-up])
  (define-key function-key-map "\e[1;3B" [M-down])
  (define-key function-key-map "\e[1;3C" [M-right])
  (define-key function-key-map "\e[1;3D" [M-left])

  (define-key function-key-map "\e[1;4A" [M-S-up])
  (define-key function-key-map "\e[1;4B" [M-S-down])
  (define-key function-key-map "\e[1;4C" [M-S-right])
  (define-key function-key-map "\e[1;4D" [M-S-left])

  (define-key function-key-map "\e[5~"   [prior])
  (define-key function-key-map "\e[6~"   [next])
  (define-key function-key-map "\e[5;5~" [C-prior])
  (define-key function-key-map "\e[6;5~" [C-next])

  (define-key function-key-map "\e[5;2~"  [S-prior])
  (define-key function-key-map "\e[5;3~"  [M-prior])
  (define-key function-key-map "\e[5;4~"  [M-S-prior])
  (define-key function-key-map "\e[5;5~"  [C-prior])
  (define-key function-key-map "\e[5;6~"  [C-S-prior])
  (define-key function-key-map "\e[5;7~"  [M-C-prior])
  (define-key function-key-map "\e[5;8~"  [M-C-S-prior])

  (define-key function-key-map "\e[6;2~"  [S-next])
  (define-key function-key-map "\e[6;3~"  [M-next])
  (define-key function-key-map "\e[6;4~"  [M-S-next])
  (define-key function-key-map "\e[6;5~"  [C-next])
  (define-key function-key-map "\e[6;6~"  [C-S-next])
  (define-key function-key-map "\e[6;7~"  [M-C-next])
  (define-key function-key-map "\e[6;8~"  [M-C-S-next])

  (define-key function-key-map "\e[z2a"    [S-tab])
  (define-key function-key-map "\e[z5a"    [C-tab])
  (define-key function-key-map "\e[z2b"    [S-return])
  (define-key function-key-map "\e[z3b"    [M-return])
  (define-key function-key-map "\e[z4b"    [M-S-return])
  (define-key function-key-map "\e[z5b"    [C-return])
  (define-key function-key-map "\e[z6b"    [C-S-return])
  (define-key function-key-map "\e[z7b"    [M-C-return])
  (define-key function-key-map "\e[z8b"    [M-C-S-return]))
(fix-up-xterm-input-function-key-map)

;; As in Windows, replace after typing a letter
(require 'delsel)
(delete-selection-mode 1)
(setq mouse-drag-copy-region nil)

;; Windows-like mouse/arrow movement & selection
(transient-mark-mode t)
(setq shift-select-mode t)
(cua-mode 1)

;; Smart tab
(add-to-list 'load-path "~/.emacs.d/smart-tab")
(require 'smart-tab)
(cons 'yas-hippie-try-expand 'hippie-expand-try-functions-list)
(setq smart-tab-using-hippie-expand t)
(global-smart-tab-mode)

;; Fix tab problem in some modes that grab the tab key so auto-complete and yasnipet dont work
(defun ac-tab-noconflict ()
  (let ((command (key-binding [tab]))) ;; remember command
    (local-unset-key [tab]) ;; unset from (kbd "<tab>")
    (local-set-key (kbd "TAB") command))) ;; bind to (kbd "TAB")
(add-hook 'markdown-mode-hook 'ac-tab-noconflict)
(add-hook 'org-mode-hook 'ac-tab-noconflict)

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
(global-set-key [(control a)] 'mark-whole-buffer)

;; Recent files
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Bookmarks
(global-set-key (kbd "<f2>")   'helm-bm)
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<S-f2>") 'bm-next)

;; Highlight symbol at point
(global-set-key [f3] 'highlight-symbol-at-point)
(global-set-key [(control f3)] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)
(global-set-key [(control shift mouse-1)]
                (lambda (event)
                  (interactive "e")
                  (goto-char (posn-point (event-start event)))
                  (highlight-symbol-at-point)))

;; Helm semantic (switch function)
(global-set-key (kbd "<f4>") 'helm-semantic-or-imenu)

;; Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "<f5>") 'smex)

;; Flyspell
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))
(global-set-key (kbd "<f6>") 'flyspell-check-next-highlighted-word)
(global-set-key (kbd "C-<f6>") 'helm-flyspell-correct)

;; Flycheck
(global-set-key (kbd "<f7>") 'flycheck-next-error)
(global-set-key (kbd "C-<f7>") 'helm-flycheck)

;; Code folding
(defun toggle-selective-display ()
  (interactive)
  (set-selective-display (if selective-display nil 1)))
(global-set-key (kbd "<f8>") 'toggle-selective-display)
(global-set-key (kbd "C-<f8>") 'fold-dwim-toggle)

;; Org capture
(global-set-key (kbd "<f9>") 'org-capture)
(global-set-key (kbd "C-<f9>") 'org-projectile:project-todo-completing-read)

;; Hint: customize `magit-repo-dirs' so that you can
;; quickly open magit on any one of your projects.
(global-set-key (kbd "<f10>") 'menu-bar-open)
(global-set-key (kbd "C-<f10>") 'helm-ls-git-ls)
(global-set-key (kbd "S-<f10>") #'git-messenger:popup-message)

;; Toggle frame maximized
(global-set-key (kbd "<f11>") 'toggle-frame-maximized)

;; iBuffer
(global-set-key (kbd "<f12>") 'helm-buffer-list)

;; Use GNU global instead of normal find-tag, fall back to etags-select
(global-set-key (kbd "C-.") (if (and (fboundp 'ggtags-find-tag-dwim)
                                     (executable-find "global"))
                                'ggtags-find-tag-dwim
                              'etags-select-find-tag))

;; Use Helm instead of 'etags-select-find-tag
(global-set-key (kbd "M-.") 'helm-etags-select)

;; Native file opening
(cond
 ;; Windows
 ((equal system-type 'windows-nt)
  (global-set-key "\C-x\C-f" 'dlgopen-open-files)
  (define-key menu-bar-file-menu [open-file] '("Open File..." . dlgopen-open-files))) ;; if

 ;; Linux
 ((and (equal system-type 'gnu/linux)
       (executable-find "kdialog"))
  (global-set-key "\C-x\C-f" 'kde-open-file)
  (define-key menu-bar-file-menu [open-file] '("Open File..." . kde-open-file))) ;; if

 ;; Mac
 ((equal system-type 'darwin)
  (global-set-key "\C-x\C-f" 'mac-open-file)
  (define-key menu-bar-file-menu [open-file] '("Open File..." . mac-open-file))) ;; if

 (t
  nil)) ;; cond

;; C-v
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-S-v") 'helm-show-kill-ring)
(global-set-key "\C-cy" '(lambda ()
                           (interactive)
                           (popup-menu 'yank-menu)))

;; Beautify buffer
(global-unset-key "\C-b")
(global-set-key "\C-b" 'beautify-buffer)

;; Find matching parenthesis
(global-set-key [(control %)] 'goto-match-paren-or-up)

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

;; exit search mode with any key
(setq search-exit-option t)

;; Search at point
(global-set-key (kbd "C-=") 'my-isearch-word-at-point)
(define-key isearch-mode-map (kbd "C-=")
  (lambda ()
    "Reset current isearch to a word-mode search of the word under point."
    (interactive)
    (setq isearch-word t
          isearch-string ""
          isearch-message "")
    (isearch-yank-string (word-at-point))))

;; Helm-swoop extension
(global-set-key (kbd "C-*") 'helm-swoop)
(global-set-key [(control kp-multiply)] 'helm-swoop)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "C-*") 'helm-swoop-from-isearch)
(define-key isearch-mode-map [(control kp-multiply)] 'helm-swoop-from-isearch)

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

;; Enable mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t))

;; Moving cursor down at bottom scrolls only a single line, not half page
(setq scroll-step 1)

;; Mouse wheel scroll support
(mouse-wheel-mode t)

;; scroll window under mouse
(setq mouse-wheel-follow-mouse 't)

;; scroll one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)

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

;; Refresh file
(defun refresh-file ()
  (interactive)
  (revert-buffer t t t))

;; Redo
(require 'redo+)
(global-set-key (kbd "C-S-z") 'redo) ;; Mac style
(global-set-key (kbd "C-y") 'redo) ;; Microsoft Windows style
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

;; Tabbar
(global-set-key [C-prior] 'tabbar-backward-tab)
(global-set-key [C-next] 'tabbar-forward-tab)
(global-set-key [C-home] 'tabbar-backward-group)
(global-set-key [C-end] 'tabbar-forward-group)

;; Alternative tabbar keys
(global-set-key (kbd "C-x <prior>") 'tabbar-backward-group)
(global-set-key (kbd "C-x <next>") 'tabbar-forward-group)
(global-set-key (kbd "C-x <home>") 'tabbar-forward-tab)
(global-set-key (kbd "C-x <end>") 'tabbar-backward-tab)

;; Jump between windows
(global-set-key [C-up] 'windmove-up)
(global-set-key [C-down] 'windmove-down)
(global-set-key [C-left] 'windmove-left)
(global-set-key [C-right] 'windmove-right)

;; Code navigation
(global-set-key (kbd "C-`") 'helm-semantic)

;; Flycheck tips
(define-key global-map (kbd "C->") 'error-tip-cycle-dwim)
(define-key global-map (kbd "C-<") 'error-tip-cycle-dwim-reverse)

;; Extra Ctrl-x mapping
(define-key ctl-x-map (kbd "u") 'my-unindent)

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

;; Move text
(require 'move-text)
(define-key region-bindings-mode-map [C-up] 'move-text-up)
(define-key region-bindings-mode-map [C-down] 'move-text-down)
(define-key region-bindings-mode-map [C-right] 'increase-left-margin)
(define-key region-bindings-mode-map [C-left] 'decrease-left-margin)

;; Overwrite other modes
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
(define-key my-keys-minor-mode-map (kbd "<mouse-3>") 'mouse3-popup-menu)
(define-key my-keys-minor-mode-map [C-tab] 'comment-or-uncomment-region)
(define-key my-keys-minor-mode-map (kbd "M-.") 'helm-etags-select)
(define-key my-keys-minor-mode-map (kbd "<f2>")   'helm-bm)
(define-key my-keys-minor-mode-map (kbd "<C-f2>") 'bm-toggle)
(define-key my-keys-minor-mode-map (kbd "<left-margin> <mouse-1>") 'bm-toggle)
(define-key my-keys-minor-mode-map (kbd "C-`") 'helm-semantic)
(define-key my-keys-minor-mode-map (kbd "C-b") 'beautify-buffer)
(define-key my-keys-minor-mode-map (kbd "M-o") 'psw-switch-buffer)
(define-key my-keys-minor-mode-map (kbd "<f4>") 'helm-semantic-or-imenu)
(define-key my-keys-minor-mode-map (kbd "<f5>") 'smex)
(define-key my-keys-minor-mode-map (kbd "<f12>") 'helm-buffer-list)

;; Define custom key mode
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " Custom" 'my-keys-minor-mode-map)

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

(provide 'setup-keys)
;;; setup-keys.el ends here
