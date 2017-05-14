;;; setup-keys.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015, 2016, 2017  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojara@Abelardos-MacBook-Pro.local>
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

;; As in Windows, replace after typing a letter
(use-package delsel
  :config (delete-selection-mode 1))

;; Windows-like mouse/arrow movement & selection
(use-package cua-base
  :config (cua-mode 1))

;; Shift mark package
(use-package shift-mark
  :init (progn
          (transient-mark-mode t)
          (setq shift-select-mode t))
  :config (progn
            (defun shift-mark-forward-page ()
              (interactive)
              (shift-mark 'my/page-down))

            (defun shift-mark-backward-page ()
              (interactive)
              (shift-mark 'my/page-up)))

  ;; key bindings for shift select
  (global-set-key [S-prior] 'shift-mark-backward-page)
  (global-set-key [S-next]  'shift-mark-forward-page))

;; popup-based buffer switcher
(use-package popup-switcher
  :defer t
  :commands (psw-switch-recentf
             psw-switch-buffer
             psw-switch-function
             psw-switch-projectile-files
             psw-navigate-files)
  :bind (:map ctl-x-map
              ("/" . psw-switch-function))
  :load-path (lambda () (expand-file-name "popup-switcher/" user-emacs-directory)))

;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

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
(global-set-key [(control s)] 'save-buffer)
(global-set-key [(control o)] 'counsel-find-file)
(global-set-key [(meta s)] 'write-file)
(global-set-key [(control q)] 'save-buffers-kill-emacs)
(global-set-key [(meta q)] 'kill-this-buffer)
(global-set-key [(control r)] 'replace-string)
(global-set-key [(control a)] 'mark-whole-buffer)

;; Buffer navigation
(global-set-key [(control n)] 'scroll-down)
(global-set-key [(control p)] 'scroll-up)

;; Bookmarks
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<C-f2>") 'bm-toggle)

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
(global-set-key (kbd "<f5>") 'recompile)

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
(global-set-key (kbd "<f8>") 'hs-toggle-hiding-all)
(global-set-key (kbd "C-<f8>") 'fold-dwim-toggle)

;; Refresh file
(global-set-key (kbd "<f9>") 'refresh-file)

;; Menu bar
(global-set-key (kbd "<f10>") 'menu-bar-open)

;; Toggle frame maximized
(global-set-key (kbd "<f11>") 'toggle-frame-maximized)

;; List buffers
(global-set-key (kbd "<f12>") 'ivy-switch-buffer)
(global-set-key (kbd "C-<f12>") 'neotree-toggle)

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
(global-set-key (kbd "C-c v") 'counsel-yank-pop)
(define-key ctl-x-map (kbd "P") '(lambda () (interactive) (popup-menu 'yank-menu)))

;; Unindent (do what I mean version)
(global-set-key (kbd "C-<") 'unindent-dwim)
(global-set-key (kbd "C->") (lambda () (interactive) (unindent-dwim -1)))

;; Beautify buffer
(global-set-key "\C-c\C-b" 'beautify-buffer)

;; Escape key in minibuffer
(define-key minibuffer-local-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

;; Tabbar
(global-set-key [C-prior] 'tabbar-backward-tab)
(global-set-key [C-next] 'tabbar-forward-tab)
(global-set-key [C-home] 'tabbar-backward-group)
(global-set-key [C-end] 'tabbar-forward-group)

;; Tabbar, now using ctl-x-map
(global-set-key (kbd "C-x <prior>") 'tabbar-backward-tab)
(global-set-key (kbd "C-x <next>") 'tabbar-forward-tab)
(global-set-key (kbd "C-x <home>") 'tabbar-backward-group)
(global-set-key (kbd "C-x <end>") 'tabbar-forward-group)

;; Jump between windows
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

;; Extra Ctrl-x mappings
(define-key ctl-x-map (kbd "SPC") (lambda () (interactive) (push-mark nil nil 1)))
(define-key ctl-x-map (kbd ">") 'increase-left-margin)
(define-key ctl-x-map (kbd "<") 'decrease-left-margin)
(define-key ctl-x-map (kbd "F") 'toggle-frame-fullscreen)
(define-key ctl-x-map (kbd "T") 'toggle-truncate-lines)

;; Extra Ctrl-x mappings for navigation
(define-key ctl-x-map (kbd "<up>") 'windmove-up)
(define-key ctl-x-map (kbd "<down>") 'windmove-down)
(define-key ctl-x-map (kbd "<left>") 'windmove-left)
(define-key ctl-x-map (kbd "<right>") 'windmove-right)
(define-key ctl-x-map (kbd "<prior>") 'tabbar-backward-tab)
(define-key ctl-x-map (kbd "<next>") 'tabbar-forward-tab)
(define-key ctl-x-map (kbd "<home>") 'tabbar-backward-group)
(define-key ctl-x-map (kbd "<end>") 'tabbar-forward-group)

;; Overwrite other modes
(defvar my/keys-minor-mode-map (make-keymap) "my/keys-minor-mode keymap.")
(define-key my/keys-minor-mode-map (kbd "<mouse-3>") 'mouse3-popup-menu)
(define-key my/keys-minor-mode-map [C-tab] 'comment-or-uncomment-region)
(define-key my/keys-minor-mode-map (kbd "C-x z") 'ielm-repl)
(define-key my/keys-minor-mode-map (kbd "M-.") 'helm-etags-select)
(define-key my/keys-minor-mode-map (kbd "C-.") 'helm-gtags-dwim)
(define-key my/keys-minor-mode-map (kbd "<f2>")   'bm-next)
(define-key my/keys-minor-mode-map (kbd "<C-f2>") 'bm-toggle)
(define-key my/keys-minor-mode-map (kbd "<left-fringe> <double-mouse-1>") 'bm-toggle)
(define-key my/keys-minor-mode-map (kbd "<f4>") 'helm-semantic-or-imenu)
(define-key my/keys-minor-mode-map (kbd "C-`") 'helm-semantic-or-imenu)
(define-key my/keys-minor-mode-map (kbd "<f12>") 'ivy-switch-buffer)
(define-key my/keys-minor-mode-map (kbd "C-0") 'ivy-switch-buffer)
(define-key my/keys-minor-mode-map (kbd "<f5>") 'recompile)
(define-key my/keys-minor-mode-map [(control p)] 'scroll-down)
(define-key my/keys-minor-mode-map [(control n)] 'scroll-up)
(define-key my/keys-minor-mode-map [(control b)] 'ivy-switch-buffer)

;; key bindings for shift select
(define-key my/keys-minor-mode-map [S-prior] 'shift-mark-backward-page)
(define-key my/keys-minor-mode-map [S-next] 'shift-mark-forward-page)

;; Define custom key mode
(define-minor-mode my/keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t "" 'my/keys-minor-mode-map)
(diminish 'my/keys-minor-mode)
(my/keys-minor-mode 1)
(defun my/keys-disable-setup-hook ()
  (my/keys-minor-mode 0))

;; Disable overwrite for some modes
(add-hook 'org-mode-hook 'my/keys-disable-setup-hook)

;; Advice to set proper order for keymaps
(defadvice load (after give-my/keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my/keys-minor-mode))
      (let ((mykeys (assq 'my/keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'my/keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

;; Enable console keys
(use-package console-keys
  :if (not (display-graphic-p))
  :load-path (lambda () (expand-file-name "console-keys/" user-emacs-directory)))

(provide 'setup-keys)
;;; setup-keys.el ends here
