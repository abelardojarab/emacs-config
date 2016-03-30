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

;; As in Windows, replace after typing a letter
(use-package delsel
  :config (delete-selection-mode 1))

;; Windows-like mouse/arrow movement & selection
(use-package cua-base
  :init (progn
          (transient-mark-mode t)
          (setq shift-select-mode t))
  :config (progn
            (cua-mode 1)))

;; Smart tab
(use-package smart-tab
  :load-path (lambda () (expand-file-name "smart-tab/" user-emacs-directory))
  :config (progn
            (cons 'yas-hippie-try-expand 'hippie-expand-try-functions-list)
            (setq smart-tab-using-hippie-expand t)
            (global-smart-tab-mode)))

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
(global-set-key '[C--] 'text-scale-decrease)

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
(global-set-key [(control r)] 'replace-string)
(global-set-key [(control a)] 'mark-whole-buffer)
(global-set-key [(control t)] 'magit-status)

;; Recent files
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Bookmarks
(global-set-key (kbd "<f2>")   'helm-bm)
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
(global-set-key (kbd "<f8>") 'toggle-selective-display)
(global-set-key (kbd "C-<f8>") 'fold-dwim-toggle)

;; Refresh file
(global-set-key (kbd "<f9>") 'refresh-file)

;; Menu bar
(global-set-key (kbd "<f10>") 'menu-bar-open)

;; Toggle frame maximized
(global-set-key (kbd "<f11>") 'toggle-frame-maximized)

;; List buffers
(global-set-key (kbd "<f12>") 'helm-buffers-list)

;; Hint: customize `magit-repo-dirs' so that you can
;; quickly open magit on any one of your projects.
(global-set-key (kbd "C-<f10>") 'helm-ls-git-ls)

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
(global-set-key (kbd "C-=") 'isearch-forward-word-at-point)
(define-key isearch-mode-map (kbd "C-=")
  (lambda ()
    "Reset current isearch to a word-mode search of the word under point."
    (interactive)
    (setq isearch-word t
          isearch-string ""
          isearch-message "")
    (isearch-yank-string (word-at-point))))

;; Use GNU global instead of normal find-tag, fall back to etags-select
(global-set-key (kbd "C-.") (if (and (fboundp 'ggtags-find-tag-dwim)
                                     (executable-find "global"))
                                'ggtags-find-tag-dwim
                              'helm-etags-select))

;; Use Helm instead of 'etags-select-find-tag
(global-set-key (kbd "M-.") 'helm-etags-select)

;; Helm semantic or imenu
(global-set-key (kbd "C-`") 'helm-semantic-or-imenu)

;; Helm buffer list
(global-set-key (kbd "C-0") 'helm-buffers-list)

;; Helm-swoop extension
(global-set-key (kbd "C-*") 'helm-swoop)
(global-set-key [(control kp-multiply)] 'helm-swoop)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "C-*") 'helm-swoop-from-isearch)
(define-key isearch-mode-map [(control kp-multiply)] 'helm-swoop-from-isearch)

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

;; Alternative tabbar keys
(global-set-key (kbd "C-x <up>") 'tabbar-backward-group)
(global-set-key (kbd "C-x <down>") 'tabbar-forward-group)
(global-set-key (kbd "C-x <left>") 'tabbar-forward-tab)
(global-set-key (kbd "C-x <right>") 'tabbar-backward-tab)

;; Jump between windows
(global-set-key [C-up] 'windmove-up)
(global-set-key [C-down] 'windmove-down)
(global-set-key [C-left] 'windmove-left)
(global-set-key [C-right] 'windmove-right)

;; Flycheck tips
(define-key global-map (kbd "C->") 'error-tip-cycle-dwim)
(define-key global-map (kbd "C-<") 'error-tip-cycle-dwim-reverse)

;; Extra Ctrl-x mappings
(global-set-key (kbd "C-x x") 'kill-region)
(define-key ctl-x-map (kbd "x") 'kill-region)
(define-key ctl-x-map (kbd "p") 'yank)
(define-key ctl-x-map (kbd "u") 'unindent-block-or-line)
(define-key ctl-x-map (kbd "SPC") (lambda() (interactive) (push-mark nil nil 1)))
(define-key ctl-x-map (kbd "<up>") 'tabbar-backward-group)
(define-key ctl-x-map (kbd "<down>") 'tabbar-forward-group)
(define-key ctl-x-map (kbd "<left>") 'tabbar-backward-tab)
(define-key ctl-x-map (kbd "<right>") 'tabbar-forward-tab)

;; Overwrite other modes
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
(define-key my-keys-minor-mode-map (kbd "<mouse-3>") 'mouse3-popup-menu)
(define-key my-keys-minor-mode-map [C-tab] 'comment-or-uncomment-region)
(define-key my-keys-minor-mode-map (kbd "M-.") 'helm-etags-select)
(define-key my-keys-minor-mode-map (kbd "<f2>")   'helm-bm)
(define-key my-keys-minor-mode-map (kbd "<C-f2>") 'bm-toggle)
(define-key my-keys-minor-mode-map (kbd "<left-margin> <mouse-1>") 'bm-toggle)
(define-key my-keys-minor-mode-map (kbd "C-b") 'beautify-buffer)
(define-key my-keys-minor-mode-map (kbd "<f4>") 'helm-semantic-or-imenu)
(define-key my-keys-minor-mode-map (kbd "C-`") 'helm-semantic-or-imenu)
(define-key my-keys-minor-mode-map (kbd "<f5>") 'smex)
(define-key my-keys-minor-mode-map (kbd "<f12>") 'helm-buffers-list)
(define-key my-keys-minor-mode-map (kbd "C-0") 'helm-buffers-list)
(define-key my-keys-minor-mode-map (kbd "C-*") 'helm-swoop)

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
