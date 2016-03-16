;;; setup-general.el ---

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

;; Popup, used by auto-complete and other tools
(use-package popup
  ;; We don't ensure this package, because we definitely don't want to have this
  ;; mess, but unfortunately it's a dependency of Ensime :(
  :load-path "~/.emacs.d/popup"
  :config (progn
            (setq popup-use-optimized-column-computation t)))

;; Fix indent guide issue
;; (defvar sanityinc/indent-guide-mode-suppressed nil)
;; (defadvice popup-create (before indent-guide-mode activate)
;;   "Suspend indent-guide-mode while popups are visible"
;;   (let ((indent-guide-enabled (and (boundp 'indent-guide-mode) indent-guide-mode)))
;;     (set (make-local-variable 'sanityinc/indent-guide-mode-suppressed) indent-guide-mode)
;;     (when indent-guide-enabled
;;       (indent-guide-mode nil))))
;; (defadvice popup-delete (after indent-guide-mode activate)
;;   "Restore indent-guide-mode when all popups have closed"
;;   (let ((indent-guide-enabled (and (boundp 'indent-guide-mode) indent-guide-mode)))
;;     (when (and (not popup-instances) sanityinc/indent-guide-mode-suppressed)
;;       (setq sanityinc/indent-guide-mode-suppressed nil)
;;       (indent-guide-mode 1))))

;; Pos-tip library
(use-package pos-tip
  :defer t
  :load-path "~/.emacs.d/pos-tip")

;; Drop down list support, related to popup
(use-package dropdown-list)

;; Manage popup windows
(use-package popwin
  :load-path "~/.emacs.d/popwin"
  :config  (progn
             (defvar popwin:special-display-config-backup popwin:special-display-config)
             (setq display-buffer-function 'popwin:display-buffer)

             ;; basic
             (push '("*Help*" :stick t :noselect t) popwin:special-display-config)
             (push '("*helm world time*" :stick t :noselect t) popwin:special-display-config)
             (push '("*Pp Eval Output*" :stick t) popwin:special-display-config)

             ;; magit
             (push '("*magit-process*" :stick t) popwin:special-display-config)

             ;; quickrun
             (push '("*quickrun*" :stick t) popwin:special-display-config)

             ;; dictionaly
             (push '("*dict*" :stick t) popwin:special-display-config)
             (push '("*sdic*" :stick t) popwin:special-display-config)

             ;; popwin for slime
             (push '(slime-repl-mode :stick t) popwin:special-display-config)

             ;; man
             (push '(Man-mode :stick t :height 20) popwin:special-display-config)

             ;; Elisp
             (push '("*ielm*" :stick t) popwin:special-display-config)
             (push '("*eshell pop*" :stick t) popwin:special-display-config)

             ;; pry
             (push '(inf-ruby-mode :stick t :height 20) popwin:special-display-config)

             ;; python
             (push '("*Python*"   :stick t) popwin:special-display-config)
             (push '("*Python Help*" :stick t :height 20) popwin:special-display-config)
             (push '("*jedi:doc*" :stick t :noselect t) popwin:special-display-config)

             ;; Haskell
             (push '("*haskell*" :stick t) popwin:special-display-config)
             (push '("*GHC Info*") popwin:special-display-config)

             ;; sgit
             (push '("*sgit*" :position right :width 0.5 :stick t)
                   popwin:special-display-config)

             ;; git-gutter
             (push '("*git-gutter:diff*" :width 0.5 :stick t)
                   popwin:special-display-config)

             ;; es-results-mode
             (push '(es-result-mode :stick t :width 0.5)
                   popwin:special-display-config)

             ;; direx
             (push '(direx:direx-mode :position left :width 40 :dedicated t)
                   popwin:special-display-config)

             (push '("*Occur*" :stick t) popwin:special-display-config)

             ;; prodigy
             (push '("*prodigy*" :stick t) popwin:special-display-config)

             ;; malabar-mode
             (push '("*Malabar Compilation*" :stick t :height 30)
                   popwin:special-display-config)

             ;; org-mode
             (push '("*Org tags*" :stick t :height 30)
                   popwin:special-display-config)

             ;; Completions
             (push '("*Completions*" :stick t :noselect t) popwin:special-display-config)

             ;; ggtags
             (push '("*ggtags-global*" :stick t :noselect t :height 30) popwin:special-display-config)

             ;; async shell commands
             (push '("*Async Shell Command*" :stick t) popwin:special-display-config)

             (defun my/popup-downloads ()
               "Pop up the downloads buffer (3rd eshell buffer for me"
               (interactive)
               (popwin:popup-buffer "*eshell*<3>"))

             ;; eshell 3 is always my "download stuff" buffer
             (global-set-key (kbd "C-x M-d") #'my/popup-downloads)
             (global-set-key (kbd "C-h e") 'popwin:messages)

             ;; popwin conflicts with ecb
             (popwin-mode -1)))

;; Better search, similar to vim
(use-package isearch+
  :init (progn
          (require 'isearch-prop))
  :config (progn
            (defun my-isearch-yank-word-hook ()
              (when (equal this-command 'my-isearch-word-at-point)
                (let ((string (concat "\\<"
                                      (buffer-substring-no-properties
                                       (progn (skip-syntax-backward "w_") (point))
                                       (progn (skip-syntax-forward "w_") (point)))
                                      "\\>")))
                  (if (and isearch-case-fold-search
                           (eq 'not-yanks search-upper-case))
                      (setq string (downcase string)))
                  (setq isearch-string string
                        isearch-message
                        (concat isearch-message
                                (mapconcat 'isearch-text-char-description
                                           string ""))
                        isearch-yank-flag t)
                  (isearch-search-and-update))))
            (add-hook 'isearch-mode-hook 'my-isearch-yank-word-hook)

            ;; Keep the search results in the center in incremental search
            (defadvice isearch-repeat-forward (after isearch-repeat-forward-recenter activate)
              (recenter))
            (defadvice isearch-repeat-backward (after isearch-repeat-backward-recenter activate)
              (recenter))
            (ad-activate 'isearch-repeat-forward)
            (ad-activate 'isearch-repeat-backward)))

;; Search at point
(use-package thingatpt
  :defer 1
  :bind (("M-s ." . my-isearch-forward-symbol-at-point)
         ("M-s ," . my-isearch-forward-word-at-point))
  :config
  (progn
    (defun my-isearch-forward-word-at-point ()
      "Search for word at point."
      (interactive)
      (let ((word (thing-at-point 'word t))
            (bounds (bounds-of-thing-at-point 'word)))
        (if word
            (progn
              (isearch-mode t nil nil nil t)
              (when (< (car bounds) (point))
                (goto-char (car bounds)))
              (isearch-yank-string word))
          (user-error "No word at point"))))

    (defun my-isearch-forward-symbol-at-point ()
      "Search for symbol at point."
      (interactive)
      (let ((symbol (thing-at-point 'symbol t))
            (bounds (bounds-of-thing-at-point 'symbol)))
        (if symbol
            (progn
              (isearch-mode t nil nil nil 'isearch-symbol-regexp)
              (when (< (car bounds) (point))
                (goto-char (car bounds)))
              (isearch-yank-string symbol))
          (user-error "No symbol at point"))))))

;; Turn on subword-mode for non-lispy languages
(use-package subword
  :config (progn (mapc (lambda (mode)
                         (add-hook mode 'subword-mode))
                       '(c-common-mode-hook
                         python-mode-hook
                         js2-mode-hook
                         java-mode-hook)))
  :diminish subword-mode)

;; Uniquify-buffers
(use-package uniquify
  :config (progn
            (setq
             uniquify-buffer-name-style 'post-forward
             uniquify-separator ":"
             ;; rename after killing uniquified
             uniquify-after-kill-buffer-p t
             ;; don't muck with special buffers
             uniquify-ignore-buffers-re "^\\*")))

;; imenu list
(add-to-list 'load-path "~/.emacs.d/imenu-list")
(use-package imenu-list
  :load-path "~/.emacs.d/imenu-list"
  :config (progn
            (setq imenu-list-size 0.2)
            (setq imenu-list-focus-after-activation t)
            (setq imenu-list-auto-resize t)
            (setq imenu-list-position 'right)))

;; Browse kill ring
(use-package browse-kill-ring
  :load-path "~/.emacs.d/browse-kill-ring")

;; Multiple cursors
(use-package multiple-cursors
  :load-path "~/.emacs.d/multiple-cursors")

;; Abbrevs
(use-package abbrev
  :diminish abbrev-mode
  :init (progn
          (setq abbrev-file-name "~/.emacs.cache/abbrev_defs")
          (if (file-exists-p abbrev-file-name)
              (quietly-read-abbrev-file))
          (add-hook 'kill-emacs-hook
                    'write-abbrev-file))

  :config (progn
            ;; Activate template autocompletion
            (abbrev-mode t)
            (setq save-abbrevs t)
            (dolist (hook '(prog-mode-hook
                            markdown-mode-hook
                            org-mode-hook
                            text-mode-hook))
              (add-hook hook (lambda () (abbrev-mode 1))))))

;; log4e
(use-package log4e
  :load-path "~/.emacs.d/log4e")

;; yaxception
(use-package yaxception
  :load-path "~/.emacs.d/yaxception")

(provide 'setup-general)
;;; setup-general.el ends here
