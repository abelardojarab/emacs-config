;;; setup-search.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2016  abelardo.jara-berrocal

;; Author: Abelardo Jara <abelardojara@Abelardos-MacBook-Pro.local>
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

;; Better search, similar to vim
(use-package isearch-prop
  :config (progn
            ;; Activate highlight in search and replace
            (setq search-highlight t)
            (setq query-replace-highlight t)

            ;; exit search mode with any key
            (setq search-exit-option t)

            ;; Keep the search results in the center in incremental search
            (defadvice isearch-repeat-forward (after isearch-repeat-forward-recenter activate)
              (recenter))
            (defadvice isearch-repeat-backward (after isearch-repeat-backward-recenter activate)
              (recenter))
            (ad-activate 'isearch-repeat-forward)
            (ad-activate 'isearch-repeat-backward)

            ;; search forward with Ctrl-f
            (global-set-key [(control f)] 'isearch-forward)
            ;; (define-key isearch-mode-map [(control f)] (lookup-key isearch-mode-map "\C-s"))
            ;; (define-key minibuffer-local-isearch-map [(control f)]
            ;;   (lookup-key minibuffer-local-isearch-map "\C-s"))

            ;; search backward with Alt-f
            (global-set-key [(meta f)] 'isearch-backward)
            (define-key isearch-mode-map [(meta f)] (lookup-key isearch-mode-map "\C-r"))
            (define-key minibuffer-local-isearch-map [(meta f)]
              (lookup-key minibuffer-local-isearch-map "\C-r"))


            ;; Paste text into search buffer with C-v
            (defun isearch-yank-symbol ()
              (interactive)
              (isearch-yank-internal (lambda () (forward-symbol 1) (point))))
            (define-key minibuffer-local-isearch-map (kbd "C-v") 'isearch-yank-pop)
            (define-key isearch-mode-map (kbd "C-v") 'isearch-yank-pop)))

;; Search at point
(use-package thingatpt
  :bind (("C-=" . isearch-forward-word-at-point)
         :map isearch-mode-map
         ("C-=" . isearch-forward-word-at-point-isearch))
  :commands (isearch-forward-word-at-point isearch-forward-symbol-at-point)
  :config
  (progn
    (defun isearch-forward-word-at-point ()
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

    (defun isearch-forward-symbol-at-point ()
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
          (user-error "No symbol at point"))))

    ;; Search at point
    (defun isearch-forward-word-at-point-isearch ()
        "Reset current isearch to a word-mode search of the word under point."
        (interactive)
        (setq isearch-word t
              isearch-string ""
              isearch-message "")
        (isearch-yank-string (word-at-point)))))

;; Simple search with avy
(use-package avy
  :defer t
  :load-path (lambda () (expand-file-name "avy/" user-emacs-directory))
  :config (setq avy-background t))

(provide 'setup-search)
;;; setup-search.el ends here
