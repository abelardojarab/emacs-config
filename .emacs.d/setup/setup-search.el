;;; setup-search.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2016  abelardo.jara-berrocal

;; Author: abelardo.jara-berrocal <ajaraber@plxcj9063.pdx.intel.com>
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
(use-package isearch+
  :init (progn
          (require 'isearch-prop))
  :config (progn
            ;; Activate highlight in search and replace
            (setq search-highlight t)
            (setq query-replace-highlight t)
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

(provide 'setup-search)
;;; setup-search.el ends here
