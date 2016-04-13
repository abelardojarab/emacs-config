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
  :bind (("M-s ." . isearch-forward-symbol-at-point)
         ("M-s ," . isearch-forward-word-at-point))
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
          (user-error "No symbol at point"))))))

(provide 'setup-search)
;;; setup-search.el ends here
