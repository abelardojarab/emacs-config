;;; setup-regexp.el ---

;; Copyright (C) 2014

;; Author:  <ajaraber@AJARABER-MOBL5>
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

;; if the files are not already in the load path
(add-to-list 'load-path "~/.emacs.d/pcre2el")
(add-to-list 'load-path "~/.emacs.d/visual-regexp")
(add-to-list 'load-path "~/.emacs.d/visual-regexp-steroids")
(require 'pcre2el)
(require 'visual-regexp)
(require 'visual-regexp-steroids)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)

;; to use visual-regexp-steroids's isearch instead of the built-in regexp isearch, also include the following lines:
(when (executable-find "python")
  (define-key esc-map (kbd "C-r") 'vr/isearch-backward) ;; C-M-r
  (define-key esc-map (kbd "C-s") 'vr/isearch-forward) ;; C-M-s
  ) ;; when

(provide 'setup-regexp)
;; setup-regexp.el ends here
