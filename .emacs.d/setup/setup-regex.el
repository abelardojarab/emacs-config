;;; setup-regex.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2018  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojarab@gmail.com>
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

;; visual regexp replace
(use-package visual-regexp
  :load-path (lambda () (expand-file-name "visual-regexp/" user-emacs-directory))
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)
         ("C-c m" . vr/mc-mark)))

;; Convert regexps to RX and back
(use-package pcre2el
  :load-path (lambda () (expand-file-name "pcre2el/" user-emacs-directory))
  :diminish pcre-mode
  :config (progn
            (pcre-mode)
            (rxt-global-mode)))

;; Combine it with Visual Regexp
(use-package visual-regexp-steroids
  :load-path (lambda () (expand-file-name "visual-regexp-steroids/" user-emacs-directory))
  ;; to use visual-regexp-steroids's isearch instead of the built-in regexp isearch:
  :bind (:map esc-map
              (("M-f" . vr/isearch-backward) ;; Esc-M-f
               ("C-f" . vr/isearch-forward))) ;; Esc-C-f
  :config (custom-set-variables
   '(vr/engine (quote pcre2el))))

(provide 'setup-regex)
;;; setup-regex.el ends here
