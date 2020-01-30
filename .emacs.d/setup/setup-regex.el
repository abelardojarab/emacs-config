;;; setup-regex.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2020  Abelardo Jara-Berrocal

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
  :bind (("C-%"   . vr/isearch-forward)
         ("C-c q" . vr/query-replace)
         ("C-c m" . vr/mc-mark)))

;; Convert regexps to RX and back
(use-package pcre2el
  :diminish pcre-mode
  :config (progn
            (pcre-mode)
            (rxt-global-mode)))

;; Combine it with Visual Regexp
(use-package visual-regexp-steroids
  :bind (:map esc-map
              (("M-f" . vr/isearch-backward) ;; Esc-M-f
               ("C-f" . vr/isearch-forward))) ;; Esc-C-f
  :custom (vr/engine 'pcre2el))

(provide 'setup-regex)
;;; setup-regex.el ends here
