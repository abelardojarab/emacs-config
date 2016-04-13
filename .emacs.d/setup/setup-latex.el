;;; setup-latex.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Abelardo Jara

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

(use-package latex-pretty-symbols
  :commands latex-unicode-simplified
  :load-path (lambda () (expand-file-name "latex-pretty-symbols/" user-emacs-directory))
  :init (progn
          ;;AUCTeX
          (add-hook 'LaTeX-mode-hook 'latex-unicode-simplified)

          ;;latex-mode
          (add-hook 'latex-mode-hook 'latex-unicode-simplified)))

(use-package magic-latex-buffer
  :commands magic-latex-buffer
  :load-path (lambda () (expand-file-name "magic-latex-buffer/" user-emacs-directory))
  :init (add-hook 'latex-mode-hook 'magic-latex-buffer)
  :config (setq magic-latex-enable-block-highlight t
                magic-latex-enable-suscript        t
                magic-latex-enable-pretty-symbols  t
                magic-latex-enable-block-align     nil
                magic-latex-enable-inline-image    nil
                magic-latex-enable-minibuffer-echo t))

(provide 'setup-latex)
;;; setup-latex.el ends here
