;;; setup-latex.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2017  Abelardo Jara-Berrocal

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

(use-package auctex
  :load-path (lambda () (expand-file-name "auctex/" user-emacs-directory))
  :init (progn
          (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
          (add-hook 'LaTeX-mode-hook #'flyspell-mode)
          (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
          (setq TeX-auto-save t
                TeX-parse-self t
                TeX-save-query nil
                TeX-PDF-mode t)
          (setq-default TeX-master nil)

          ;; Auto-fill for LaTeX
          (defun my/latex-auto-fill ()
            "Turn on auto-fill for LaTeX mode."
            (turn-on-auto-fill)
            (set-fill-column 80)
            (setq default-justification 'left))
          (add-hook 'LaTeX-mode-hook #'my/latex-auto-fill)))

(use-package reftex
  :commands turn-on-reftex
  :init (progn
          (setq reftex-plug-into-AUCTeX t)))

(use-package bibtex
  :mode ("\\.bib" . bibtex-mode)
  :init (progn
          (setq bibtex-completion-bibliography (list my/bibtex-completion-bibliography)
                bibtex-completion-library-path my/bibtex-completion-library-path
                bibtex-completion-notes-path my/bibtex-completion-notes)

          (setq bibtex-align-at-equal-sign t)
          (add-hook 'bibtex-mode-hook (lambda () (set-fill-column 120)))))

(use-package ebib-handy
  :load-path (lambda () (expand-file-name "ebib-handy/" user-emacs-directory))
  :bind ("C-c b" . ebib-handy)
  :config (progn
            (ebib-handy-enable)
            (setq ebib-extra-fields
                  '((BibTeX "keywords" "abstract" "timestamp"
                            "file"  "url" "crossref" "annote" "doi")
                    (biblatex "keywords" "abstract" "timestamp"
                              "file"  "url" "crossref" "annote" "doi")))))

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

;; Fast input methods for LaTeX environments and math
(use-package cdlatex
  :load-path (lambda () (expand-file-name "cdlatex/" user-emacs-directory)))

(provide 'setup-latex)
;;; setup-latex.el ends here
