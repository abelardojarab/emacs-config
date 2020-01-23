;;; setup-latex.el ---                           -*- lexical-binding: t; -*-

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

;; dont fetch from melpa
(use-package auctex
  :defer t
  :load-path (lambda () (expand-file-name "auctex/" user-emacs-directory))
  :commands (LaTeX-math-mode
             TeX-source-correlate-mode
             my/latex-mode-init)
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook (LaTeX-mode . my/latex-mode-init)
  :custom ((TeX-auto-save                     t)
           (TeX-parse-self                    t)
           (TeX-save-query                    nil)
           (TeX-PDF-mode                      t)
           (TeX-source-correlate-start-server t)
           (TeX-master                        nil))
  :preface (defun my/latex-mode-init ()
             "Tweaks and customisations for LaTeX mode."
             ;; Auto-fill for LaTeX
             (turn-on-auto-fill)
             (set-fill-column 80)
             (setq default-justification 'left)
             ;; enable spell checking
             (flyspell-mode 1)
             ;; enable source-correlate for Control-click forward/reverse search.
             (TeX-source-correlate-mode 1)
             ;; enable math mode in latex
             (LaTeX-math-mode 1)
             ;; enable math mode in latex
             (LaTeX-preview-setup)
             ;; Enable reftex
             (turn-on-reftex)))

(use-package reftex
  :defer t
  :commands turn-on-reftex
  :custom (reftex-plug-into-AUCTeX t))

(use-package bibtex
  :defer t
  :mode ("\\.bib" . bibtex-mode)
  :init (progn
          (setq bibtex-completion-bibliography (list my/bibtex-completion-bibliography)
                bibtex-completion-library-path my/bibtex-completion-library-path
                bibtex-completion-notes-path my/bibtex-completion-notes)

          (setq bibtex-align-at-equal-sign t)))

(use-package ebib
  :demand t
  :init (progn
          ;; Restore legacy code
          (eval-and-compile
            (define-prefix-command 'ebib-prefix-map)
            (suppress-keymap ebib-prefix-map)
            (defvar ebib-prefixed-functions '(ebib-delete-entry
                                              ebib-latex-entries
                                              ebib-mark-entry
                                              ebib-print-entries
                                              ebib-push-bibtex-key
                                              ebib-export-entry)))

          ;; macro to redefine key bindings.
          (defmacro ebib-key (buffer key &optional command)
            (cond
             ((eq buffer 'index)
              (let ((one `(define-key ebib-index-mode-map ,key (quote ,command)))
                    (two (when (or (null command)
                                   (member command ebib-prefixed-functions))
                           `(define-key ebib-prefix-map ,key (quote ,command)))))
                (if two
                    `(progn ,one ,two)
                  one)))
             ((eq buffer 'entry)
              `(define-key ebib-entry-mode-map ,key (quote ,command)))
             ((eq buffer 'strings)
              `(define-key ebib-strings-mode-map ,key (quote ,command)))
             ((eq buffer 'mark-prefix)
              `(progn
                 (define-key ebib-index-mode-map (format "%c" ebib-prefix-key) nil)
                 (define-key ebib-index-mode-map ,key 'ebib-prefix-map)
                 (setq ebib-prefix-key (string-to-char ,key))))
             ((eq buffer 'multiline)
              `(progn
                 (define-key ebib-multiline-mode-map "\C-c" nil)
                 (mapc #'(lambda (command)
                           (define-key ebib-multiline-mode-map (format "\C-c%s%c" ,key (car command)) (cdr command)))
                       '((?q . ebib-quit-multiline-edit)
                         (?c . ebib-cancel-multiline-edit)
                         (?s . ebib-save-from-multiline-edit)))
                 (setq ebib-multiline-key (string-to-char ,key)))))))
  :commands ebib-key)

(use-package ebib-handy
  :defer t
  :after ebib
  :load-path (lambda () (expand-file-name "ebib-handy/" user-emacs-directory))
  :commands ebib-handy
  :config (progn
            (ebib-handy-enable)
            (setq ebib-extra-fields
                  '((BibTeX "keywords" "abstract" "timestamp"
                            "file"  "url" "crossref" "annote" "doi")
                    (biblatex "keywords" "abstract" "timestamp"
                              "file"  "url" "crossref" "annote" "doi")))))

(use-package latex-pretty-symbols
  :defer t
  :commands latex-unicode-simplified
  :hook ((LaTeX-mode latex-mode) . latex-unicode-simplified))

(use-package magic-latex-buffer
  :defer t
  :commands magic-latex-buffer
  :hook (latex-mode . magic-latex-buffer)
  :custom ((magic-latex-enable-block-highlight t)
           (magic-latex-enable-suscript        t)
           (magic-latex-enable-pretty-symbols  t)
           (magic-latex-enable-block-align     nil)
           (magic-latex-enable-inline-image    nil)
           (magic-latex-enable-minibuffer-echo t)))

;; Fast input methods for LaTeX environments and math
(use-package cdlatex
  :defer t)

(provide 'setup-latex)
;;; setup-latex.el ends here
