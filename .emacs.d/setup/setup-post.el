;;; setup-post.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2016  abelardo.jara-berrocal

;; Author: abelardo.jara-berrocal <ajaraber@plxc20122.pdx.intel.com>
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

;; iMenu
(set-default 'imenu-auto-rescan t)
(add-hook 'lisp-mode-hook
         (lambda ()
           (setq imenu-create-index-function 'imenu-example--create-lisp-index)
           (setq imenu-generic-expression scheme-imenu-generic-expression)))

(mapc (lambda (mode)
       (add-hook mode 'imenu-add-menubar-index))
      '(prog-mode-hook
        reftex-mode-hook
        reftex-load-hook
        org-mode-hook))

;; iMenus
(add-to-list 'load-path "~/.emacs.d/imenus")
(autoload 'imenus "imenus" nil t)
(autoload 'imenus-mode-buffers "imenus" nil t)

;; Helm-bibtex
(add-to-list 'load-path "~/.emacs.d/ebib")
(add-to-list 'load-path "~/.emacs.d/parsebib")
(add-to-list 'load-path "~/.emacs.d/helm-bibtex")
(require 'ebib)
(require 'parsebib)
(require 'helm-bibtex)
(defun helm-bibtex-cite ()
  "Helm command to cite bibliography."
  (interactive)
  (helm-other-buffer
   '(helm-c-source-bibtex)
   "*helm bibtex:"))

;; Org-Ref
(add-to-list 'load-path "~/.emacs.d/org-ref")
(require 'org-ref)
(setq org-ref-bibliography-notes "~/workspace/Documents/Bibliography/notes.org"
      org-ref-default-bibliography '("~/workspace/Documents/Bibliography/biblio.bib")
      org-ref-pdf-directory "~/workspace/Documents/Bibliography/bibtex-pdfs")
(setq org-ref-insert-cite-key "C-c )")
(setq org-ref-default-citation-link "autocite")

;; Enable GUI features
(setq use-file-dialog t)
(setq use-dialog-box t)

(provide 'setup-post)
;;; setup-post.el ends here
