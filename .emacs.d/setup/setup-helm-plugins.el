;;; setup-helm-plugins.el ---                        -*- lexical-binding: t; -*-

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

;; Helm desc-binds
(use-package helm-descbinds
  :load-path (lambda () (expand-file-name "helm-descbinds/" user-emacs-directory))
  :bind (:map ctl-x-map
              ("k" . helm-descbinds))
  :config (progn
            (helm-descbinds-mode 1)))

;; Helm flycheck
(use-package helm-flycheck
  :load-path (lambda () (expand-file-name "helm-flycheck/" user-emacs-directory))
  :bind (:map ctl-x-map
              ("c" . helm-flycheck)))

;; Helm flyspell
(use-package helm-flyspell
  :load-path (lambda () (expand-file-name "helm-flyspell/" user-emacs-directory))
  :bind (:map ctl-x-map
              ("s" . helm-flyspell-correct)))

;; Helm ls git
(use-package helm-ls-git
  :load-path (lambda () (expand-file-name "helm-ls-git/" user-emacs-directory))
  :bind (:map ctl-x-map
              ("g" . helm-ls-git-ls)))

;; Helm bm support
(use-package helm-bm
  :load-path (lambda () (expand-file-name "helm-bm/" user-emacs-directory))
  :bind (:map ctl-x-map
              ("b" . helm-bookmarks)))

;; Helm etags plus
(use-package helm-etags+
  :load-path (lambda () (expand-file-name "helm-etags-plus/" user-emacs-directory))
  :bind (:map ctl-x-map
              ("t" . helm-etags-select)))

;; Helm yasnippet
(use-package helm-c-yasnippet
  :load-path (lambda () (expand-file-name "helm-c-yasnippet/" user-emacs-directory))
  :bind (:map ctl-x-map
              ("y" . helm-yas-complete)))

;; Helm make support
(use-package helm-make
  :load-path (lambda () (expand-file-name "helm-make/" user-emacs-directory))
  :bind (:map ctl-x-map
              ("m" . helm-make)))

;; Helm dash
(use-package helm-dash
  :load-path (lambda () (expand-file-name "helm-dash/" user-emacs-directory))
  :bind (:map ctl-x-map
              ("d" . helm-dash))
  :config (progn
            (setq helm-dash-enable-debugging nil)
            (setq helm-dash-min-length 2)
            (setq helm-dash-docsets-path (expand-file-name "docsets/" user-emacs-directory))
            (setq helm-dash-common-docsets '(
                                             ;; "C" ;; does not work
                                             ;; "C++" ;; does not work
                                             ;; "Bash" ;; does not work
                                             ;; "LaTeX" ;; does not work
                                             "Emacs_Lisp" ;; works
                                             "Python_2" ;; works
                                             ;; "NumPy" ;; does not work
                                             ;; "SciPy" ;; does not work
                                             ;; "JavaScript" ;; does not work
                                             ;; "NodeJS";; does not work
                                             ;; "HTML";; does not work
                                             ;; "Java";; does not work
                                             ))

            ;; (defun lisp-doc-hook ()
            ;;   (interactive)
            ;;   (setq-local helm-dash-docsets '("Emacs_Lisp")))
            ;; (add-hook 'lisp-mode-hook 'lisp-doc-hook)

            ))

;; Helm bibtex
(use-package helm-bibtex
  :load-path (lambda () (expand-file-name "helm-bibtex/" user-emacs-directory))
  :config (progn
            (setq helm-bibtex-bibliography "~/workspace/Documents/Bib/biblio.bib")
            (setq helm-bibtex-library-path "~/workspace/Documents/Bib/bibtex-pdfs")
            (setq helm-bibtex-notes-path "~/workspace/Documents/Bib/notes.org")

            ;; open pdf with system pdf viewer (works on mac)
            (setq helm-bibtex-pdf-open-function
                  (lambda (fpath)
                    (start-process "open" "*open*" "open" fpath)))

            (defun helm-bibtex-cite ()
              "Helm command to cite bibliography."
              (interactive)
              (helm-other-buffer
               '(helm-c-source-bibtex)
               "*helm bibtex:"))))

;; Org-Ref
(use-package org-ref
  :load-path (lambda () (expand-file-name "org-ref/" user-emacs-directory))
  :config (progn
            (setq org-ref-bibliography-notes "~/workspace/Documents/Bib/notes.org"
                  org-ref-default-bibliography '("~/workspace/Documents/Bib/biblio.bib")
                  org-ref-pdf-directory "~/workspace/Documents/Bib/bibtex-pdfs")
            (setq org-ref-insert-cite-key "C-c [")
            (setq org-ref-default-citation-link "autocite")))

;; Helm themes
(use-package helm-themes
  :load-path (lambda () (expand-file-name "helm-themes/" user-emacs-directory)))

;; Helm swoop
(use-package helm-swoop
  :load-path (lambda () (expand-file-name "helm-swoop/" user-emacs-directory))
  :config (progn

            ;; From helm-swoop to helm-multi-swoop-all
            (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

            ;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
            (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

            ;; Move up and down like isearch
            (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
            (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
            (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
            (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

            ;; Save buffer when helm-multi-swoop-edit complete
            (setq helm-multi-swoop-edit-save t)

            ;; If this value is t, split window inside the current window
            (setq helm-swoop-split-with-multiple-windows nil)

            ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
            (setq helm-swoop-split-direction 'split-window-horizontally)

            ;; If nil, you can slightly boost invoke speed in exchange for text color
            (setq helm-swoop-speed-or-color nil)

            ;; ;; Go to the opposite side of line from the end or beginning of line
            (setq helm-swoop-move-to-line-cycle t)))

(provide 'setup-helm-plugins)
;;; setup-helm-plugins.el ends here
