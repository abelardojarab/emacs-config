;; setup-helm-plugins.el ---                        -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018, 2020  Abelardo Jara-Berrocal

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

;; helm grepint
(use-package helm-grepint
  :defer t
  :after helm
  :if (not (executable-find "rg"))
  :commands (helm-grepint-grep)
  :bind (:map ctl-x-map
              ("a" . helm-grepint-grep))
  :config (helm-grepint-set-default-config))

;; helm ripgrep
(use-package helm-rg
  :defer t
  :after helm
  :if (executable-find "rg")
  :commands helm-rg
  :bind (:map ctl-x-map
              ("a" . helm-rg)))

;; helm elscreen
(use-package helm-elscreen
  :defer t
  :after (helm elscreen))

;; helm git-grep
(use-package helm-git-grep
  :defer t
  :after (helm helm-elscreen)
  :commands (helm-git-grep)
  :custom (helm-git-grep-candidate-number-limit nil))

;; helm ag/ack
(use-package helm-ag
  :defer t
  :after helm
  :commands (helm-ag
             helm-do-ag
             helm-do-ag-this-file
             helm-do-ag-project-root)
  :config (progn
            ;; Fallback to ack if the silver searcher is not found
            (unless (or (executable-find "ag")
                        (executable-find "pt"))
              (setq helm-ag-base-command "awk --nocolor --nogroup"))))

;; helm line search with ag
(use-package helm-lines
  :defer t
  :if (executable-find "ag")
  :after helm
  :commands helm-lines)

;; helm describe modes
(use-package helm-describe-modes
  :defer t
  :after helm
  :commands (helm-describe-modes)
  :bind ([remap describe-mode] . helm-describe-modes))

;; helm desc-binds
(use-package helm-descbinds
  :defer t
  :after helm
  :commands (helm-descbinds helm-descbinds-mode)
  :bind (:map ctl-x-map
              ("k" . helm-descbinds))
  :init (helm-descbinds-mode 1))

;; helm flycheck
(use-package helm-flycheck
  :defer t
  :after (helm flycheck)
  :commands (helm-flycheck)
  :bind (:map ctl-x-map
              ("e" . helm-flycheck)))

;; helm flyspell
(use-package helm-flyspell
  :defer t
  :after (helm flyspell)
  :commands (helm-flyspell-correct)
  :bind (([remap ispell-word] . helm-flyspell-correct)
         :map ctl-x-map
         (";" . helm-flyspell-correct)))

;; helm ls git
(use-package helm-ls-git
  :defer t
  :commands (helm-ls-git-ls helm-browse-project)
  :bind (:map ctl-x-map
              ("l" . helm-browse-project)))

;; helm bm support
(use-package helm-bm
  :defer t
  :commands (helm-bm)
  :after (helm bm)
  :bind (("C-c b" . helm-bm)
         :map ctl-x-map
         ("m"     . helm-bm))
  :config (setq helm-bookmark-show-location t))

;; helm etags plus
(use-package helm-etags+
  :defer t
  :after (helm etags)
  :commands (helm-etags-select)
  :bind ("M-." . helm-etags-select))

;; helm gtags
(use-package helm-gtags
  :defer t
  :after (helm ggtags)
  :commands (helm-gtags-select
             helm-gtags-dwim
             helm-gtags-mode)
  :if (executable-find "global")
  :bind (("C-." . helm-gtags-dwim)
         :map ctl-x-map
         ("." . helm-gtags-dwim))
  :hook (c-mode-common . helm-gtags-mode)
  :custom ((helm-gtags-ignore-case            t)
           (helm-gtags-auto-update            t)
           (helm-gtags-use-input-at-cursor    t)
           (helm-gtags-pulse-at-cursor        t)
           (helm-gtags-prefix-key             "\C-cg")
           (helm-gtags-suggested-key-mapping  t)))

;; helm xref
(use-package helm-xref
  :after helm
  :if (and (executable-find "global")
           (boundp 'xref-backend-functions))
  :custom (xref-show-xrefs-function 'helm-xref-show-xrefs))

;; helm yasnippet
(use-package helm-c-yasnippet
  :demand t
  :commands (helm-yas-complete helm-c-yas-complete)
  :defines helm-c-yas-complete)

;; helm make support
(use-package helm-make
  :defer t
  :after helm
  :commands (helm-make))

;; Biblio (helm-bibtex requirement)
;; extensible Emacs package for browsing and fetching references
(use-package biblio
  :after helm
  :defer t)

;; helm bibtex
(use-package helm-bibtex
  :defer t
  :after (helm biblio)
  :commands (helm-bibtex)
  :bind (:map ctl-x-map
              ("[" . helm-bibtex))
  :config (progn
            (setq helm-bibtex-bibliography my/bibtex-completion-bibliography
                  helm-bibtex-library-path my/bibtex-completion-library-path
                  helm-bibtex-notes-path my/bibtex-completion-notes)

            ;; open pdf with system pdf viewer (works on mac)
            (setq helm-bibtex-pdf-open-function
                  (lambda (fpath)
                    (start-process "open" "*open*" "open" fpath)))

            (defun helm-bibtex-cite ()
              "helm command to cite bibliography."
              (interactive)
              (helm-other-buffer
               '(helm-c-source-bibtex)
               "*helm bibtex:"))))

;; Org-Ref
(use-package org-ref
  :defer t
  :commands (org-ref-insert-cite-key)
  :after (helm async org)
  :custom ((org-ref-insert-cite-key "C-c [")
           (org-ref-default-citation-link "autocite"))
  :config (setq org-ref-default-bibliography (list my/bibtex-completion-bibliography)
                org-ref-bibliography-files (list my/bibtex-completion-bibliography)
                org-ref-pdf-directory my/bibtex-completion-library-path
                org-ref-bibliography-notes my/bibtex-completion-notes))

;; helm themes
(use-package helm-themes
  :defer t
  :after helm
  :commands (helm-themes))

;; helm dash
(use-package helm-dash
  :defer t
  :after (helm dash)
  :if (executable-find "sqlite3")
  :commands (helm-dash
             dash-load-org
             dash-load-git
             dash-load-bash
             dash-load-c
             dash-load-c++
             dash-load-py
             dash-load-js
             dash-load-md)
  :hook ((org-mode          . dash-load-org)
         (markdown-mode     . dash-load-md)
         (c-mode            . dash-load-c)
         (sh-mode           . dash-load-bash)
         (c++-mode          . dash-load-c++)
         (js2-mode          . dash-load-js)
         (ess-mode          . dash-load-r)
         (emacs-lisp-mode   . dash-load-elisp))
  :bind (:map ctl-x-map
              ("d" . helm-dash))
  :config (progn
            (setq helm-dash-enable-debugging nil)
            (setq helm-dash-min-length 2)
            (setq helm-dash-docsets-path (expand-file-name "docsets/" user-emacs-directory))
            (setq helm-dash-common-docsets '("Git"
                                             "Emacs_Lisp"))

            ;; Mode-specific hooks
            (defun dash-load-git ()
              (interactive)
              (setq-local helm-dash-docsets '("Git")))

            (defun dash-load-org ()
              (interactive)
              (setq-local helm-dash-docsets '("Org")))

            (defun dash-load-cmake ()
              (interactive)
              (setq-local helm-dash-docsets '("CMake")))

            (defun dash-load-elisp ()
              (interactive)
              (setq-local helm-dash-docsets '("Emacs_Lisp")))

            (defun dash-load-c ()
              (interactive)
              (setq-local helm-dash-docsets '("C")))

            (defun dash-load-c++ ()
              (interactive)
              (setq-local helm-dash-docsets '("C++"
                                              "GLib")))

            (defun dash-load-py ()
              (interactive)
              (setq-local helm-dash-docsets '("Python_2"
                                              "Pandas")))

            (defun dash-load-js ()
              (interactive)
              (setq-local helm-dash-docsets '("JavaScript"
                                              "NodeJS"
                                              "AngularJS")))

            (defun dash-load-r ()
              (interactive)
              (setq-local helm-dash-docsets '("R")))

            (defun dash-load-bash ()
              (interactive)
              (setq-local helm-dash-docsets '("Bash")))

            (defun dash-load-md ()
              (interactive)
              (setq-local helm-dash-docsets '("Markdown")))))

;; helm company
(use-package helm-company
  :defer t
  :bind (:map ctl-x-map
              (":" . helm-company)
              :map company-mode-map
              ("C-:" . helm-company))
  :after (helm company)
  :commands (helm-company))

;; helm pages
;; Text is divided into pages delimited by the formfeed character (ASCII code 12, also denoted as ‘control-L’)
(use-package helm-pages
  :defer t
  :after helm
  :commands helm-pages)

;; helm integration with magit
(use-package helm-magit
  :defer t
  :commands (helm-magit:checkout helm-magit:diff)
  :if (executable-find "git")
  :after (helm magit))

;; helm gitignore generation
(use-package helm-gitignore
  :defer t
  :after (helm magit)
  :commands helm-gitignore
  :after (helm git-modes request))

;; helm hunks
(use-package helm-hunks
  :defer t
  :commands (helm-hunks
             helm-hunks-current-buffer
             helm-hunks-staged
             helm-hunks-staged-current-buffer)
  :after (helm git-modes request))

;; helm pass
(use-package helm-pass
  :defer t
  :if (and (equal system-type 'gnu/linux)
           (executable-find "pass"))
  :commands (helm-pass)
  :after (helm pass))

;; helm ctest
(use-package helm-ctest
  :defer t
  :if (executable-find "ctest")
  :commands (helm-ctest)
  :after helm)

(provide 'setup-helm-plugins)
;;; setup-helm-plugins.el ends here
