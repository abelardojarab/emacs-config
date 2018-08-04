;; setup-helm-plugins.el ---                        -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Abelardo Jara-Berrocal

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
  :commands (helm-grepint-grep)
  :bind (:map ctl-x-map
              ("a" . helm-grepint-grep))
  :load-path (lambda () (expand-file-name "helm-grepint/" user-emacs-directory))
  :config (helm-grepint-set-default-config))

;; helm elscreen
(use-package helm-elscreen
  :defer t
  :after (helm elscreen)
  :load-path (lambda () (expand-file-name "helm-elscreen/" user-emacs-directory)))

;; helm git-grep
(use-package helm-git-grep
  :defer t
  :after (helm helm-elscreen)
  :commands (helm-git-grep)
  :load-path (lambda () (expand-file-name "helm-git-grep/" user-emacs-directory))
  :bind (:map ctl-x-map
              ("g" . helm-git-grep))
  :config (setq helm-git-grep-candidate-number-limit nil))

;; helm ag/ack
(use-package helm-ag
  :defer t
  :after helm
  :commands (helm-ag helm-do-ag helm-do-ag-this-file helm-do-ag-project-root)
  :load-path (lambda () (expand-file-name "helm-ag/" user-emacs-directory))
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
  :bind ([remap describe-mode] . helm-describe-modes)
  :load-path (lambda () (expand-file-name "helm-describe-modes/" user-emacs-directory)))

;; helm desc-binds
(use-package helm-descbinds
  :defer t
  :after helm
  :commands (helm-descbinds helm-descbinds-mode)
  :load-path (lambda () (expand-file-name "helm-descbinds/" user-emacs-directory))
  :bind (:map ctl-x-map
              ("k" . helm-descbinds))
  :init (helm-descbinds-mode 1))

;; helm flycheck
(use-package helm-flycheck
  :defer t
  :after (helm flycheck)
  :commands (helm-flycheck)
  :load-path (lambda () (expand-file-name "helm-flycheck/" user-emacs-directory))
  :bind (:map ctl-x-map
              ("e" . helm-flycheck)))

;; helm flyspell
(use-package helm-flyspell
  :defer t
  :after (helm flyspell)
  :commands (helm-flyspell-correct)
  :load-path (lambda () (expand-file-name "helm-flyspell/" user-emacs-directory))
  :bind (([remap ispell-word] . helm-flyspell-correct)
         :map ctl-x-map
         (";" . helm-flyspell-correct)))

;; helm ls git
(use-package helm-ls-git
  :defer t
  :commands (helm-ls-git-ls helm-browse-project)
  :bind (:map ctl-x-map
              ("C-d" . helm-browse-project))
  :load-path (lambda () (expand-file-name "helm-ls-git/" user-emacs-directory)))

;; helm bm support
(use-package helm-bm
  :defer t
  :commands (helm-bm)
  :after (helm bm)
  :load-path (lambda () (expand-file-name "helm-bm/" user-emacs-directory))
  :bind (:map ctl-x-map
              ("l" . helm-bm))
  :config (setq helm-bookmark-show-location t))

;; helm etags plus
(use-package helm-etags+
  :defer t
  :after (helm etags)
  :commands (helm-etags-select)
  :load-path (lambda () (expand-file-name "helm-etags-plus/" user-emacs-directory))
  :bind ("M-." . helm-etags-select))

;; helm gtags
(use-package helm-gtags
  :defer t
  :after (helm ggtags)
  :commands (helm-gtags-select
             helm-gtags-dwim
             helm-gtags-mode)
  :if (executable-find "global")
  :load-path (lambda () (expand-file-name "helm-gtags/" user-emacs-directory))
  :bind (("C-." . helm-gtags-dwim)
         :map ctl-x-map
         ("." . helm-gtags-dwim))
  :init (add-hook 'c-mode-common-hook
                  (lambda () (helm-gtags-mode t)))
  :config (setq
           helm-gtags-ignore-case t
           helm-gtags-auto-update t
           helm-gtags-use-input-at-cursor t
           helm-gtags-pulse-at-cursor t
           helm-gtags-prefix-key "\C-cg"
           helm-gtags-suggested-key-mapping t))

;; helm xref
(use-package helm-xref
  :after helm
  :if (and (executable-find "global")
           (boundp 'xref-backend-functions))
  :load-path (lambda () (expand-file-name "helm-xref/" user-emacs-directory))
  :config (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

;; helm yasnippet
(use-package helm-c-yasnippet
  :defer t
  :commands (helm-yas-complete helm-c-yas-complete)
  :load-path (lambda () (expand-file-name "helm-c-yasnippet/" user-emacs-directory))
  :bind (:map yas-minor-mode-map
              ([(shift tab)]     . helm-c-yas-complete)
              ([backtab]         . helm-c-yas-complete)
              ("<S-iso-lefttab>" . helm-c-yas-complete)
          :map ctl-x-map
              ("y"               . helm-c-yas-complete)))

;; helm make support
(use-package helm-make
  :defer t
  :after helm
  :commands (helm-make)
  :load-path (lambda () (expand-file-name "helm-make/" user-emacs-directory)))

;; Biblio (helm-bibtex requirement)
;; extensible Emacs package for browsing and fetching references
(use-package biblio
  :after helm
  :defer t
  :load-path (lambda () (expand-file-name "biblio/" user-emacs-directory)))

;; helm bibtex
(use-package helm-bibtex
  :defer t
  :after (helm biblio)
  :commands (helm-bibtex)
  :bind (:map ctl-x-map
              ("[" . helm-bibtex))
  :load-path (lambda () (expand-file-name "helm-bibtex/" user-emacs-directory))
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
  :after (helm async org)
  :load-path (lambda () (expand-file-name "org-ref/" user-emacs-directory))
  :config (progn
            (setq org-ref-default-bibliography (list my/bibtex-completion-bibliography)
                  org-ref-bibliography-files (list my/bibtex-completion-bibliography)
                  org-ref-pdf-directory my/bibtex-completion-library-path
                  org-ref-bibliography-notes my/bibtex-completion-notes)

            (setq org-ref-insert-cite-key "C-c [")
            (setq org-ref-default-citation-link "autocite")))

;; helm themes
(use-package helm-themes
  :defer t
  :after helm
  :commands (helm-themes)
  :load-path (lambda () (expand-file-name "helm-themes/" user-emacs-directory)))

;; helm dash
(use-package helm-dash
  :defer t
  :after (helm dash)
  :if (executable-find "sqlite3")
  :commands (helm-dash)
  :load-path (lambda () (expand-file-name "helm-dash/" user-emacs-directory))
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
            (add-hook 'org-mode-hook #'dash-load-org)

            (defun dash-load-cmake ()
              (interactive)
              (setq-local helm-dash-docsets '("CMake")))
            (add-hook 'cmake-mode-hook #'dash-load-cmake)

            (defun dash-load-elisp ()
              (interactive)
              (setq-local helm-dash-docsets '("Emacs_Lisp")))
            (add-hook 'emacs-lisp-mode-hook #'dash-load-elisp)

            (defun dash-load-c ()
              (interactive)
              (setq-local helm-dash-docsets '("C")))
            (add-hook 'c-mode-hook #'dash-load-c)

            (defun dash-load-c++ ()
              (interactive)
              (setq-local helm-dash-docsets '("C++"
                                              "GLib")))
            (add-hook 'c++-mode-hook #'dash-load-c++)

            (defun dash-load-py ()
              (interactive)
              (setq-local helm-dash-docsets '("Python_2"
                                              "Pandas")))
            (add-hook 'python-mode-hook #'dash-load-py)

            (defun dash-load-js ()
              (interactive)
              (setq-local helm-dash-docsets '("JavaScript"
                                              "NodeJS"
                                              "AngularJS")))
            (add-hook 'js2-mode-hook #'dash-load-js)

            (defun dash-load-r ()
              (interactive)
              (setq-local helm-dash-docsets '("R")))
            (add-hook 'ess-mode-hook #'dash-load-r)

            (defun dash-load-bash ()
              (interactive)
              (setq-local helm-dash-docsets '("Bash")))
            (add-hook 'shell-mode-hook #'dash-load-bash)

            (defun dash-load-md ()
              (interactive)
              (setq-local helm-dash-docsets '("Markdown")))
            (add-hook 'markdown-mode-hook #'dash-load-md)))

;; helm company
(use-package helm-company
  :defer t
  :bind (:map ctl-x-map
              (":" . helm-company)
              :map company-mode-map
              ("C-:" . helm-company))
  :after (helm company)
  :commands (helm-company)
  :load-path (lambda () (expand-file-name "helm-company/" user-emacs-directory)))

;; helm pages
;; Text is divided into pages delimited by the formfeed character (ASCII code 12, also denoted as ‘control-L’)
(use-package helm-pages
  :defer t
  :after helm
  :commands helm-pages
  :load-path (lambda () (expand-file-name "helm-pages/" user-emacs-directory)))

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
  :after (helm git-modes request)
  :load-path (lambda () (expand-file-name "helm-gitignore/" user-emacs-directory)))

;; helm hunks
(use-package helm-hunks
  :defer t
  :commands (helm-hunks
         helm-hunks-current-buffer
         helm-hunks-staged
         helm-hunks-staged-current-buffer)
  :after (helm git-modes request)
  :load-path (lambda () (expand-file-name "helm-hunks/" user-emacs-directory)))

;; helm pass
(use-package helm-pass
  :defer t
  :if (and (equal system-type 'gnu/linux)
           (executable-find "pass"))
  :commands (helm-pass)
  :after (helm pass)
  :load-path (lambda () (expand-file-name "helm-pass/" user-emacs-directory)))

;; helm ctest
(use-package helm-ctest
  :defer t
  :if (executable-find "ctest")
  :commands (helm-ctest)
  :after helm
  :load-path (lambda () (expand-file-name "helm-ctest/" user-emacs-directory)))

(provide 'setup-helm-plugins)
;;; setup-helm-plugins.el ends here
