;;; setup-org-babel.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Abelardo Jara-Berrocal

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

(use-package ob
  :after org
  :config (progn

            ;; Settings
            (setq org-src-tab-acts-natively t
                  org-src-fontify-natively t
                  org-src-tab-acts-natively t

                  ;; Don't ask for confirmation on every =C-c C-c= code-block compile.
                  org-confirm-babel-evaluate nil

                  ;; Re-enable org-export babel evaluate
                  org-export-babel-evaluate t

                  ;; Rendering ditaa
                  org-ditaa-jar-path (expand-file-name "jar/ditaa.jar" user-emacs-directory)

                  ;; Rendering plantuml
                  org-plantuml-jar-path (expand-file-name "jar/plantuml.jar" user-emacs-directory))

            ;; A progress indicator for code blocks in org-mode courtesy of John Kitchin
            (defadvice org-babel-execute-src-block (around progress nil activate)
              ;; (set-face-attribute
              (message "Running your code block")
              ad-do-it
              ;; (set-face-attribute 'org-block-background nil :background "gray")
              (message "Done with code block"))

            ;; Automatically refresh inline images that are generated from Babel blocks
            (add-hook 'org-babel-after-execute-hook (lambda ()
                                                      (condition-case nil
                                                          (org-display-inline-images)
                                                        (error nil))) 'append)

            ;; Enable multiple languages
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((latex . t)
               (emacs-lisp . t)
               (plantuml . t)
               (ditaa . t)
               (dot . t)
               (gnuplot . t)
               ;; sh was renamed to shell according to:
               ;; http://comments.gmane.org/gmane.emacs.orgmode/102877
               (shell . t)
               (R . t)
               (perl . t)
               (ruby . t)
               (python . t)
               (js . t)
               (C . t)
               (haskell . t)))

            ;; for Tikz image in Org
            (setq org-babel-latex-htlatex "htlatex")
            (defmacro by-backend (&rest body)
              `(case org-export-current-backend ,@body))

            ;; The next block makes org-babel aware that a lower-case 'r' in a =src= block header should be processed as R.
            (add-to-list 'org-src-lang-modes
                         '("r" . ess-mode))

            ;; To edit Graphviz code in Org
            (add-to-list 'org-src-lang-modes
                         '("dot" . graphviz-dot))

            ;; To edit plantuml code in Org
            (add-to-list 'org-src-lang-modes
                         '("plantuml" . puml))

            ;; Abbrev
            (add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))
            (define-skeleton skel-org-block-elisp
              "Insert an emacs-lisp block"
              ""
              "#+begin_src emacs-lisp\n"
              _ - \n
              "#+end_src\n")
            (define-abbrev org-mode-abbrev-table "elsrc" "" 'skel-org-block-elisp)

            (define-skeleton skel-org-block-js
              "Insert a JavaScript block"
              ""
              "#+begin_src js\n"
              _ - \n
              "#+end_src\n")
            (define-abbrev org-mode-abbrev-table "jssrc" "" 'skel-org-block-js)

            (define-skeleton skel-header-block
              "Creates my default header"
              ""
              "#+TITLE: " str "\n"
              "#+AUTHOR:\n"
              "#+EMAIL:\n"
              "#+LANGUAGE: en\n"
              "\n"
              "# Setup for document style:\n"
              "#+OPTIONS: toc:nil num:0 H:2\n"
              "#+OPTIONS: author:t email:nil  date:t\n"
              "#+OPTIONS: c:nil d:(not LOGBOOK) e:t f:t inline:t p:nil pri:nil stat:t tags:t\n"
              "#+OPTIONS: tasks:t tex:t timestamp:t todo:t\n"
              "\n"
              "# Setup for document summary and keywords:\n"
              "#+DESCRIPTION:\n"
              "#+KEYWORDS:\n"
              "#+SELECT_TAGS: export\n"
              "#+EXCLUDE_TAGS: noexport\n"
              "\n"
              "# Custom LaTeX packages:\n"
              "#+LaTeX_HEADER: \\usepackage{etex}\n"
              "\n"
              "# Setup tikz package for both LaTeX and HTML export:\n"
              "#+PROPERTY: header-args:latex+ :iminoptions -density 600 -resample 100x100\n"
              "#+PROPERTY: header-args:latex+ :imoutoptions -geometry 600\n"
              "\n"
              "# Setup for HTML export:\n"
              "#+STYLE: <link rel=\"stylesheet\" type=\"text/css\" href=\"../csstheme/solarized-light.min.css\"/>\n")
            (define-abbrev org-mode-abbrev-table "sheader" "" 'skel-header-block)

            ;; Tell auto-insert what to use for .org files
            (define-auto-insert "\\.org" 'skel-header-block)))

(provide 'setup-org-babel)
;;; setup-org-babel.el ends here
