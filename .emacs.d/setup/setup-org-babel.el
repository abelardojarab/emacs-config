;;; setup-org-babel.el ---                           -*- lexical-binding: t; -*-

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

(use-package ob
  :defer t
  :after org
  :custom ((org-src-tab-acts-natively    t)
           (org-src-fontify-natively     t)
           (org-src-tab-acts-natively    t)
           (org-src-window-setup         'current-window)
           (org-src-preserve-indentation t)
           (org-confirm-babel-evaluate   nil)
           (org-export-babel-evaluate    t))
  :config (progn
            ;; Assure you can execute org code
            (use-package ob-org)

            ;; Ditaa rendering
            (use-package ob-ditaa
              :config (setq org-ditaa-jar-path (expand-file-name "jar/ditaa.jar" user-emacs-directory)))

            ;; PlantUML rendering
            (use-package ob-plantuml
              :config (setq org-plantuml-jar-path (expand-file-name "jar/plantuml.jar" user-emacs-directory)))

            ;; Mermaid rendering, requires 'npm install mermaid.cli'
            (use-package ob-mermaid
              :if (executable-find "mmdc"))

            ;; Fast todo selection allows changing from any task todo state to any other state
            ;; directly by selecting the appropriate key from the fast todo selection key menu. This
            ;; is a great feature!
            (setq org-use-fast-todo-selection t)

            ;; A progress indicator for code blocks in org-mode courtesy of John Kitchin
            (defadvice org-babel-execute-src-block (around progress nil activate)
              (message "Running your code block")
              ad-do-it
              (message "Done with code block"))

            ;; Automatically refresh inline images that are generated from Babel blocks
            (add-hook 'org-babel-after-execute-hook (lambda ()
                                                      (condition-case nil
                                                          (org-redisplay-inline-images)
                                                        (error nil))) 'append)

            ;; Avoid `org-babel-do-load-languages' since it does an eager require.
            (dolist (mode my/babel-modes)
              (eval
               (car
                (read-from-string
                 (format
                  "(use-package ob-%s
                      :defer t
                      :commands (org-babel-execute:%s))"
                  mode mode)))))

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

            (define-skeleton skel-org-latex-header
              "Insert LaTeX exporting header"
              ""
              "# Additional style references:\n"
              "#+LATEX_HEADER: %% Using the word +LATEX_HEADER: %% (or tag :noexport) at the beginning of a heading prevents the entire subtree from being exported.\n"
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
              "# LaTeX class configuration\n"
              "#+LaTeX_CLASS: xelatex\n"
              "#+LaTeX_CLASS_OPTIONS: [koma, a4paper, portrait, onecolumn, 11pt]\n"
              "\n"
              "# Paper configuration\n"
              "#+LATEX_HEADER: \\geometry{margin=1.0in, top=1.25in, a4paper, textwidth=6.5in, textheight=9.0in, marginparsep=0.5in, marginparwidth=0.5in}\n"
              "\n"
              "# Header and footer configurations\n"
              "#+LATEX_HEADER: %% Header and footer configuration\n"
              "#+LATEX_HEADER: \\pagestyle{fancy}\n"
              "#+LATEX_HEADER: \\fancyhf{}\n"
              "#+LATEX_HEADER: \\fancyhead[LE,RO]{Share\\LaTeX}\n"
              "#+LATEX_HEADER: \\fancyhead[RE,LO]{Guides and tutorials}\n"
              "#+LATEX_HEADER: \\fancyfoot[CE,CO]{\\leftmark}\n"
              "#+LATEX_HEADER: \\fancyfoot[LE,RO]{\\thepage}\n"
              "#+LATEX_HEADER: \\renewcommand{\\headrulewidth}{2pt}\n"
              "#+LATEX_HEADER: \\renewcommand{\\footrulewidth}{1pt}\n"
              "\n"
              "# Additional LaTeX packages:\n"
              "#+LATEX_HEADER: %% Additional LaTeX packages:\n"
              "#+LaTeX_HEADER: %% \\usepackage{etex}\n"
              "\n"
              "# Enable Latin modern fonts (instead of cm-super) and microtype font enhancements:\n"
              "#+LATEX_HEADER: %% Enable Latin modern fonts and font enhancements:\n"
              "#+LaTeX_HEADER: \\usepackage{lmodern}\n"
              "#+LaTeX_HEADER: \\usepackage{microtype}\n"
              "\n"
              "# Equations fontset:\n"
              "#+LATEX_HEADER: %% Equations fontset:\n"
              "#+LATEX_HEADER: %% \\usepackage{fontspecz, unicode-math}\n"
              "#+LATEX_HEADER: %% \\setromanfont[Ligatures=TeX]{Cambria}\n"
              "#+LATEX_HEADER: %% \\setmathfont[math-style=ISO]{Cambria Math}\n"
              "\n"
              "# Define Tikz block styles:\n"
              "#+LATEX_HEADER: %% Define Tikz block styles\n"
              "#+LATEX_HEADER: \\tikzstyle{decision} = [diamond, draw, text width=4.5em, text badly centered, node distance=3cm, inner sep=0pt]\n"
              "#+LATEX_HEADER: \\tikzstyle{block} = [rectangle, draw, text width=5em, text centered, rounded corners, minimum height=4em]\n"
              "#+LATEX_HEADER: \\tikzstyle{line} = [draw, -latex']\n"
              "#+LATEX_HEADER: \\tikzstyle{cloud} = [draw, ellipse, node distance=3cm, minimum height=2em]\n"
              "#+LATEX_HEADER: \\tikzstyle{box} = [draw, rectangle, minimum width = 4em, minimum height = 4em]\n"
              "#+LATEX_HEADER: \\tikzstyle{circle} = [draw, circle, minimum size=1em, node distance=1.75cm]\n"
              "#+LATEX_HEADER: \\tikzstyle{point} = [coordinate]\n"
              "\n"
              "# Setup Tikz package for both LaTeX and HTML export:\n"
              "#+PROPERTY: header-args:latex+ :iminoptions -density 600 -resample 100x100\n"
              "#+PROPERTY: header-args:latex+ :imoutoptions -geometry 600\n"
              "\n"
              "# Setup for HTML export:\n"
              "#+STYLE: <link rel=\"stylesheet\" type=\"text/css\" href=\"http://thomasf.github.io/solarized-css/solarized-light.min.css\"/>\n"
              "\n"
              "#+LATEX_HEADER: %% Bibliography (biblatex)\n"
              "#+LATEX_HEADER: %% Using IEEE style, sorted = none makes the numbering appear in order.\n"
              "#+LATEX_HEADER: \\usepackage[\n"
              "#+LATEX_HEADER:    sorting=none,\n"
              "#+LATEX_HEADER:    url=false,\n"
              "#+LATEX_HEADER:    hyperref=true,\n"
              "#+LATEX_HEADER:    style=authoryear,\n"
              "#+LATEX_HEADER:    backend=biber,\n"
              "#+LATEX_HEADER:    bibencoding=ascii]{biblatex}\n"
              "#+LATEX_HEADER: \\DeclareLanguageMapping{american}{american-apa}\n"
              "#+LATEX_HEADER: %% Point at your bib file:\n"
              "#+LATEX_HEADER: %% \\addbibresource[datatype=bibtex]{/home/abelardojara/Dropbox/Documents/Bibliography/biblio.bib}\n"
              "#+LATEX_HEADER: %% \\bibliography{biblio}\n")
            (define-abbrev org-mode-abbrev-table "ltxhdr" "" 'skel-org-latex-header)

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
              "# LaTeX class configuration\n"
              "#+LaTeX_CLASS: xelatex\n"
              "#+LaTeX_CLASS_OPTIONS: [koma, a4paper, portrait, onecolumn, 11pt]\n"
              "\n"
              "# Paper configuration\n"
              "#+LATEX_HEADER: \\geometry{margin=1.0in, top=1.25in, a4paper, textwidth=6.5in, textheight=9.0in, marginparsep=0.5in, marginparwidth=0.5in}\n"
              "\n"
              "# Setup for HTML export:\n"
              "#+STYLE: <link rel=\"stylesheet\" type=\"text/css\" href=\"http://thomasf.github.io/solarized-css/solarized-light.min.css\"/>\n"
              "\n"
              "# Equations fontset:\n"
              "#+LATEX_HEADER: %% Equations fontset:\n"
              "#+LATEX_HEADER: %% \\usepackage{fontspecz, unicode-math}\n"
              "#+LATEX_HEADER: %% \\setromanfont[Ligatures=TeX]{Cambria}\n"
              "#+LATEX_HEADER: %% \\setmathfont[math-style=ISO]{Cambria Math}\n"
              "\n"
              )
            (define-abbrev org-mode-abbrev-table "sheader" "" 'skel-header-block)

            ;; Tell auto-insert what to use for .org files
            (define-auto-insert "\\.org" 'skel-header-block)))

(provide 'setup-org-babel)
;;; setup-org-babel.el ends here
