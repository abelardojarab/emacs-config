;;; setup-org-latex.el ---                           -*- lexical-binding: t; -*-

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

;; Tweaks for LaTeX exporting
(use-package ox-latex
  :config (progn
            (setq org-latex-listings t)
            (setq org-export-latex-quotes
                  '(("en" ("\\(\\s-\\|[[(]\\)\"" . "\\enquote{") ("\\(\\S-\\)\"" . "}") ("\\(\\s-\\|(\\)'" . "`"))))

            ;; CDLaTeX is “is a minor mode that is normally used in combination with a
            ;; major LaTeX mode like AUCTeX in order to speed-up insertion of environments
            ;; and math templates”
            (add-hook 'org-mode-hook 'turn-on-org-cdlatex)

            ;; Preview LaTeX equations in buffers by showing images (C-c C-x C-l)
            (if (executable-find "convert")
                (setq org-latex-create-formula-image-program 'imagemagick)
              (setq org-latex-create-formula-image-program 'dvipng))

            ;; Directory where LaTeX previews are stored
            (if (not (file-exists-p "~/.emacs.cache/ltxpng"))
                (make-directory "~/.emacs.cache/ltxpng") t)
            (setq org-latex-preview-ltxpng-directory "~/.emacs.cache/ltxpng/")

            ;; Bigger LaTeX fragments and other options for LaTeX export
            (setq org-format-latex-options '(:scale 2.0
                                                    :html-foreground "Black" :html-background "Transparent"
                                                    :html-scale 1.0
                                                    :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

            ;; Toggle previsualization of LaTeX equations in Org-mode
            (when (display-graphic-p)
              (defvar org-latex-fragment-last nil
                "Holds last fragment/environment you were on.")

              (defun org-latex-fragment-toggle ()
                "Toggle a latex fragment image "
                (and (eq 'org-mode major-mode)
                     (let* ((el (org-element-context))
                            (el-type (car el)))
                       (cond
                        ;; were on a fragment and now on a new fragment
                        ((and
                          ;; fragment we were on
                          org-latex-fragment-last
                          ;; and are on a fragment now
                          (or
                           (eq 'latex-fragment el-type)
                           (eq 'latex-environment el-type))
                          ;; but not on the last one this is a little tricky. as you edit the
                          ;; fragment, it is not equal to the last one. We use the begin
                          ;; property which is less likely to change for the comparison.
                          (not (= (org-element-property :begin el)
                                  (org-element-property :begin org-latex-fragment-last))))
                         ;; go back to last one and put image back
                         (save-excursion
                           (goto-char (org-element-property :begin org-latex-fragment-last))
                           (org-preview-latex-fragment))
                         ;; now remove current image
                         (goto-char (org-element-property :begin el))
                         (let ((ov (loop for ov in (org--list-latex-overlays)
                                         if
                                         (and
                                          (<= (overlay-start ov) (point))
                                          (>= (overlay-end ov) (point)))
                                         return ov)))
                           (when ov
                             (delete-overlay ov)))
                         ;; and save new fragment
                         (setq org-latex-fragment-last el))

                        ;; were on a fragment and now are not on a fragment
                        ((and
                          ;; not on a fragment now
                          (not (or
                                (eq 'latex-fragment el-type)
                                (eq 'latex-environment el-type)))
                          ;; but we were on one
                          org-latex-fragment-last)
                         ;; put image back on
                         (save-excursion
                           (goto-char (org-element-property :begin org-latex-fragment-last))
                           (org-preview-latex-fragment))
                         ;; unset last fragment
                         (setq org-latex-fragment-last nil))

                        ;; were not on a fragment, and now are
                        ((and
                          ;; we were not one one
                          (not org-latex-fragment-last)
                          ;; but now we are
                          (or
                           (eq 'latex-fragment el-type)
                           (eq 'latex-environment el-type)))
                         (goto-char (org-element-property :begin el))
                         ;; remove image
                         (let ((ov (loop for ov in (org--list-latex-overlays)
                                         if
                                         (and
                                          (<= (overlay-start ov) (point))
                                          (>= (overlay-end ov) (point)))
                                         return ov)))
                           (when ov
                             (delete-overlay ov)))
                         (setq org-latex-fragment-last el))))))

              (add-hook 'post-command-hook 'org-latex-fragment-toggle))

            ;; Force figure position
            (setq org-latex-default-figure-position "!htb")

            ;; Place table caption below table
            (setq org-latex-table-caption-above nil)

            ;; Use centered images in Org-mode
            (ignore-errors
              (advice-add 'org-latex--inline-image :around
                          (lambda (orig link info)
                            (concat
                             "\\begin{center}"
                             (funcall orig link info)
                             "\\end{center}"))))))

;; Reftex
(use-package reftex-cite
  :config (progn

            ;; Make RefTeX faster
            (setq reftex-enable-partial-scans t)
            (setq reftex-save-parse-info t)
            (setq reftex-use-multiple-selection-buffers t)
            (setq reftex-plug-into-AUCTeX t)
            (setq reftex-cite-prompt-optional-args nil)
            (setq reftex-cite-cleanup-optional-args t)

            ;; Enable RefTeX in Org-mode to find bibliography
            (setq reftex-default-bibliography '("~/workspace/Documents/Bibliography/biblio.bib"))
            (defun org-mode-reftex-setup ()
              (interactive)
              (and (buffer-file-name) (file-exists-p (buffer-file-name))
                   (progn
                     ;; Reftex should use the org file as master file. See C-h v TeX-master for infos.
                     (setq TeX-master t)
                     (load-library "reftex")
                     (turn-on-reftex)
                     (and (buffer-file-name)
                          (file-exists-p (buffer-file-name))
                          (reftex-parse-all))
                     ;; enable auto-revert-mode to update reftex when bibtex file changes on disk
                     (global-auto-revert-mode t)
                     ;; add a custom reftex cite format to insert links
                     ;; This also changes any call to org-citation!
                     ;; RefTeX formats for biblatex (not natbib)
                     (reftex-set-cite-format
                      '((?\C-m . "\\cite[]{%l}")
                        (?t . "\\textcite{%l}")
                        (?a . "\\autocite[]{%l}")
                        (?p . "\\parencite{%l}")
                        (?f . "\\footcite[][]{%l}")
                        (?F . "\\fullcite[]{%l}")
                        (?x . "[]{%l}")
                        (?X . "{%l}")
                        ))))
              (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
              (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))
            (add-hook 'org-mode-hook 'org-mode-reftex-setup)

            ;; Add cite link
            (org-add-link-type "cite" 'ebib
                               (lambda (path desc format)
                                 (cond
                                  ((eq format 'html)  (format "(<cite>%s</cite>)" path))
                                  ((eq format 'latex) (format "\\cite{%s}" path)))))

            (setq font-latex-match-reference-keywords
                  '(("cite" "[{")
                    ("cites" "[{}]")
                    ("autocite" "[{")
                    ("footcite" "[{")
                    ("footcites" "[{")
                    ("parencite" "[{")
                    ("textcite" "[{")
                    ("fullcite" "[{")
                    ("citetitle" "[{")
                    ("citetitles" "[{")
                    ("headlessfullcite" "[{")))))

;; Add defaults packages to include when exporting.
(setq org-latex-hyperref-template
      "\\hypersetup{\n  pdfkeywords={%k},\n  pdfsubject={%d},\n  pdfcreator={%c},\n  citecolor=black,\n  filecolor=black,\n  colorlinks=true,\n  linkcolor=black,\n  urlcolor=black}\n")
(add-to-list 'org-latex-packages-alist '("" "graphicx"))
(add-to-list 'org-latex-packages-alist '("" "geometry"))
(add-to-list 'org-latex-packages-alist '("" "hyperref"))
(add-to-list 'org-latex-packages-alist '("" "caption"))
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))
(add-to-list 'org-latex-packages-alist '("" "mathptmx"))

;; Define the output styles
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))
(add-to-list 'org-latex-classes
             '("xelatex"
               "\\documentclass[11pt,a4paper]{article}

% Choose the main language
\\usepackage[english]{babel}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}

% Additional packages
\\usepackage{graphicx}
\\usepackage{geometry}
\\usepackage{listings}
\\usepackage{caption}
\\usepackage{color}
\\usepackage{xcolor}
\\usepackage{mathptmx}
\\usepackage[]{xkeyval}

% Enumeration inside paragraphs
\\usepackage{paralist}

% Math macros and styles from American Math Society (AMS)
\\usepackage{amsmath}
\\usepackage{amsthm}

% Figure wrapping
\\usepackage{wrapfig}

% Enable support for floating figures [h], [t], etc.
\\usepackage{float}

% Disable float to cross between sections boundaries
\\usepackage[section]{placeins}

% Lorem Ipsum generator
\\usepackage{blindtext}

% Fancy headers
\\usepackage{fancyhdr}

% Code listings settings
\\definecolor{light-gray}{gray}{0.95}
\\lstset{
  frame=tlbr,
  framesep=4pt,
  framerule=0pt,
  columns=fullflexible,
  breaklines=true,
  backgroundcolor=\\color{light-gray},
  basicstyle=\\normalsize\\ttfamily,
  showstringspaces=false,
  keywordstyle=\\itshape\\color{blue},
  identifierstyle=\\ttfamily,
  commentstyle=\\color{black},
  xleftmargin=0.5cm,
  xrightmargin=0.5cm,
  numberstyle=\\tiny,             % the size of the fonts that are used for the line-numbers
  numbers=left,                   % where to put the line-numbers
  stepnumber=1,                   % the step between two line-numbers. If it's 1 each line
  tabsize=2,                      % sets default tabsize to 2 spaces
  breaklines=true,                % sets automatic line breaking
  aboveskip=\\bigskipamount,
  belowskip=\\bigskipamount}

% For wrapping text in tables
\\usepackage{array}

% Auto-fit columns to the table width
\\usepackage{tabularx}

% Enable extra features for table formatting
\\usepackage{booktabs}

% Use hyperlinks for convenience, but let's not make them coloured
\\usepackage[bookmarks,colorlinks,breaklinks]{hyperref}

% Tikz images
\\usepackage{filecontents}
\\usepackage{tikz}
\\usepackage{tikzscale}

% For using MatLab matlab2tikz
\\usepackage{pgfplots}
\\pgfplotsset{compat=newest}
\\pgfplotsset{plot coordinates/math parser=false}
\\newlength\\figureheight
\\newlength\\figurewidth

% pstricks support, setting compatibility with Inkscape
\\usepackage{pstricks}
\\psset{xunit=.5pt,yunit=.5pt,runit=.5pt}

% Load Tikz libraries
\\usetikzlibrary{calc,trees,positioning,arrows,chains,shapes.geometric,%
  decorations.pathreplacing,decorations.pathmorphing,shapes,%
  matrix,shapes.symbols}

% Context sensitive quoting
\\usepackage{csquotes}

% Bibliography (biblatex)
% Using IEEE style, sorted = none makes the numbering appear in order
% \\usepackage[
%   sorting=none,
%   url=false,
%   hyperref=true,
%   style=authoryear,
%   backend=biber,
%   bibencoding=ascii]{biblatex}
% \\DeclareLanguageMapping{american}{american-apa}
% \\addbibresource[datatype=bibtex]{../../Bibliography/biblio.bib} %% point at your bib file
% \\bibliography{biblio}

\\renewcommand{\\rmdefault}{ptm}
\\title{}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]
\\hypersetup{pdfencoding=auto,colorlinks=true}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("pdflatex"
               "\\documentclass[11pt,a4paper]{article}
\\usepackage[T1]{fontenc}
\\usepackage{lmodern}
\\usepackage{graphicx}
\\usepackage{geometry}
\\usepackage{listings}
\\usepackage{hyperref}
\\usepackage{caption}
\\usepackage{color}
\\usepackage{mathptmx}
\\usepackage[section]{placeins}
\\definecolor{light-gray}{gray}{0.95}
\\lstset{
  frame=tlbr,
  framesep=4pt,
  framerule=0pt,
  columns=fullflexible,
  backgroundcolor=\\color{light-gray},
  basicstyle=\\normalsize\\ttfamily,
  showstringspaces=false,
  keywordstyle=\\itshape\\color{blue},
  identifierstyle=\\ttfamily,
  commentstyle=\\color{black},
  xleftmargin=0.5cm,
  xrightmargin=0.5cm,
  numberstyle=\\tiny,             % the size of the fonts that are used for the line-numbers
  numbers=left,                   % where to put the line-numbers
  stepnumber=1,                   % the step between two line-numbers. If it's 1 each line
  tabsize=2,                      % sets default tabsize to 2 spaces
  breaklines=true,                % sets automatic line breaking
  breakatwhitespace=true,         % sets if automatic breaks should only happen at whitespace
  aboveskip=\\bigskipamount,
  belowskip=\\bigskipamount}
\\renewcommand{\\rmdefault}{ptm}
\\title{}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass[11pt,presentation]{beamer}
\\usepackage[T1]{fontenc}
\\usepackage{lmodern}
\\usepackage{graphicx}
\\usepackage{geometry}
\\usepackage{listings}
\\usepackage{hyperref}
\\usepackage{caption}
\\usepackage{color}
\\usepackage{mathptmx}
\\usepackage[section]{placeins}
\\definecolor{light-gray}{gray}{0.95}
\\lstset{
  frame=single,
  showtabs=false,
  columns=fullflexible,
  backgroundcolor=\\color{light-gray},
  basicstyle=\\normalsize\\ttfamily,
  showstringspaces=false,
  keywordstyle=\\itshape\\color{blue},
  identifierstyle=\\ttfamily,
  commentstyle=\\color{black},
  xleftmargin=0.5cm,
  xrightmargin=0.5cm,
  numberstyle=\\tiny,             % the size of the fonts that are used for the line-numbers
  numbers=left,                   % where to put the line-numbers
  stepnumber=1,                   % the step between two line-numbers. If it's 1 each line
  tabsize=4,                      % sets default tabsize to 4 spaces
  breaklines=true,                % sets automatic line breaking
  breakatwhitespace=true,         % sets if automatic breaks should only happen at whitespace
}
\\usepackage{verbatim}
\\usetheme{Madrid}
\\title{}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"

               ;; Other section
               ("\\section{%s}" . "\\section*{%s}")
               ("\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}"
                "\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}")))

(add-to-list 'org-latex-classes
             '("ieeeproceedings"
               "\\documentclass[conference]{IEEEtran}
\\usepackage[T1]{fontenc}
\\usepackage{lmodern}
\\usepackage{graphicx}
\\usepackage{geometry}
\\usepackage{listings}
\\usepackage{caption}
\\usepackage{color}
\\usepackage{mathptmx}
\\usepackage[mla]{ellipsis}
\\usepackage[section]{placeins}
\\definecolor{light-gray}{gray}{0.95}
\\lstset{
  frame=tlbr,
  framesep=4pt,
  framerule=0pt,
  columns=fullflexible,
  backgroundcolor=\\color{light-gray},
  basicstyle=\\normalsize\\ttfamily,
  showstringspaces=false,
  keywordstyle=\\itshape\\color{blue},
  identifierstyle=\\ttfamily,
  commentstyle=\\color{black},
  xleftmargin=0.5cm,
  xrightmargin=0.5cm,
  numberstyle=\\tiny,             % the size of the fonts that are used for the line-numbers
  numbers=left,                   % where to put the line-numbers
  stepnumber=1,                   % the step between two line-numbers. If it's 1 each line
  tabsize=2,                      % sets default tabsize to 2 spaces
  breaklines=true,                % sets automatic line breaking
  breakatwhitespace=true,         % sets if automatic breaks should only happen at whitespace
  aboveskip=\\bigskipamount,
  belowskip=\\bigskipamount}

% For wrapping text in tables
\\usepackage{array}

% Support for floats
\\usepackage{float}

% For more table formatting
\\usepackage{booktabs}

% Use hyperlinks for convenience, but let's not make them coloured
\\usepackage[bookmarks,colorlinks,breaklinks]{hyperref}

% Use the excellent biblatex package with IEEE style, sorted = none makes the numbering appear in order
\\usepackage[american]{babel}
\\usepackage{csquotes}
\\usepackage[
  sorting=nyt, % name, year, title
  url=false,
  hyperref=true,
  style=ieee,
  bibencoding=ascii]{biblatex}
\\DeclareLanguageMapping{american}{american-apa}

% Bibliography
\\addbibresource{../../Bibliography/biblio.bib}  %% point at your bib file
\\bibliography{biblio}

\\title{}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; Set default document stylesheet
(if (executable-find "xelatex")
    (setq org-latex-default-class "xelatex")
  (setq org-latex-default-class "pdflatex"))

;; Let the exporter use the -shell-escape option to let latex execute external programs.
(if (executable-find "xelatex")
    (setq org-latex-pdf-process
          '("xelatex -interaction nonstopmode -synctex=1 -shell-escape -output-directory %o %f"
            "biber %b"
            "xelatex -interaction nonstopmode -synctex=1 -shell-escape -output-directory %o %f"
            "xelatex -interaction nonstopmode -synctex=1 -shell-escape -output-directory %o %f"
            "xelatex -interaction nonstopmode -synctex=1 -shell-escape -output-directory %o %f")) ;; multipass
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
          "bibtex $(basename %b)"
          "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
          "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"))
  ) ;; multipass

;; add emacs lisp support for minted
(setq org-latex-custom-lang-environments
      '((emacs-lisp "common-lispcode")))

;; Tweak the PDF viewer
(eval-after-load "org"
  '(progn
     ;; .txt files aren't in the list initially, but in case that changes
     ;; in a future version of org, use if to avoid errors
     (if (assoc "\\.txt\\'" org-file-apps)
         (setcdr (assoc "\\.txt\\'" org-file-apps) "kate %s")
       (add-to-list 'org-file-apps '("\\.txt\\'" . "kate %s") t))
     ;; Change .pdf association directly within the alist
     (setcdr (assoc "\\.pdf\\'" org-file-apps) "acroread %s")))

(provide 'setup-org-latex)
;;; setup-org-latex.el ends here
