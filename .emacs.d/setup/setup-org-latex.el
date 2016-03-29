;;; setup-org-latex.el ---                           -*- lexical-binding: t; -*-

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

;; Preview LaTeX equations in buffers by showing images (C-c C-x C-l)
(setq org-latex-create-formula-image-program 'imagemagick)

;; Equations in Org
(defvar text-scale-mode-hook nil
  "Hook run at end of command `text-scale-mode'.")

(defadvice text-scale-mode (after text-scale-mode-hooks nil activate)
  "Run `text-scale-mode-hook' at end of command `text-scale-mode'."
  (if (functionp text-scale-mode-hook)
      (funcall text-scale-mode-hook)
    (loop for hook in text-scale-mode-hook do
          (if (eq hook 't)
              (run-hooks (default-value text-scale-mode-hook))
            (run-hooks hook)))))

(defun org-text-scale-eye ()
  "Scale equation images according to text-scale-mode-amount."
  (when (boundp 'text-scale-mode-amount)
    (let ((relwidth (* (expt text-scale-mode-step text-scale-mode-amount))))
      (loop for ol in (overlays-in (point-min) (point-max)) do
            (when (eq (overlay-get ol 'org-overlay-type) 'org-latex-overlay)
              (unless (overlay-get ol 'org-image-original-width)
                (overlay-put ol 'org-image-original-width (car (image-size (overlay-get ol 'display) t))))
              (let ((ol-disp-plist (cdr (overlay-get ol 'display))))
                (setq ol-disp-plist (plist-put ol-disp-plist :type 'imagemagick))
                (setq ol-disp-plist (plist-put ol-disp-plist :width (round (* relwidth (overlay-get ol 'org-image-original-width)))))
                (overlay-put ol 'display (append '(image) ol-disp-plist))))))
    (force-window-update)))
(add-hook 'org-mode-hook '(lambda () (add-hook 'text-scale-mode-hook 'org-text-scale-eye)))

(defadvice org-format-latex (before set-scale activate)
  "Set :scale in `org-format-latex-options' to the scaling factor resulting from `text-scale-mode' and clear cache."
  (let ((relwidth (expt text-scale-mode-step text-scale-mode-amount)))
    (unless (= (plist-get org-format-latex-options :scale) relwidth)
      (plist-put org-format-latex-options :scale relwidth))))

;; for Tikz image in Org
(setq org-babel-latex-htlatex "htlatex")
(defmacro by-backend (&rest body)
  `(case (if (boundp 'backend) (org-export-backend-name backend) nil) ,@body))

;; for Graphviz image in Org
(add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))

;; Tweaks for Latex exporting
(require 'ox-latex)
(setq org-latex-listings t)
(setq org-export-latex-quotes
      '(("en" ("\\(\\s-\\|[[(]\\)\"" . "\\enquote{") ("\\(\\S-\\)\"" . "}") ("\\(\\s-\\|(\\)'" . "`"))))

;; Reftex
(require 'dash)
(require 'reftex-cite)
(setq reftex-default-bibliography '("~/workspace/Documents/Bibliography/biblio.bib")) ;; So that RefTeX in Org-mode knows bibliography
(defun org-mode-reftex-setup ()
  (interactive)
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
         ;; Reftex should use the org file as master file. See C-h v TeX-master for infos.
         (setq TeX-master t)
         (turn-on-reftex)
         ;; enable auto-revert-mode to update reftex when bibtex file changes on disk
         (global-auto-revert-mode t) ; careful: this can kill the undo
         ;; history when you change the file
         ;; on-disk.
         (reftex-parse-all)
         ;; add a custom reftex cite format to insert links
         ;; This also changes any call to org-citation!
         (reftex-set-cite-format
          '((?c . "\\citet{%l}") ; natbib inline text
            (?i . "\\citep{%l}") ; natbib with parens
            ))))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))
(add-hook 'org-mode-hook 'org-mode-reftex-setup)

;; Add defaults packages to include when exporting.
(setq org-latex-hyperref-template
      "\\hypersetup{\n  pdfkeywords={%k},\n  pdfsubject={%d},\n  pdfcreator={%c},\n  colorlinks=true,\n  linkcolor=black,\n  urlcolor=blue}\n")
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
\\usepackage[T1]{fontenc}
\\usepackage{graphicx}
\\usepackage{geometry}
\\usepackage{listings}
\\usepackage{caption}
\\usepackage{color}
\\usepackage{xcolor}
\\usepackage{mathptmx}
\\usepackage[section]{placeins}
\\usepackage{tikz}
\\usepackage{csquotes}
\\usepackage[backend=biber,sorting=none]{biblatex}
\\addbibresource[datatype=bibtex]{~/workspace/Documents/Bibliography/biblio.bib}
\\usepackage[]{xkeyval}
\\usepackage{paralist}
\\geometry{a4paper, textwidth=6.5in, textheight=10in,
            marginparsep=7pt, marginparwidth=.6in}
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
  aboveskip=\\bigskipamount,
  belowskip=\\bigskipamount}

% For wrapping text in tables
\\usepackage{array}

% Support for floats
\\RequirePackage{float}

% For more table formatting
\\RequirePackage{booktabs}

% Use hyperlinks for convenience, but let's not make them coloured
\\RequirePackage[bookmarks,colorlinks,breaklinks]{hyperref}
\\hypersetup{
  colorlinks,
  citecolor=black,
  filecolor=black,
  linkcolor=black,
  urlcolor=black
}

% Use the excellent biblatex package with IEEE style, sorted = none makes the numbering appear in order
\\RequirePackage[american]{babel}
\\RequirePackage{csquotes}
\\RequirePackage[
  sorting = none,
  url = false,
  hyperref = true,
  style = ieee,
  bibencoding = utf8]{biblatex}
\\DeclareLanguageMapping{american}{american-apa}

% Bibliography
\\bibliography{biblio}

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
\\geometry{a4paper, textwidth=6.5in, textheight=10in,
            marginparsep=7pt, marginparwidth=.6in}
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
\\usepackage{verbatim}\n

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
\\usepackage[section]{placeins}
\\geometry{a4paper, textwidth=6.5in, textheight=9.2in,
            marginparsep=7pt, marginparwidth=.6in}
\\definecolor{light-gray}{gray}{0.95}
\\lstset{
  frame=tlbr,
  framesep=4pt,
  framerule=0pt,
  columns=fullflexible,
  numberstyle=\\tiny,             % the size of the fonts that are used for the line-numbers
  numbers=left,                   % where to put the line-numbers
  stepnumber=1,                   % the step between two line-numbers. If it's 1 each line
  tabsize=2,                      % sets default tabsize to 2 spaces
  breaklines=true,                % sets automatic line breaking
  breakatwhitespace=true,         % sets if automatic breaks should only happen at whitespace
  backgroundcolor=\\color{light-gray},
  basicstyle=\\normalsize\\ttfamily,
  showstringspaces=false,
  keywordstyle=\\itshape\\color{blue},
  identifierstyle=\\ttfamily,
  commentstyle=\\color{black},
  xleftmargin=0.5cm,
  xrightmargin=0.5cm,
  aboveskip=\\bigskipamount,
  belowskip=\\bigskipamount}

% For wrapping text in tables
\\usepackage{array}

% Support for floats
\\RequirePackage{float}

% For more table formatting
\\RequirePackage{booktabs}

% Use hyperlinks for convenience, but let's not make them coloured
\\RequirePackage[bookmarks,colorlinks,breaklinks]{hyperref}
\\hypersetup{
  colorlinks,
  citecolor=black,
  filecolor=black,
  linkcolor=black,
  urlcolor=black
}

% Use the excellent biblatex package with IEEE style, sorted = none makes the numbering appear in order
\\RequirePackage[american]{babel}
\\RequirePackage{csquotes}
\\RequirePackage[
  sorting = none,
  url = false,
  hyperref = true,
  style = ieee,
  bibencoding = utf8]{biblatex}
\\DeclareLanguageMapping{american}{american-apa}

% Bibliography
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
            "bibtex $(basename %b)"
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
