;;; setup-org.el ---

;; Copyright (C) 2014  abelardo.jara-berrocal

;; Author: abelardo.jara-berrocal <ajaraber@plxc25288.pdx.intel.com>
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

;; Loads latex auto-complete
(add-to-list 'load-path "~/.emacs.d/ac-math")
(require 'ac-math)
(require 'auto-complete-latex)
(add-to-list 'ac-modes 'latex-mode)
(defun ac-latex-mode-setup ()
  (when (and (require 'auto-complete nil t) (require 'auto-complete-config nil t))
    (make-local-variable 'ac-sources)
    (setq ac-sources (append '(ac-source-words-in-same-mode-buffers
                               ac-source-dictionary
                               ac-source-math-unicode
                               ac-source-math-latex) ac-sources))))
(add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)

;; Org mode
(setq load-path (cons "~/.emacs.d/org/lisp" load-path))
(defvar org-list-allow-alphabetical t)
(defun org-element-bold-successor           (arg))
(defun org-element-code-successor           (arg))
(defun org-element-entity-successor         (arg))
(defun org-element-italic-successor         (arg))
(defun org-element-latex-fragment-successor (arg))
(defun org-element-strike-through-successor (arg))
(defun org-element-subscript-successor      (arg))
(defun org-element-superscript-successor    (arg))
(defun org-element-underline-successor      (arg))
(defun org-element-verbatim-successor       (arg))
(require 'org)
(require 'org-list)
(require 'ox-org)
(require 'ox-md)

(let ((todo "~/workspace/Documents/agenda.org"))
  (when (file-readable-p todo)
    (setq org-agenda-files '("~/workspace/Documents/agenda.org"))
    (setq initial-buffer-choice (lambda ()
                                  (org-agenda nil "n")
                                  (delete-other-windows)
                                  (current-buffer)))))
(add-hook 'org-mode-hook 'flyspell-mode)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'ac-modes 'org-mode)
(setq org-startup-folded 'nofold)
(setq org-startup-indented t)
(setq org-startup-with-inline-images t)
(setq org-startup-truncated t)
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-use-speed-commands t)
(setq org-default-notes-file "~/workspace/Documents/agenda.org")
(setq org-export-with-sub-superscripts nil)

;; Mouse in Org
(require 'org-mouse)

;; Fonts
(add-hook 'org-mode-hook (lambda () (variable-pitch-mode t)))
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit 'fixed-pitch)
(set-face-attribute 'org-block nil :inherit 'fixed-pitch)

(defface org-block-begin-line
  '((t (:inherit org-meta-line
                 :overline "light grey" :foreground "#008ED1")))
  "Face used for the line delimiting the begin of source blocks.")

(defface org-block-end-line
  '((t (:inherit org-meta-line
                 :underline "light grey" :foreground "#008ED1")))
  "Face used for the line delimiting the end of source blocks.")

;; Beamer support
(require 'ox-beamer)

;; Odt export
(require 'ox-odt)

;; Org Templates
(setq org-capture-templates
      '(("t" "Task" entry (file+headline "" "Tasks") "* TODO %?\n  %u\n  %a")
        ("s" "Simple Task" entry (file+headline "" "Tasks") "* TODO %?\n  %U\n")))

(add-to-list 'org-structure-template-alist '("E" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC\n"))
(add-to-list 'org-structure-template-alist '("S" "#+BEGIN_SRC shell-script\n?\n#+END_SRC\n"))

;; Fix shift problem in Org mode
(setq org-support-shift-select t)
(eval-after-load "org"
  '(progn
     (eval-after-load "cua-base"
       '(progn
          (defadvice org-call-for-shift-select (before org-call-for-shift-select-cua activate)
            (if (and cua-mode
                     org-support-shift-select
                     (not (use-region-p)))
                (cua-set-mark)))))))

;; Fix on the keys
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map [kp-enter] 'org-return-indent)
            (define-key org-mode-map [enter] 'org-return-indent)
            (define-key org-mode-map (kbd "RET") 'org-return-indent)))

;; define todo states: set time stamps one waiting, delegated and done
(setq org-todo-keywords
      '((sequence
         "TODO(t)"
         "IN PROGRESS(p!)"
         "HOLD(h!)"
         "WAITING(w)"
         "|"
         "DONE(d!)"
         "CANCELLED(c)")))
(setq org-todo-keyword-faces
      '(
        ("IN PROGRESS" . 'warning)
        ("HOLD" . 'font-lock-keyword-face)
        ("WAITING" . 'font-lock-builtin-face)
        ("CANCELLED" . 'font-lock-doc-face)))

;; PlantUML
(require 'iimage)
(autoload 'iimage-mode "iimage" "Support Inline image minor mode." t)
(autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)
(add-to-list 'iimage-mode-image-regex-alist '("@startuml\s+\\(.+\\)" . 1))
(add-to-list 'iimage-mode-image-regex-alist (cons (concat "\[\[file:\(~?" iimage-mode-image-filename-regex "\)\]") 1))
(add-hook 'org-mode-hook '(lambda () (org-turn-on-iimage-in-org)))
(setq org-image-actual-width '(400))

;; Rendering plantuml
(setq org-plantuml-jar-path (expand-file-name "~/Downloads/plantuml.jar"))
(defun plantuml-render-buffer ()
  (interactive)
  (message "PLANTUML Start rendering")
  (shell-command (concat "java -jar ~/Downloads/plantuml.jar "
                         buffer-file-name))
  (message (concat "PLANTUML Rendered:  " (buffer-name))))

;; Image reloading
(defun reload-image-at-point ()
  (interactive)
  (message "reloading image at point in the current buffer...")
  (image-refresh (get-text-property (point) 'display)))

;; Image resizing and reloading
(defun resize-image-at-point ()
  (interactive)
  (message "resizing image at point in the current buffer...")
  (let* ((image-spec (get-text-property (point) 'display))
         (file (cadr (member :file image-spec))))
    (message (concat "resizing image..." file))
    (shell-command (format "convert -resize %d %s %s "
                           (* (window-width (selected-window)) (frame-char-width))
                           file file))
    (reload-image-at-point)))

;; Function to setup images for display on load
(defun org-turn-on-iimage-in-org ()
  "display images in your org file"
  (interactive)
  (turn-on-iimage-mode)
  (set-face-underline-p 'org-link nil))

;; Function to toggle images in a org buffer
(defun org-toggle-iimage-in-org ()
  "display images in your org file"
  (interactive)
  (if (face-underline-p 'org-link)
      (set-face-underline-p 'org-link nil)
    (set-face-underline-p 'org-link t))
  (call-interactively 'iimage-mode))

;; Preview LaTeX equations in buffers by showing images (C-c C-x C-l)
(setq org-latex-create-formula-image-program 'imagemagick)

;; Equations in Org
;; Use C-c C-x C-l to regenerate the images
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

;; Insert images from files #+BEGIN: image :file "~/Documents/personal/foo.png"
(defun org-dblock-write:image (params)
  (let ((file (plist-get params :file)))
    (clear-image-cache file)
    (insert-image (create-image file) )))

;; Insert screenshots into Org mode, very useful
(defun org-insert-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the same
directory as the org-buffer and insert
a link to this file."
  (interactive)
  (let ((case-fold-search nil))
    (setq tilde-buffer-filename
          (replace-regexp-in-string "/" "\\" (buffer-file-name) t t))
    (setq tilde-buffer-filename
          (replace-regexp-in-string ".org" "" tilde-buffer-filename t t))
    (setq filename
          (concat
           (make-temp-name
            (concat tilde-buffer-filename
                    "_"
                    (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
    (setq filename (file-relative-name filename (file-name-directory (buffer-file-name))))
    (setq filename (replace-regexp-in-string "\\\\" "/" filename))
    (if (equal system-type 'windows-nt)
        ;; Windows: Irfanview
        (call-process "C:\\Program Files (x86)\\IrfanView\\i_view32.exe" nil nil nil (concat
                                                                                      "/clippaste /convert=" filename))

      ;; Linux: ImageMagick: (call-process "import" nil nil nil filename)
      (call-process "import" nil nil nil filename)
      ) ;; if
    (insert (concat "[[file:" filename "]]"))
    (org-display-inline-images)))

;; for Tikz image in Org
(setq org-babel-latex-htlatex "htlatex")
(defmacro by-backend (&rest body)
  `(case (if (boundp 'backend) (org-export-backend-name backend) nil) ,@body))

;; for Graphviz image in Org
(add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))

;; for Gnuplot
(add-to-list 'load-path "~/.emacs.d/gnuplot")
(require 'gnuplot)

;; Make Yasnippet work here, but for Org
(defun yas-org-very-safe-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (and (fboundp 'yas-expand) (yas-expand))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-to-list 'org-tab-first-hook
                         'yas-org-very-safe-expand)))

;; Configure org-mode so that when you edit source code in an indirect buffer (with C-c '), the buffer is opened in the current window. That way, your window organization isn't broken when switching.
(setq org-src-window-setup 'current-window)

;; Markdown exporter
(require 'ox-md)
(setq org-completion-use-ido t)

;; Tweaks for Latex exporting
(require 'ox-latex)

;; Choose either listings or minted for exporting source code blocks.
(setq org-latex-listings t)

;; Export " to csquotes macros
(setq org-export-latex-quotes
      '(("en" ("\\(\\s-\\|[[(]\\)\"" . "\\enquote{") ("\\(\\S-\\)\"" . "}") ("\\(\\s-\\|(\\)'" . "`"))))

;; Reftex
(require 'reftex-cite)
(require 'dash)
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

;; Org-Ref
(add-to-list 'load-path "~/.emacs.d/org-ref")
(require 'org-ref)
;; (org-babel-load-file "~/.emacs.d/org-ref/org-ref.org")
(setq org-ref-bibliography-notes "~/workspace/Documents/Bibliography/notes.org"
      org-ref-default-bibliography '("~/workspace/Documents/Bibliography/biblio.bib")
      org-ref-pdf-directory "~/workspace/Documents/Bibliography/bibtex-pdfs/")
(setq org-ref-insert-cite-key "C-c )")
(setq org-ref-default-citation-link "autocite")

;; Add defaults packages to include when exporting.
(add-to-list 'org-latex-packages-alist '("" "graphicx"))
(add-to-list 'org-latex-packages-alist '("" "geometry"))
(add-to-list 'org-latex-packages-alist '("" "hyperref"))
(add-to-list 'org-latex-packages-alist '("" "caption"))
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))
(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass[10pt,article,oneside]{memoir}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
             '("book"
               "\\documentclass[10pt]{memoir}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
             '("xelatex"
               "\\documentclass[11pt,a4paper]{article}
\\usepackage[T1]{fontenc}
\\usepackage{fontspec,lipsum}
\\usepackage{graphicx}
\\defaultfontfeatures{Mapping=tex-text}
\\setromanfont{Gentium}
\\setromanfont [BoldFont={Gentium Basic Bold},
                ItalicFont={Gentium Basic Italic}]{Gentium Basic}
\\setsansfont{Charis SIL}
\\setmonofont[Scale=0.8]{DejaVu Sans Mono}
\\defaultfontfeatures{Ligatures=TeX}
\\usepackage{geometry}
\\usepackage{listings}
\\usepackage{hyperref}
\\usepackage{caption}
\\usepackage{color}
\\usepackage{tikz}
\\geometry{a4paper, textwidth=6.5in, textheight=10in,
            marginparsep=7pt, marginparwidth=.6in}
\\pagestyle{empty}
\\title{}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("pdflatex"
               "\\documentclass[11pt,a4paper]{article}
\\usepackage[T1]{fontenc}
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
\\renewcommand{\\rmdefault}{ptm}
\\pagestyle{empty}
\\title{}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-latex-default-class "pdflatex")

;; Let the exporter use the -shell-escape option to let latex execute external programs.
;; (setq org-latex-pdf-process
;;       '("xelatex -interaction nonstopmode %f"
;;         "bibtex $(basename %b)"
;;         "xelatex -interaction nonstopmode %f"
;;         "xelatex -interaction nonstopmode %f"
;;         )) ;; multipass

(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
        "bibtex $(basename %b)"
        "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
        "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
        )) ;; multipass

;; Tweak the PDF viewer
(eval-after-load "org"
  '(progn
     ;; .txt files aren't in the list initially, but in case that changes
     ;; in a future version of org, use if to avoid errors
     (if (assoc "\\.txt\\'" org-file-apps)
         (setcdr (assoc "\\.txt\\'" org-file-apps) "gedit %s")
       (add-to-list 'org-file-apps '("\\.txt\\'" . "gedit %s") t))
     ;; Change .pdf association directly within the alist
     (setcdr (assoc "\\.pdf\\'" org-file-apps) "acroread %s")))

;; Automatically refresh inline images that are generated from Babel blocks
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; Enable multiple languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (plantuml . t)
   (dot . t)
   (gnuplot . t)
   (sh . t)
   (R . t)
   (perl . t)
   (ruby . t)
   (python . t)
   (js . t)
   (C . t)
   (haskell . t)))

;; Make org do not open other frames
(setq org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame)
                                   (vm-imap . vm-visit-imap-folder-other-frame)
                                   (gnus . org-gnus-no-new-news)
                                   (file . find-file)
                                   (wl . wl-other-frame))))

;; Add missing function
(defun org-reverse-string (string)
  (apply 'string (reverse (string-to-list string))))

;; The next block makes org-babel aware that a lower-case 'r' in a =src= block header should be processed as R.
(add-to-list 'org-src-lang-modes
             '("r" . ess-mode))

;; Don't ask for confirmation on every =C-c C-c= code-block compile.
(setq org-confirm-babel-evaluate nil)

;; Nice bulleted lists
(add-to-list 'load-path "~/.emacs.d/org-bullets")
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; CSS for the HTML
(setq org-html-style-include-scripts nil
      org-html-style-include-default nil)
(setq org-html-style
      "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://thomasf.github.io/solarized-css/solarized-light.min.css\" />")

;; Use Org bold, italics, code styles
(defun org-text-wrapper (txt &optional endtxt)
  "Wraps the region with the text passed in as an argument."
  (if (use-region-p)
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (goto-char (point-min))
        (insert txt)
        (goto-char (point-max))
        (if endtxt
            (insert endtxt)
          (insert txt)))
    (if (looking-at "[A-z]")
        (save-excursion
          (if (not (looking-back "[     ]"))
              (backward-word))
          (progn
            (mark-word)
            (org-text-wrapper txt endtxt)))
      (progn
        (insert txt)
        (let ((spot (point)))
          (insert txt)
          (goto-char spot))))))

(defun org-text-bold () "Wraps the region with asterisks."
  (interactive)
  (org-text-wrapper "*"))

(defun org-text-italics () "Wraps the region with slashes."
  (interactive)
  (org-text-wrapper "/"))

(defun org-text-code () "Wraps the region with equal signs."
  (interactive)
  (org-text-wrapper "="))

;; Stop Org splitting window vertically
(setq org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame)
                                   (vm-imap . vm-visit-imap-folder-other-frame)
                                   (gnus . org-gnus-no-new-news)
                                   (file . find-file)
                                   (wl . wl-other-frame))))


(provide 'setup-org)
;;; setup-org.el ends here
