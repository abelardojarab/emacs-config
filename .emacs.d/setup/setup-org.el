;;; setup-org.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Abelardo Jara-Berrocal

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

;; Org mode
(use-package org
  :defer t
  :mode (("\\.org$" . org-mode))
  :load-path (lambda () (expand-file-name "org/lisp" user-emacs-directory))
  :bind (("C-c C"        . org-capture)
         ("C-c L"        . org-store-link)
         ("C-c I"        . org-insert-link-global)
         ("C-c O"        . org-open-at-point-global)
         :map org-mode-map
         ("C-t"          . org-time-stamp)
         ("<return>"     . org-return)
         ([enter]        . org-return)
         ([kp-enter]     . org-meta-return)
         ([C-M-return]   . org-insert-todo-heading)
         ([S-return]     . org-insert-subheading)
         ("RET"          . org-retun)
         ("C-TAB"        . org-cycle)
         ("C-c c"        . org-html-copy)
         ("C-c k"        . org-cut-subtree)
         ("C-c t"        . org-show-todo-tree)
         ("C-c r"        . org-refile)
         ("C-c v"        . org-reveal))
  :commands (org-mode
             org-capture
             org-store-link
             org-insert-link-global
             org-open-at-point-global
             orgtbl-mode
             orgstruct-mode
             orgstruct++-mode
             org-agenda)
  :init (progn
          (setq load-path (cons (expand-file-name "org/contrib/lisp" user-emacs-directory) load-path))
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
          (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)))
  :config (progn
            ;; Basic packages
            (use-package org-list)
            (use-package ox-org)
            (use-package ox-extra)

            ;; Avoid error when inserting '_'
            (defadvice org-backward-paragraph (around bar activate)
              (ignore-errors add-do-it))

            ;; Tweaks
            (add-hook 'org-mode-hook
                      (lambda ()
                        (progn
                          (linum-mode -1)
                          (cua-mode t)
                          (flyspell-mode t)
                          (writegood-mode t)
                          (yas-minor-mode t)
                          (undo-tree-mode t))))

            ;; Ignore tex commands during flyspell
            (add-hook 'org-mode-hook (lambda () (setq ispell-parser 'tex)))
            (defun flyspell-ignore-tex ()
              (interactive)
              (set (make-variable-buffer-local 'ispell-parser) 'tex))
            (add-hook 'org-mode-hook #'flyspell-ignore-tex)

            ;; source http://endlessparentheses.com/ispell-and-org-mode.html
            (defun my/org-ispell ()
              "Configure `ispell-skip-region-alist' for `org-mode'."
              (make-local-variable 'ispell-skip-region-alist)
              (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
              (add-to-list 'ispell-skip-region-alist '("~" "~"))
              (add-to-list 'ispell-skip-region-alist '("=" "="))
              (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "^#\\+END_SRC"))
              (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))
              (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_QUOTE" . "#\\+END_QUOTE")))
            (add-hook 'org-mode-hook #'my/org-ispell)

            ;; Bind org-table-* command when the point is in an org table (source).
            (bind-keys
             :map org-mode-map
             :filter (org-at-table-p)
             ("C-c ?"   . org-table-field-info)
             ("C-c SPC" . org-table-blank-field)
             ("C-c +"   . org-table-sum)
             ("C-c ="   . org-table-eval-formula)
             ("C-c `"   . org-table-edit-field)
             ("C-#"     . org-table-rotate-recalc-marks)
             ("C-c }"   . org-table-toggle-coordinate-overlays)
             ("C-c {"   . org-table-toggle-formula-debugger))

            ;; Miscellanenous settings
            (setq org-startup-folded              t
                  org-startup-indented            t
                  org-cycle-separator-lines       1
                  org-cycle-include-plain-lists   'integrate
                  org-startup-with-inline-images  nil
                  org-startup-truncated           t
                  org-use-speed-commands          t
                  org-completion-use-ido          t
                  org-hide-leading-stars          t
                  org-highlight-latex-and-related '(latex)
                  org-ellipsis                    " ••• "

                  ;; this causes problem in other modes
                  org-indent-mode                 nil

                  ;; Enable sub and super script only when enclosed by {}
                  ;; Also improves readability when exponent/subscript is composed of multiple words
                  org-use-sub-superscripts        nil

                  ;; Hide the /italics/ and *bold* markers
                  org-hide-emphasis-markers      t

                  ;; org-entities displays \alpha etc. as Unicode characters.
                  org-pretty-entities            t

                  ;; Allow a) b) c) lists
                  org-list-allow-alphabetical    t

                  ;; Right-align tags to an indent from the right margin
                  org-tags-column                120)

            ;; Allow quotes to be verbatim
            (add-hook 'org-mode-hook
                      (lambda ()
                        (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\n,'")
                        (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
                        (org-element--set-regexps)
                        (custom-set-variables `(org-emphasis-alist ',org-emphasis-alist))))

            ;; Export options
            (setq org-export-coding-system         'utf-8
                  org-export-time-stamp-file       nil
                  org-export-with-sub-superscripts '{}
                  org-export-allow-bind-keywords   t
                  org-export-async-debug           t

                  ;; Turn ' and " into ‘posh’ “quotes”
                  org-export-with-smart-quotes     t)

            ;; Enable mouse in Org
            (use-package org-mouse)

            ;; Fonts
            (add-hook 'org-mode-hook (lambda () (variable-pitch-mode t)))
            (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
            (set-face-attribute 'org-code  nil :inherit 'fixed-pitch)
            (set-face-attribute 'org-block nil :inherit 'fixed-pitch)

            (defface org-block-begin-line
              '((t (:inherit org-meta-line
                             :overline "light grey" :foreground "#008ED1")))
              "Face used for the line delimiting the begin of source blocks.")

            (defface org-block-end-line
              '((t (:inherit org-meta-line
                             :underline "light grey" :foreground "#008ED1")))
              "Face used for the line delimiting the end of source blocks.")

            ;; Org Templates
            (setq org-structure-template-alist
                  '(("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
                    ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
                    ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
                    ("v" "#+BEGIN_VERSE\n?\n#+END_VERSE" "<verse>\n?\n</verse>")
                    ("c" "#+BEGIN_COMMENT\n?\n#+END_COMMENT")
                    ("p" "#+BEGIN_PRACTICE\n?\n#+END_PRACTICE")
                    ("l" "#+begin_src emacs-lisp\n?\n#+end_src" "<src lang=\"emacs-lisp\">\n?\n</src>")
                    ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
                    ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
                    ("H" "#+html: " "<literal style=\"html\">?</literal>")
                    ("E" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC\n")
                    ("S" "#+BEGIN_SRC shell-script\n?\n#+END_SRC\n")
                    ("L" "#+BEGIN_SRC latex\n\\begin{align*}\n?\\end{align*}\n#+END_SRC")
                    ("a" "#+begin_ascii\n?\n#+end_ascii")
                    ("A" "#+ascii: ")
                    ("i" "#+index: ?" "#+index: ?")
                    ("I" "#+include %file ?" "<include file=%file markup=\"?\">")))

            ;; Fix shift problem in Org mode
            (setq org-CUA-compatible t)
            (setq org-support-shift-select 'always)
            (eval-after-load "org"
              '(progn
                 (eval-after-load "cua-base"
                   '(progn
                      (defadvice org-call-for-shift-select (before org-call-for-shift-select-cua activate)
                        (if (and cua-mode
                                 org-support-shift-select
                                 (not (use-region-p)))
                            (cua-set-mark)))))))

            ;; Make org do not open other frames
            (setq org-link-frame-setup (quote ((vm      . vm-visit-folder-other-frame)
                                               (vm-imap . vm-visit-imap-folder-other-frame)
                                               (gnus    . org-gnus-no-new-news)
                                               (file    . find-file)
                                               (wl      . wl-other-frame))))

            ;; Repair export blocks to match new format
            ;; #+BEGIN_EXPORT backend
            ;; ...
            ;; #+END_EXPORT
            (defun org-repair-export-blocks ()
              "Repair export blocks and INCLUDE keywords in current buffer."
              (interactive)
              (when (eq major-mode 'org-mode)
                (let ((case-fold-search t)
                      (back-end-re (regexp-opt
                                    '("HTML" "ASCII" "LATEX" "ODT" "MARKDOWN" "MD" "ORG"
                                      "MAN" "BEAMER" "TEXINFO" "GROFF" "KOMA-LETTER")
                                    t)))
                  (org-with-wide-buffer
                   (goto-char (point-min))
                   (let ((block-re (concat "^[ \t]*#\\+BEGIN_" back-end-re)))
                     (save-excursion
                       (while (re-search-forward block-re nil t)
                         (let ((element (save-match-data (org-element-at-point))))
                           (when (eq (org-element-type element) 'special-block)
                             (save-excursion
                               (goto-char (org-element-property :end element))
                               (save-match-data (search-backward "_"))
                               (forward-char)
                               (insert "EXPORT")
                               (delete-region (point) (line-end-position)))
                             (replace-match "EXPORT \\1" nil nil nil 1))))))
                   (let ((include-re
                          (format "^[ \t]*#\\+INCLUDE: .*?%s[ \t]*$" back-end-re)))
                     (while (re-search-forward include-re nil t)
                       (let ((element (save-match-data (org-element-at-point))))
                         (when (and (eq (org-element-type element) 'keyword)
                                    (string= (org-element-property :key element) "INCLUDE"))
                           (replace-match "EXPORT \\1" nil nil nil 1)))))))))

            ;; Add missing function
            (defun org-reverse-string (string)
              (apply 'string (reverse (string-to-list string))))

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

            ;; Org formatted copy and paste
            (if (equal system-type 'darwin)
                (progn
                  (defun org-html-paste ()
                    "Convert clipboard contents from HTML to Org and then paste (yank)."
                    (interactive)
                    (kill-new (shell-command-to-string "osascript -e 'the clipboard as \"HTML\"' | perl -ne 'print chr foreach unpack(\"C*\",pack(\"H*\",substr($_,11,-3)))' | pandoc -f html -t json | pandoc -f json -t org"))
                    (yank))

                  (defun org-html-copy ()
                    "Export region to HTML, and copy it to the clipboard."
                    (interactive)
                    (save-window-excursion
                      (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
                             (html (with-current-buffer buf (buffer-string))))
                        (with-current-buffer buf
                          (shell-command-on-region
                           (point-min)
                           (point-max)
                           "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
                        (kill-buffer buf)))))
              (progn
                (defun org-html-paste ()
                  "Convert clipboard contents from HTML to Org and then paste (yank)."
                  (interactive)
                  (kill-new (shell-command-to-string
                             "xclip -o -t TARGETS | grep -q text/html && (xclip -o -t text/html | pandoc -f html -t json | pandoc -f json -t org) || xclip -o"))
                  (yank))

                (defun org-html-copy ()
                  "Export region to HTML, and copy it to the clipboard."
                  (interactive)
                  (org-export-to-file 'html "/tmp/org.html")
                  (apply
                   'start-process "xclip" "*xclip*"
                   (split-string
                    "xclip -verbose -i /tmp/org.html -t text/html -selection clipboard" " ")))))

            ;; convert csv to org-table considering "12,12"
            (defun org-convert-csv-table (beg end)
              (interactive (list (point) (mark)))
              (replace-regexp "\\(^\\)\\|\\(\".*?\"\\)\\|," (quote (replace-eval-replacement
                                                                    replace-quote (cond ((equal "^" (match-string 1)) "|")
                                                                                        ((equal "," (match-string 0)) "|")
                                                                                        ((match-string 2))) ))  nil  beg end))

            ;; Footnote definitions
            (setq org-footnote-definition-re (org-re "^\\[\\(fn:[-_[:word:]]+\\)\\]")
                  org-footnote-re (concat "\\[\\(?:"
                                          ;; Match inline footnotes.
                                          (org-re "fn:\\([-_[:word:]]+\\)?:\\|")
                                          (org-re "\\(fn:[-_[:word:]]+\\)")
                                          "\\)"))

            ;; .txt files aren't in the list initially, but in case that changes
            ;; in a future version of org, use if to avoid errors
            (if (assoc "\\.txt\\'" org-file-apps)
                (setcdr (assoc "\\.txt\\'" org-file-apps) "kate %s")
              (add-to-list 'org-file-apps '("\\.txt\\'" . "kate %s") t))

            ;; Change .pdf association directly within the alist
            (setcdr (assoc "\\.pdf\\'" org-file-apps) "acroread %s")

            ;; Beamer/ODT/Markdown exporters
            (use-package ox-beamer)
            (use-package ox-odt)
            (use-package ox-md)

            ;; Org extensions

            ;; Setup Org Agenda
            (use-package setup-org-agenda)

            ;; Setup Org (babel support)
            (use-package setup-org-babel)

            ;; Setup Org plugins
            (use-package setup-org-plugins)

            ;; Setup Org (latex support)
            (use-package setup-org-latex)

            ;; Setup Org (html support)
            (use-package setup-org-html)

            ;; Use footnotes as eldoc source
            (use-package org-eldoc
              :defer t
              :init (progn
                      (add-hook 'org-mode-hook #'org-eldoc-load)
                      (add-hook 'org-mode-hook #'eldoc-mode))
              :commands org-eldoc-load
              :config (progn
                        (defun my/org-eldoc-get-footnote ()
                          (save-excursion
                            (let ((fn (org-between-regexps-p "\\[fn:" "\\]")))
                              (when fn
                                (save-match-data
                                  (nth 3 (org-footnote-get-definition (buffer-substring (+ 1 (car fn)) (- (cdr fn) 1)))))))))
                        (advice-add 'org-eldoc-documentation-function
                                    :before-until #'my/org-eldoc-get-footnote)))))

(provide 'setup-org)
;;; setup-org.el ends here
