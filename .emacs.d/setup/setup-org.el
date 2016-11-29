;;; setup-org.el ---

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

;; Org mode
(use-package org
  :mode (("\\.org$" . org-mode))
  :load-path (lambda () (expand-file-name "org-mode/lisp" user-emacs-directory))
  :bind (("C-c C" . org-capture)
         ("C-c A" . org-agenda)
         ("C-c L" . org-store-link)
         ("C-c I" . org-insert-link-global)
         ("C-c O" . org-open-at-point-global)
         :map org-mode-map
         ("C-TAB" . org-cycle)
         ("C-c c" . org-html-copy)
         ("C-c k" . org-cut-subtree)
         ("C-c t" . org-show-todo-tree)
         ("C-c r" . org-refile)
         ("C-c v" . org-reveal))
  :commands (org-mode org-capture org-store-link org-insert-link-global org-open-at-point-global)
  :init (progn
          (setq load-path (cons (expand-file-name "org-mode/contrib/lisp" user-emacs-directory) load-path))
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
            (require 'org-list)
            (require 'ox-org)

            ;; Tweaks
            (add-hook 'org-mode-hook
                      (lambda ()
                        (progn
                          (linum-mode -1)
                          (cua-mode t)
                          (flyspell-mode t)
                          (writegood-mode t)
                          (yas-minor-mode t))))

            ;; Ignore tex commands during flyspell
            (add-hook 'org-mode-hook (lambda () (setq ispell-parser 'tex)))
            (defun flyspell-ignore-tex ()
              (interactive)
              (set (make-variable-buffer-local 'ispell-parser) 'tex))
            (add-hook 'org-mode-hook 'flyspell-ignore-tex)

            ;; Set up Org default files
            (let ((todo_workspace "~/workspace/Documents/Org/todo.org")
                  (todo_dropbox "~/Dropbox/Documents/Org/todo.org")
                  (todo_googledrive "~/Google Drive/Documents/Org/todo.org"))
              (when (or (file-readable-p todo_workspace)
                        (file-readable-p todo_dropbox)
                        (file-readable-p todo_googledrive))

                ;; Set base Org directory
                (if (file-exists-p todo_dropbox)
                    (setq org-directory "~/Dropbox/Documents/Org")
                  (if (file-exists-p todo_workspace)
                      (setq org-directory "~/workspace/Documents/Org")
                    (setq org-directory "~/Google Drive/Documents/Org")))

                ;; Set remaining Org files
                (setq diary-file (concat org-directory "/diary"))
                (setq org-id-locations-file "~/.emacs.cache/org-id-locations")
                (setq org-default-notes-file (concat org-directory "/notes.org"))
                (setq org-default-refile-file (concat org-directory "/refile.org"))
                (setq org-agenda-diary-file (concat org-directory "/todo.org"))
                (setq org-mobile-file (concat org-directory "/mobileorg.org"))
                (setq org-mobile-directory (concat org-directory "/mobile-org"))
                (setq org-agenda-files (list
                                        (concat org-directory "/diary.org")
                                        (concat org-directory "/agenda.org")
                                        (concat org-directory "/todo.org")
                                        (concat org-directory "/notes.org")
                                        (concat org-directory "/mobileorg.org")
                                        (concat org-directory "/refile.org")))))

            ;; Miscellanenous settings
            (setq org-startup-folded t
                  org-startup-indented t
                  org-cycle-separator-lines 1
                  org-cycle-include-plain-lists 'integrate
                  org-startup-with-inline-images nil
                  org-startup-truncated t
                  org-blank-before-new-entry '((heading . auto) (plain-list-item . auto))
                  org-refile-targets '((org-agenda-files :maxlevel . 9))
                  org-src-fontify-natively t
                  org-src-tab-acts-natively t
                  org-confirm-babel-evaluate nil
                  org-use-speed-commands t
                  org-completion-use-ido t
                  org-hide-leading-stars t
                  org-hide-emphasis-markers t
                  org-log-done t
                  org-enforce-todo-dependencies t
                  org-indent-mode nil) ;; this causes problem in other modes

            ;; Insert blank line before new heading
            (setq org-blank-before-new-entry
                  '((heading . t)))

            ;; Allow quotes to be verbatim
            (add-hook 'org-mode-hook
                      (lambda ()
                        (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\n,'")
                        (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
                        (org-element--set-regexps)
                        (custom-set-variables `(org-emphasis-alist ',org-emphasis-alist))))

            ;; Agenda settings
            (setq org-agenda-inhibit-startup t ;; 50x speedup
                  org-agenda-use-tag-inheritance nil ;; 3-4x speedup
                  org-agenda-show-log t
                  org-agenda-show-all-dates t
                  org-agenda-start-on-weekday nil
                  org-agenda-span 14
                  org-agenda-ndays 14
                  org-agenda-include-diary t
                  org-agenda-window-setup 'current-window
                  org-agenda-skip-scheduled-if-done t
                  org-agenda-skip-deadline-if-done t
                  org-deadline-warning-days 7
                  org-agenda-time-grid
                  '((daily today require-timed)
                    "----------------"
                    (800 1000 1200 1400 1600 1800)))

            ;; Export options
            (setq org-export-time-stamp-file nil)
            (setq org-export-with-smart-quotes t) ;; curly quotes in HTML
            (setq org-export-with-sub-superscripts '{})
            (setq org-export-allow-bind-keywords t)
            (setq org-export-async-debug t)

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

            ;; Beamer/ODT/Markdown support
            (require 'ox-beamer)
            (require 'ox-odt)
            (require 'ox-md)

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

            ;; Fix on the keys
            (add-hook 'org-mode-hook
                      (lambda ()
                        (define-key org-mode-map [(control t)] 'org-time-stamp)
                        (define-key org-mode-map [kp-enter] 'org-meta-return)
                        (define-key org-mode-map [enter] 'org-return)
                        (define-key org-mode-map (kbd "<return>") 'org-return)
                        (define-key org-mode-map (kbd "RET") 'org-return)
                        (define-key org-mode-map [C-M-return] 'org-insert-todo-heading)
                        (define-key org-mode-map [S-return] 'org-insert-subheading)))

            ;; define todo states: set time stamps one waiting, delegated and done
            (setq org-todo-keywords
                  '((sequence "TODO(t!)" "|" "DONE(d!)" "CANCELLED(c@/!)" "HOLD(h!)")
                    (sequence "TASK(t!)" "|" "DONE(d!)" "IN PROGRESS(p@/!)" "CANCELLED(c@/!)")
                    (sequence "IDEA(i!)" "MAYBE(y!)" "STAGED(s!)" "WORKING(k!)" "|" "USED(u!/@)")))
            (setq org-todo-keyword-faces
                  '(("IN PROGRESS" . 'warning)
                    ("HOLD" . 'font-lock-keyword-face)
                    ("TASK" . 'font-lock-builtin-face)
                    ("CANCELLED" . 'font-lock-doc-face)))

            ;; Tag tasks with GTD-ish contexts
            (setq org-tag-alist '(("@work" . ?b)
                                  ("@home" . ?h)
                                  ("@place" . ?p)
                                  ("@writing" . ?w)
                                  ("@errands" . ?e)
                                  ("@family" . ?f)
                                  ("@coding" . ?c)
                                  ("@tasks" . ?t)
                                  ("@learning" . ?l)
                                  ("@reading" . ?r)
                                  ("time" . ?q)
                                  ("high-energy" . ?1)))

            ;; Enable filtering by effort estimates
            (add-to-list 'org-global-properties
                         '("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00"))

            ;; Make org do not open other frames
            (setq org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame)
                                               (vm-imap . vm-visit-imap-folder-other-frame)
                                               (gnus . org-gnus-no-new-news)
                                               (file . find-file)
                                               (wl . wl-other-frame))))

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

                  (if (equal system-type 'darwin)
                      ;; Mac OSX pngpaste utility: https://github.com/jcsalterego/pngpaste
                      (call-process "pngpaste" nil nil nil filename)

                    ;; Linux: ImageMagick: (call-process "import" nil nil nil filename)
                    (call-process "import" nil nil nil filename))
                  ) ;; if
                (insert (concat "[[file:" filename "]]"))
                (org-display-inline-images)))))

;; ASCII doc
(use-package ox-asciidoc
  :load-path (lambda () (expand-file-name "org-asciidoc/" user-emacs-directory)))

;; Table ASCII plot
(use-package orgtbl-ascii-plot
  :load-path (lambda () (expand-file-name "orgtblasciiplot/" user-emacs-directory)))

;; Org Table of Contents
(use-package toc-org
  :load-path (lambda () (expand-file-name "toc-org/" user-emacs-directory))
  :config(add-hook 'org-mode-hook 'toc-org-enable))

;; Nice bulleted lists
(use-package org-autolist
  :load-path (lambda () (expand-file-name "org-autolist/" user-emacs-directory))
  :config (add-hook 'org-mode-hook (lambda () (org-autolist-mode))))

;; Nice bulleted lists
(use-package org-bullets
  :unless (or (not (display-graphic-p))
              (equal system-type 'windows-nt))
  :load-path (lambda () (expand-file-name "org-bullets/" user-emacs-directory))
  :config (progn
            (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

;; Lorg calendar
(use-package lorg-calendar
  :load-path (lambda () (expand-file-name "lorg-calendar/" user-emacs-directory)))

(provide 'setup-org)
;;; setup-org.el ends here
