;;; setup-org.el ---                               -*- lexical-binding: t; -*-

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

;; Org mode
(use-package org
  :demand t
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
         ([C-tab]        . org-cycle)
         ("RET"          . org-return)
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
  :custom ((org-startup-folded              'showall)
           (org-startup-indented            t)
           (org-cycle-separator-lines       1)
           (org-cycle-include-plain-lists   'integrate)
           (org-startup-with-inline-images  nil)
           (org-startup-truncated           t)
           (org-use-speed-commands          t)
           (org-completion-use-ido          t)
           (org-hide-leading-stars          nil)
           (org-highlight-latex-and-related '(latex))
           (org-catch-invisible-edits       'smart)
           (org-indent-mode                 nil)
           (org-use-sub-superscripts        "{}")
           (org-hide-emphasis-markers       t)
           (org-pretty-entities             t)
           (org-list-allow-alphabetical     t)
           (org-tags-column                 120)
           (org-support-shift-select        'always))
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
          (defun org-element-verbatim-successor       (arg)))
  :config (progn
            ;; Basic packages
            (use-package org-list)
            (use-package org-indent)

            ;; Avoid error when inserting '_'
            (defadvice org-backward-paragraph (around bar activate)
              (ignore-errors add-do-it))

            ;; Fancy ellipsis
            (if (display-graphic-p)
                (setq org-ellipsis "⮷"))

            ;; Tweaks
            (add-hook 'org-mode-hook
                      (lambda ()
                        (progn
                          (linum-mode         -1)
                          (org-indent-mode    -1)
                          (flyspell-mode      t)
                          (writegood-mode     t))))

            ;; Hide properties drawer in org mode
            (defalias 'org-cycle-hide-drawers 'my/block-org-cycle-hide-drawers)
            (defun my/block-org-cycle-hide-drawers (state)
              "Re-hide all drawers, footnotes or html blocks after a visibility state change."
              (when
                  (and
                   (derived-mode-p 'org-mode)
                   (not (memq state '(overview folded contents))))
                (save-excursion
                  (let* (
                         (globalp (memq state '(contents all)))
                         (beg (if globalp (point-min) (point)))
                         (end
                          (cond
                           (globalp
                            (point-max))
                           ((eq state 'children)
                            (save-excursion (outline-next-heading) (point)))
                           (t (org-end-of-subtree t)) )))
                    (goto-char beg)
                    (while
                        (re-search-forward
                         ".*\\[fn\\|^\\#\\+BEGIN_SRC.*$\\|^[ \t]*:PROPERTIES:[ \t]*$" end t)
                      (my/org-flag t))))))

            (defalias 'org-cycle-internal-local 'my/block-org-cycle-internal-local)

            (defun my/block-org-cycle-internal-local ()
              "Do the local cycling action."
              (let ((goal-column 0) eoh eol eos has-children children-skipped struct)
                (save-excursion
                  (if (org-at-item-p)
                      (progn
                        (beginning-of-line)
                        (setq struct (org-list-struct))
                        (setq eoh (point-at-eol))
                        (setq eos (org-list-get-item-end-before-blank (point) struct))
                        (setq has-children (org-list-has-child-p (point) struct)))
                    (org-back-to-heading)
                    (setq eoh (save-excursion (outline-end-of-heading) (point)))
                    (setq eos (save-excursion (1- (org-end-of-subtree t t))))
                    (setq has-children
                          (or
                           (save-excursion
                             (let ((level (funcall outline-level)))
                               (outline-next-heading)
                               (and
                                (org-at-heading-p t)
                                (> (funcall outline-level) level))))
                           (save-excursion
                             (org-list-search-forward (org-item-beginning-re) eos t)))))
                  (beginning-of-line 2)
                  (if (featurep 'xemacs)
                      (while
                          (and
                           (not (eobp))
                           (get-char-property (1- (point)) 'invisible))
                        (beginning-of-line 2))
                    (while
                        (and
                         (not (eobp))
                         (get-char-property (1- (point)) 'invisible))
                      (goto-char (next-single-char-property-change (point) 'invisible))
                      (and
                       (eolp)
                       (beginning-of-line 2))))
                  (setq eol (point)))
                (cond
                 ((= eos eoh)
                  (unless (org-before-first-heading-p)
                    (run-hook-with-args 'org-pre-cycle-hook 'empty))
                  (org-unlogged-message "EMPTY ENTRY")
                  (setq org-cycle-subtree-status nil)
                  (save-excursion
                    (goto-char eos)
                    (outline-next-heading)
                    (if (outline-invisible-p)
                        (org-flag-heading nil))))
                 ((and
                   (or
                    (>= eol eos)
                    (not (string-match "\\S-" (buffer-substring eol eos))))
                   (or
                    has-children
                    (not (setq children-skipped
                               org-cycle-skip-children-state-if-no-children))))
                  (unless (org-before-first-heading-p)
                    (run-hook-with-args 'org-pre-cycle-hook 'children))
                  (if (org-at-item-p)
                      ;; then
                      (org-list-set-item-visibility (point-at-bol) struct 'children)
                    ;; else
                    (org-show-entry)
                    (org-with-limited-levels (show-children))
                    (when (eq org-cycle-include-plain-lists 'integrate)
                      (save-excursion
                        (org-back-to-heading)
                        (while (org-list-search-forward (org-item-beginning-re) eos t)
                          (beginning-of-line 1)
                          (let* (
                                 (struct (org-list-struct))
                                 (prevs (org-list-prevs-alist struct))
                                 (end (org-list-get-bottom-point struct)))
                            (mapc (lambda (e) (org-list-set-item-visibility e struct 'folded))
                                  (org-list-get-all-items (point) struct prevs))
                            (goto-char (if (< end eos) end eos)))))))
                  (org-unlogged-message "CHILDREN")
                  (save-excursion
                    (goto-char eos)
                    (outline-next-heading)
                    (if (outline-invisible-p)
                        (org-flag-heading nil)))
                  (setq org-cycle-subtree-status 'children)
                  (unless (org-before-first-heading-p)
                    (run-hook-with-args 'org-cycle-hook 'children)))
                 ((or
                   children-skipped
                   (and
                    (eq last-command this-command)
                    (eq org-cycle-subtree-status 'children)))
                  (unless (org-before-first-heading-p)
                    (run-hook-with-args 'org-pre-cycle-hook 'subtree))
                  (outline-flag-region eoh eos nil)
                  (org-unlogged-message
                   (if children-skipped
                       "SUBTREE (NO CHILDREN)"
                     "SUBTREE"))
                  (setq org-cycle-subtree-status 'subtree)
                  (unless (org-before-first-heading-p)
                    (run-hook-with-args 'org-cycle-hook 'subtree)))
                 ((eq org-cycle-subtree-status 'subtree)
                  (org-show-subtree)
                  (message "ALL")
                  (setq org-cycle-subtree-status 'all))
                 (t
                  (run-hook-with-args 'org-pre-cycle-hook 'folded)
                  (outline-flag-region eoh eos t)
                  (org-unlogged-message "FOLDED")
                  (setq org-cycle-subtree-status 'folded)
                  (unless (org-before-first-heading-p)
                    (run-hook-with-args 'org-cycle-hook 'folded))))))

            (defun my/org-flag (flag)
              "When FLAG is non-nil, hide any of the following:  html code block;
footnote; or, the properties drawer.  Otherwise make it visible."
              (save-excursion
                (beginning-of-line 1)
                (cond
                 ((looking-at ".*\\[fn")
                  (let* (
                         (begin (match-end 0))
                         end-footnote)
                    (if (re-search-forward "\\]"
                                           (save-excursion (outline-next-heading) (point)) t)
                        (progn
                          (setq end-footnote (point))
                          (outline-flag-region begin end-footnote flag))
                      (user-error "Error beginning at point %s." begin))))
                 ((looking-at "^\\#\\+BEGIN_SRC.*$\\|^[ \t]*:PROPERTIES:[ \t]*$")
                  (let* ((begin (match-end 0)))
                    (if (re-search-forward "^\\#\\+END_SRC.*$\\|^[ \t]*:END:"
                                           (save-excursion (outline-next-heading) (point)) t)
                        (outline-flag-region begin (point-at-eol) flag)
                      (user-error "Error beginning at point %s." begin)))))))

            (defun my/toggle-block-visibility ()
              "For this function to work, the cursor must be on the same line as the regexp."
              (interactive)
              (if
                  (save-excursion
                    (beginning-of-line 1)
                    (looking-at
                     ".*\\[fn\\|^\\#\\+BEGIN_SRC.*$\\|^[ \t]*:PROPERTIES:[ \t]*$"))
                  (my/org-flag (not (get-char-property (match-end 0) 'invisible)))
                (message "Sorry, you are not on a line containing the beginning regexp.")))

            ;; Only use bimodal org-cycle when a heading has a :BIMODAL-CYCLING: property value.
            (advice-add 'org-cycle :around #'my/org-cycle)
            (defun my/toggle-bimodal-cycling (&optional pos)
              "Enable/disable bimodal cycling behavior for the current heading."
              (interactive)
              (let* ((enabled (org-entry-get pos "BIMODAL-CYCLING")))
                (if enabled
                    (org-entry-delete pos "BIMODAL-CYCLING")
                  (org-entry-put pos "BIMODAL-CYCLING" "yes"))))

            (defun my/org-cycle (fn &optional arg)
              "Make org outline cycling bimodal (FOLDED and SUBTREE) rather than trimodal (FOLDED, CHILDREN, and SUBTREE) when a heading has a :BIMODAL-CYCLING: property value."
              (interactive)
              (if (and (org-at-heading-p)
                       (org-entry-get nil "BIMODAL-CYCLING"))
                  (my/toggle-subtree)
                (funcall fn arg)))

            (defun my/toggle-subtree ()
              "Show or hide the current subtree depending on its current state."
              (interactive)
              (save-excursion
                (outline-back-to-heading)
                (if (not (outline-invisible-p (line-end-position)))
                    (outline-hide-subtree)
                  (outline-show-subtree))))

            ;; Ignore tex commands during flyspell
            (add-hook 'org-mode-hook (lambda () (setq ispell-parser 'tex)))
            (defun flyspell-ignore-tex ()
              (interactive)
              (set (make-variable-buffer-local 'ispell-parser) 'tex))
            (add-hook 'org-mode-hook #'flyspell-ignore-tex)

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

            ;; Allow multiple line Org emphasis markup
            (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;; Up to 20 lines

            ;; Below is needed to apply the modified `org-emphasis-regexp-components'
            ;; settings from above.
            (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

            ;; Allow quotes to be verbatim
            (add-hook 'org-mode-hook
                      (lambda ()
                        (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\n,'")
                        (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
                        (org-element--set-regexps)
                        (custom-set-variables `(org-emphasis-alist ',org-emphasis-alist))))

            ;; Enable mouse in Org
            (use-package org-mouse)

            ;; Fonts
            (eval-after-load 'org-indent
              '(set-face-attribute 'org-indent nil :inherit '(fixed-pitch org-hide)))
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

            ;; Easy Templates
            ;; Copy of the old "Easy Templates" feature that was removed in
            (defconst org-easy-template-alist  ;; Old `org-structure-template-alist'
              '(("s" "#+begin_src ?\n\n#+end_src")
                ("bd" "#+begin_description\n?\n#+end_description") ;Special block in `ox-hugo'
                ("bn" "#+begin_note\n?\n#+end_note") ;Special block in `ox-hugo'
                ("e" "#+begin_example\n?\n#+end_example")
                ("q" "#+begin_quote\n?\n#+end_quote")
                ("v" "#+begin_verse\n?\n#+end_verse")
                ("V" "#+begin_verbatim\n?\n#+end_verbatim")
                ("c" "#+begin_center\n?\n#+end_center")
                ("C" "#+begin_comment\n?\n#+end_comment")
                ("X" "#+begin_export ?\n\n#+end_export")
                ("l" "#+begin_export latex\n?\n#+end_export")
                ("L" "#+latex: ")
                ("h" "#+begin_export html\n?\n#+end_export")
                ("H" "#+html: ")
                ("a" "#+begin_export ascii\n?\n#+end_export")
                ("A" "#+ascii: ")
                ("i" "#+index: ?")
                ("I" "#+include: %file ?"))
              "Structure completion elements.
This is a list of abbreviation keys and values.  The value gets
inserted if you type `<' followed by one or more characters and
then press the completion key, usually `TAB'.  %file will be
replaced by a file name after prompting for the file using
completion.  The cursor will be placed at the position of the `?'
in the template.")

            (defun org-try-structure-completion ()
              "Try to complete a structure template before point.
This looks for strings like \"<e\" on an otherwise empty line and
expands them."
              (let ((l (buffer-substring (point-at-bol) (point)))
                    a)
                (when (and (looking-at "[ \t]*$")
                           (string-match "^[ \t]*<\\([a-zA-Z]+\\)$" l)
                           (setq a (assoc (match-string 1 l) org-easy-template-alist)))
                  (org-complete-expand-structure-template (+ -1 (point-at-bol)
                                                             (match-beginning 1))
                                                          a)
                  t)))

            (defun org-complete-expand-structure-template (start cell)
              "Expand a structure template."
              (let ((rpl (nth 1 cell))
                    (ind ""))
                (delete-region start (point))
                (when (string-match "\\`[ \t]*#\\+" rpl)
                  (cond
                   ((bolp))
                   ((not (string-match "\\S-" (buffer-substring (point-at-bol) (point))))
                    (setq ind (buffer-substring (point-at-bol) (point))))
                   (t (newline))))
                (setq start (point))
                (when (string-match "%file" rpl)
                  (setq rpl (replace-match
                             (concat
                              "\""
                              (save-match-data
                                (abbreviate-file-name (read-file-name "Include file: ")))
                              "\"")
                             t t rpl)))
                (setq rpl (mapconcat 'identity (split-string rpl "\n")
                                     (concat "\n" ind)))
                (insert rpl)
                (when (re-search-backward "\\?" start t) (delete-char 1))))

            (defun my/org-template-expand (str &optional arg)
              "Expand Org template based on STR."
              (let* ((is-region? (use-region-p))
                     (beg (if is-region?
                              (region-beginning)
                            (point)))
                     ;; Copy marker for end so that if characters are added/removed
                     ;; before the `end', the reference end point is updated (because of
                     ;; being a marker).
                     (end (when is-region?
                            (copy-marker (region-end) t)))
                     content post-src-export column)

                (goto-char beg)

                ;; Save the indentation level of the content (if region is selected) or
                ;; the point (if region is not selected).
                (save-excursion
                  (forward-line 0) ;; Go to BOL
                  (when (looking-at "[[:blank:]]")
                    (back-to-indentation)
                    (setq column (current-indentation))))

                (when is-region?
                  ;; Update `beg' if needed..
                  ;; If `beg' is at BOL, update `beg' to be at the indentation.
                  (when (and (bolp)
                             column)
                    (back-to-indentation)
                    (setq beg (point)))

                  ;; Insert a newline if `beg' is *not* at BOL.
                  ;; Example: You have ^abc$ where ^ is BOL and $ is EOL.
                  ;;          "bc" is selected and pressing <e should result in:
                  ;;            a
                  ;;            #+begin_example
                  ;;            bc
                  ;;            #+end_example
                  (unless (or (bolp)
                              (looking-back "^[[:blank:]]*"))
                    (insert "\n")
                    (when column
                      (indent-to column))
                    (setq beg (point)))

                  ;; Update `end' if needed ..
                  (goto-char end)
                  (cond
                   ((bolp)                      ;`end' is at BOL
                    (skip-chars-backward " \n\t")
                    (set-marker end (point)))
                   ((and (not (bolp)) ;; `end' is neither at BOL nor at EOL
                         (not (looking-at "[[:blank:]]*$")))
                    ;; Insert a newline if `end' is neither at BOL nor EOL
                    ;; Example: You have ^abc$ where ^ is bol and $ is eol.
                    ;;          "a" is selected and pressing <e should result in:
                    ;;            #+begin_example
                    ;;            a
                    ;;            #+end_example
                    ;;            bc
                    (insert "\n")
                    (when column
                      (indent-to column))
                    (skip-chars-backward " \n\t")
                    (set-marker end (point)))
                   (t ;; `end' is either at EOL or looking at trailing whitespace
                    ))
                  ;; Now delete the content in the selected region and save it to
                  ;; `content'.
                  (setq content (delete-and-extract-region beg end))
                  ;; Make the `end' marker point to nothing as its job is done.
                  ;; http://lists.gnu.org/archive/html/emacs-orgmode/2017-09/msg00049.html
                  ;; (elisp) Overview of Markers
                  (set-marker end nil))

                ;; Insert the `str' required for template expansion (example: "<e").
                (insert str)
                (org-try-structure-completion)
                (when (let* ((case-fold-search t)) ;Ignore case
                        (looking-back "^[[:blank:]]*#\\+begin_\\(src\\|export\\)[[:blank:]]+"))
                  (cond
                   ((stringp arg)
                    (insert arg)
                    (forward-line))
                   ((and (null arg) ;; If the language for the source block,
                         content)   ;; or the backend for the export block is not specified
                    (setq post-src-export (point))
                    (forward-line))
                   (t
                    )))
                ;; At this point the cursor will be between the #+begin_.. and
                ;; #+end_.. lines.  Now also indent the point forward if needed.
                (when column
                  (indent-to column))

                ;; Now if a region was selected, and `content' was saved from that,
                ;; paste it back in.
                (when content
                  ;; A special case for verbatim blocks.. need to escape "*" and "#+"
                  ;; with commas -- (org) Literal examples.
                  ;; http://lists.gnu.org/archive/html/emacs-orgmode/2017-10/msg00349.html
                  (when (save-excursion
                          (previous-line)
                          (forward-line 0)
                          (let* ((case-fold-search t)) ;Ignore case
                            (looking-at-p (concat "^[[:blank:]]*#\\+BEGIN_"
                                                  (regexp-opt '("EXAMPLE" "EXPORT" "SRC"))))))
                    (setq content (org-escape-code-in-string content)))
                  (insert content)
                  (deactivate-mark)
                  (when post-src-export ;Case where user needs to specify the #+begin_src language,
                    (goto-char post-src-export))))) ;or the #+begin_export backend.

            (defun my/org-template-maybe ()
              "Insert org-template if point is at the beginning of the
line, or if a region is selected.  Else call
`self-insert-command'."
              (interactive)
              (let ((is-region? (use-region-p)))
                (if (or is-region?
                        (and (not is-region?)
                             (looking-back "^[[:blank:]]*")))
                    (hydra-org-template/body)
                  (self-insert-command 1))))

            ;; Fix shift problem in Org mode
            (defadvice org-call-for-shift-select (before org-call-for-shift-select-cua activate)
              (if (and cua-mode
                       org-support-shift-select
                       (not (use-region-p)))
                  (cua-set-mark)))

            ;; Enable mouse in Org
            (use-package org-mouse)

            ;; Fonts
            (set-face-attribute 'org-indent nil :inherit '(fixed-pitch org-hide))
            (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
            (set-face-attribute 'org-code  nil :inherit 'fixed-pitch)
            (set-face-attribute 'org-block nil :inherit 'fixed-pitch)

            ;; Make org do not open other frames
            (setq org-link-frame-setup (quote ((vm      . vm-visit-folder-other-frame)
                                               (vm-imap . vm-visit-imap-folder-other-frame)
                                               (gnus    . org-gnus-no-new-news)
                                               (file    . find-file)
                                               (wl      . wl-other-frame))))

            ;; Add missing function
            (defun org-reverse-string (string)
              (apply 'string (reverse (string-to-list string))))

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
                (setcdr (assoc "\\.txt\\'" org-file-apps) "gedit %s")
              (add-to-list 'org-file-apps '("\\.txt\\'" . "gedit %s") t))

            ;; Change .pdf association directly within the alist
            (setcdr (assoc "\\.pdf\\'" org-file-apps) "acroread %s")

            ;; Org Export
            (use-package ox
              :custom ((org-export-coding-system         'utf-8)
                       (org-export-time-stamp-file       nil)
                       (org-export-headline-levels       4)
                       (org-export-with-sub-superscripts '{})
                       (org-export-allow-bind-keywords   t)
                       (org-export-async-debug           t)
                       (org-export-with-smart-quotes     t))
              :config (progn
                        ;; Default export
                        (use-package ox-org)
                        (use-package ox-extra)

                        ;; Beamer/ODT/Markdown exporters
                        (use-package ox-beamer)
                        (use-package ox-odt)
                        (use-package ox-md)

                        ;; HTML5 slide exporter
                        (use-package ox-html5slide)

                        ;; ASCII doc exporter
                        (use-package ox-asciidoc)

                        ;; Export org-mode to Google I/O HTML5 slides
                        (use-package ox-ioslide-helper)
                        (use-package ox-ioslide)))

            ;; setup Org (babel support)
            (use-package setup-org-babel)

            ;; setup Org (latex support)
            (use-package setup-org-latex)

            ;; setup Org (html support)
            (use-package setup-org-html)

            ;; setup Org Agenda
            (use-package setup-org-agenda)))

;; Setup Org plugins
(use-package setup-org-plugins)

(provide 'setup-org)
;;; setup-org.el ends here
