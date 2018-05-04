;;; setup-markdown.el ---                               -*- lexical-binding: t; -*-

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

;; Polymode; syntax highlighting inside markdown
(use-package polymode
  :defer t
  :after org
  :diminish polymode-minor-mode
  :commands polymode-minor-mode
  :load-path (lambda () (expand-file-name "polymode/" user-emacs-directory))
  :init (add-to-list 'load-path (expand-file-name "polymode/modes" user-emacs-directory))
  :config (progn
            (use-package poly-R)
            (use-package poly-markdown)
            (setq pm-weaver   "knitR-ESS" ;; Default weaver
                  pm-exporter "pandoc")))

;; Markdown
(use-package markdown-mode
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :after org
  :commands (markdown-mode
             org-table-align)
  :config (progn

            (defun markdown-table-at-point-p ()
              "Return non-nil when point is inside a table."
              (unless (orgtbl-mode)
                (if (functionp markdown-table-at-point-p-function)
                    (funcall markdown-table-at-point-p-function)
                  (markdown--table-at-point-p))))

            (defconst org-format-transports-properties-p
              (let ((x "a"))
                (add-text-properties 0 1 '(test t) x)
                (get-text-property 0 'test (format "%s" x)))
              "Does format transport text properties?")

            (defun org-table-align ()
              "Align the table at point by aligning all vertical bars."
              (interactive)
              (let* (
                     ;; Limits of table
                     (beg (org-table-begin))
                     (end (org-table-end))
                     ;; Current cursor position
                     (linepos (org-current-line))
                     (colpos (org-table-current-column))
                     (winstart (window-start))
                     (winstartline (org-current-line (min winstart (1- (point-max)))))
                     lines (new "") lengths l typenums ty fields maxfields i
                     column
                     (indent "") cnt frac
                     rfmt hfmt
                     (spaces '(1 . 1))
                     (sp1 (car spaces))
                     (sp2 (cdr spaces))
                     (rfmt1 (concat
                             (make-string sp2 ?\ ) "%%%s%ds" (make-string sp1 ?\ ) "|"))
                     (hfmt1 (concat
                             (make-string sp2 ?-) "%s" (make-string sp1 ?-) "+"))
                     emptystrings links dates emph raise narrow
                     falign falign1 fmax f1 len c e space)
                (untabify beg end)
                (remove-text-properties beg end '(org-cwidth t org-dwidth t display t))
                ;; Check if we have links or dates
                (goto-char beg)
                (setq links (re-search-forward org-bracket-link-regexp end t))
                (goto-char beg)
                (setq emph (and org-hide-emphasis-markers
                                (re-search-forward org-emph-re end t)))
                (goto-char beg)
                (setq raise (and org-use-sub-superscripts
                                 (re-search-forward org-match-substring-regexp end t)))
                (goto-char beg)
                (setq dates (and org-display-custom-times
                                 (re-search-forward org-ts-regexp-both end t)))
                ;; Make sure the link properties are right
                (when links (goto-char beg) (while (org-activate-bracket-links end)))
                ;; Make sure the date properties are right
                (when dates (goto-char beg) (while (org-activate-dates end)))
                (when emph (goto-char beg) (while (org-do-emphasis-faces end)))
                (when raise (goto-char beg) (while (org-raise-scripts end)))

                ;; Check if we are narrowing any columns
                (goto-char beg)
                (setq narrow (and org-table-do-narrow
                                  org-format-transports-properties-p
                                  (re-search-forward "<[lrc]?[0-9]+>" end t)))
                (goto-char beg)
                (setq falign (re-search-forward "<[lrc][0-9]*>" end t))
                (goto-char beg)
                ;; Get the rows
                (setq lines (org-split-string
                             (buffer-substring beg end) "\n"))
                ;; Store the indentation of the first line
                (if (string-match "^ *" (car lines))
                    (setq indent (make-string (- (match-end 0) (match-beginning 0)) ?\ )))
                ;; Mark the hlines by setting the corresponding element to nil
                ;; At the same time, we remove trailing space.
                (setq lines (mapcar (lambda (l)
                                      (if (string-match "^ *|-" l)
                                          nil
                                        (if (string-match "[ \t]+$" l)
                                            (substring l 0 (match-beginning 0))
                                          l)))
                                    lines))
                ;; Get the data fields by splitting the lines.
                (setq fields (mapcar
                              (lambda (l)
                                (org-split-string l " *| *"))
                              (delq nil (copy-sequence lines))))
                ;; How many fields in the longest line?
                (condition-case nil
                    (setq maxfields (apply 'max (mapcar 'length fields)))
                  (error
                   (kill-region beg end)
                   (org-table-create org-table-default-size)
                   (error "Empty table - created default table")))
                ;; A list of empty strings to fill any short rows on output
                (setq emptystrings (make-list maxfields ""))
                ;; Check for special formatting.
                (setq i -1)
                (while (< (setq i (1+ i)) maxfields)   ;; Loop over all columns
                  (setq column (mapcar (lambda (x) (or (nth i x) "")) fields))
                  ;; Check if there is an explicit width specified
                  (setq fmax nil)
                  (when (or narrow falign)
                    (setq c column fmax nil falign1 nil)
                    (while c
                      (setq e (pop c))
                      (when (and (stringp e) (string-match "^<\\([lrc]\\)?\\([0-9]+\\)?>$" e))
                        (if (match-end 1) (setq falign1 (match-string 1 e)))
                        (if (and org-table-do-narrow (match-end 2))
                            (setq fmax (string-to-number (match-string 2 e)) c nil))))
                    ;; Find fields that are wider than fmax, and shorten them
                    (when fmax
                      (loop for xx in column do
                            (when (and (stringp xx)
                                       (> (org-string-width xx) fmax))
                              (org-add-props xx nil
                                'help-echo
                                (concat "Clipped table field, use C-c ` to edit.  Full value is:\n" (org-no-properties (copy-sequence xx))))
                              (setq f1 (min fmax (or (string-match org-bracket-link-regexp xx) fmax)))
                              (unless (> f1 1)
                                (error "Cannot narrow field starting with wide link \"%s\""
                                       (match-string 0 xx)))
                              (add-text-properties f1 (length xx) (list 'org-cwidth t) xx)
                              (add-text-properties (- f1 2) f1
                                                   (list 'display org-narrow-column-arrow)
                                                   xx)))))
                  ;; Get the maximum width for each column
                  (push (apply 'max (or fmax 1) 1 (mapcar 'org-string-width column))
                        lengths)
                  ;; Get the fraction of numbers, to decide about alignment of the column
                  (if falign1
                      (push (equal (downcase falign1) "r") typenums)
                    (setq cnt 0 frac 0.0)
                    (loop for x in column do
                          (if (equal x "")
                              nil
                            (setq frac ( / (+ (* frac cnt)
                                              (if (string-match org-table-number-regexp x) 1 0))
                                           (setq cnt (1+ cnt))))))
                    (push (>= frac org-table-number-fraction) typenums)))
                (setq lengths (nreverse lengths) typenums (nreverse typenums))

                ;; Store the alignment of this table, for later editing of single fields
                (setq org-table-last-alignment typenums
                      org-table-last-column-widths lengths)

                ;; With invisible characters, `format' does not get the field width right
                ;; So we need to make these fields wide by hand.
                (when (or links emph raise)
                  (loop for i from 0 upto (1- maxfields) do
                        (setq len (nth i lengths))
                        (loop for j from 0 upto (1- (length fields)) do
                              (setq c (nthcdr i (car (nthcdr j fields))))
                              (if (and (stringp (car c))
                                       (or (text-property-any 0 (length (car c))
                                                              'invisible 'org-link (car c))
                                           (text-property-any 0 (length (car c))
                                                              'org-dwidth t (car c)))
                                       (< (org-string-width (car c)) len))
                                  (progn
                                    (setq space (make-string (- len (org-string-width (car c))) ?\ ))
                                    (setcar c (if (nth i typenums)
                                                  (concat space (car c))
                                                (concat (car c) space))))))))

                ;; Compute the formats needed for output of the table
                (setq rfmt (concat indent "|") hfmt (concat indent "|"))
                (while (setq l (pop lengths))
                  (setq ty (if (pop typenums) "" "-")) ; number types flushright
                  (setq rfmt (concat rfmt (format rfmt1 ty l))
                        hfmt (concat hfmt (format hfmt1 (make-string l ?-)))))
                (setq rfmt (concat rfmt "\n")
                      hfmt (concat (substring hfmt 0 -1) "|\n"))

                (setq new (mapconcat
                           (lambda (l)
                             (if l (apply 'format rfmt
                                          (append (pop fields) emptystrings))
                               hfmt))
                           lines ""))
                (move-marker org-table-aligned-begin-marker (point))
                (insert new)

                ;; Replace the old one (buggy)
                ;; (delete-region (point) end)

                (move-marker end nil)
                (move-marker org-table-aligned-end-marker (point))
                (when (and orgtbl-mode (not (derived-mode-p 'org-mode)))
                  (goto-char org-table-aligned-begin-marker)
                  (while (org-hide-wide-columns org-table-aligned-end-marker)))
                ;; Try to move to the old location
                (org-goto-line winstartline)
                (setq winstart (point-at-bol))
                (org-goto-line linepos)
                (set-window-start (selected-window) winstart 'noforce)
                (org-table-goto-column colpos)
                (and org-table-overlay-coordinates (org-table-overlay-coordinates))
                (setq org-table-may-need-update nil)))

            (setq markdown-asymmetric-header         t
                  markdown-header-scaling            t
                  markdown-enable-wiki-links         t
                  markdown-list-indent-width         2
                  markdown-enable-wiki-links         t
                  markdown-footnote-location         'immediately
                  markdown-wiki-link-fontify-missing t
                  markdown-wiki-link-alias-first     nil
                  markdown-indent-on-enter           'indent-and-new-item)

            (use-package flycheck-mmark
              :init (add-hook 'flycheck-mode-hook #'flycheck-mmark-setup)
              :commands (flycheck-mmark-setup))

            (defun my/markdown-ispell ()
              "Configure `ispell-skip-region-alist' for `markdown-mode'."
              (make-local-variable 'ispell-skip-region-alist)
              (add-to-list 'ispell-skip-region-alist '("~" "~"))
              (add-to-list 'ispell-skip-region-alist '("```" "```")))
            (add-hook 'markdown-mode-hook #'my/markdown-ispell)

            ;; Markdown preferences
            (add-hook 'markdown-mode-hook
                      (lambda ()
                        ;; Do not wrap lines
                        ;; we will use autofill
                        (visual-line-mode      -1)
                        (abbrev-mode           -1)
                        (toggle-truncate-lines t)
                        (setq                  truncate-lines t)

                        ;; Enable polymode minor mode
                        (polymode-minor-mode t)

                        ;; Org goodies; no longer needed
                        (orgtbl-mode         t)
                        (orgstruct-mode      t)
                        (orgstruct++-mode    t)

                        ;; Extra modes
                        (outline-minor-mode  t)
                        (footnote-mode       t)
                        (auto-fill-mode      t)

                        ;; Style/syntax check
                        (writegood-mode      t)
                        (flyspell-mode       t)
                        (if (or (executable-find "proselint")
                                (executable-find "mmark"))
                            (flycheck-mode t))))

            ;; http://stackoverflow.com/questions/14275122/editing-markdown-pipe-tables-in-emacs
            (defun my/cleanup-org-tables ()
              (save-excursion
                (goto-char (point-min))
                (while (search-forward "-+-" nil t) (replace-match "-|-")))
              (save-buffer)
              (set-buffer-modified-p nil))
            ;; no longer needed, markdown includes supports for tables now
            (add-hook 'markdown-mode-hook
                      (lambda () (add-hook 'after-save-hook 'my/cleanup-org-tables nil t)))

            ;; Github markdown style
            (setq markdown-css-paths `(,(expand-file-name "styles/github-pandoc.css" user-emacs-directory)))
            (setq markdown-command
                  (concat "pandoc --smart -c "
                          (concat user-emacs-directory
                                  "/styles/panam-pandoc.css")
                          " --from markdown_github -t html5 --mathjax --highlight-style pygments --standalone"))))

(provide 'setup-markdown)
;;; setup-markdown.el ends here
