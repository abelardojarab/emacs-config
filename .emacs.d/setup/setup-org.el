;;; setup-org.el ---                               -*- lexical-binding: t; -*-

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
         ([C-tab]        . org-cycle)
         ("RET"          . org-retun)
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
           (org-use-sub-superscripts        nil)
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
