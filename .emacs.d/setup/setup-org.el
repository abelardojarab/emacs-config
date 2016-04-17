;;; setup-org.el ---

;; Copyright (C) 2014, 2015, 2016  abelardo.jara-berrocal

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

;; Org mode
(use-package org
  :load-path (lambda () (expand-file-name "org-mode/lisp" user-emacs-directory))
  :bind (("C-c C" . org-capture)
         ("C-c A" . org-agenda)
         ("C-c L" . org-store-link)
         ("C-c I" . org-insert-link-global)
         ("C-c O" . org-open-at-point-global)
         :map org-mode-map
         ("C-TAB" . org-cycle)
         ("C-c k" . org-cut-subtree)
         ("C-c t" . org-show-todo-tree)
         ("C-c r" . org-refile)
         ("C-c v" . org-reveal))
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

            ;; Set up Org default files
            (let ((todo "~/workspace/Documents/Org/diary.org"))
              (when (file-readable-p todo)

                ;; set org directory
                (setq org-directory (expand-file-name "~/workspace/Documents/Org"))
                (setq diary-file (concat org-directory "/diary"))
                (setq org-id-locations-file "~/.emacs.cache/org-id-locations")
                (setq org-default-notes-file (concat org-directory "/notes.org"))
                (setq org-default-refile-file (concat org-directory "/refile.org"))
                (setq org-agenda-diary-file (concat org-directory "/todo.org"))
                (setq org-mobile-file (concat org-directory "/mobileorg.org"))
                (setq org-mobile-directory (concat org-directory "/mobile-org"))
                (setq org-agenda-files (list
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
                  org-image-actual-width '(100)
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
            (setq org-blank-before-new-entry ;; Insert blank line before new heading
                  '((heading . t)))

            ;; Ellipsis
            (unless (equal system-type 'windows-nt)
              (setq org-ellipsis " ⤵"
                    org-columns-ellipses "…"))

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
                  org-deadline-warning-days 7)

            ;; Export options
            (setq org-export-time-stamp-file nil)
            (setq org-export-with-smart-quotes t) ;; curly quotes in HTML
            (setq org-export-with-sub-superscripts '{})
            (setq org-export-allow-bind-keywords t)
            (setq org-export-async-debug t)

            ;; Stop Org splitting window
            (setq org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame)
                                               (vm-imap . vm-visit-imap-folder-other-frame)
                                               (gnus . org-gnus-no-new-news)
                                               (file . find-file)
                                               (wl . wl-other-frame))))

            ;; Mouse in Org
            (require 'org-mouse)

            ;; Show +/-'s as bullets
            (font-lock-add-keywords 'org-mode
                                    '(("^ +\\([-*]\\) "
                                       (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

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
            (add-to-list 'org-structure-template-alist '("E" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC\n"))
            (add-to-list 'org-structure-template-alist '("S" "#+BEGIN_SRC shell-script\n?\n#+END_SRC\n"))
            (add-to-list 'org-structure-template-alist '("al" "#+BEGIN_SRC latex\n\\begin{align*}\n?\\end{align*}\n#+END_SRC"))

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

            ;; Custom commands
            (defun my-agenda-prefix ()
              (format "%s" (my-agenda-indent-string (org-current-level))))

            (defun my-agenda-indent-string (level)
              (if (= level 1)
                  ""
                (let ((str ""))
                  (while (> level 2)
                    (setq level (1- level)
                          str (concat str "──")))
                  (concat str "►"))))

            (setq org-agenda-custom-commands
                  '(("c" "TODOs"
                     ((tags-todo "mytag"
                                 ((org-agenda-prefix-format " %e %(my-agenda-prefix) ")
                                  (org-tags-match-list-sublevels t)))))))

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

            ;; Make org do not open other frames
            (setq org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame)
                                               (vm-imap . vm-visit-imap-folder-other-frame)
                                               (gnus . org-gnus-no-new-news)
                                               (file . find-file)
                                               (wl . wl-other-frame))))

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

            ;; convert csv to org-table considering "12,12"
            (defun org-convert-csv-table (beg end)
              (interactive (list (point) (mark)))
              (replace-regexp "\\(^\\)\\|\\(\".*?\"\\)\\|," (quote (replace-eval-replacement
                                                                    replace-quote (cond ((equal "^" (match-string 1)) "|")
                                                                                        ((equal "," (match-string 0)) "|")
                                                                                        ((match-string 2))) ))  nil  beg end))

            ;; Strike thru headlines for DONE task
            ;; Stolen from http://sachachua.com/blog/2012/12/emacs-strike-through-headlines-for-done-tasks-in-org/
            ;; (setq org-fontify-done-headline t)
            ;; (custom-set-faces
            ;;  '(org-done ((t (:foreground "PaleGreen"
            ;;                              :weight normal
            ;;                              :strike-through t))))
            ;;  '(org-headline-done
            ;;    ((((class color) (min-colors 16) (background dark))
            ;;      (:foreground "LightSalmon" :strike-through t)))))
            ))

;; ASCII doc
(use-package ox-asciidoc
  :load-path (lambda () (expand-file-name "org-asciidoc/" user-emacs-directory)))

;; Table ASCII plot
(use-package orgtbl-ascii-plot
  :load-path (lambda () (expand-file-name "orgtblasciiplot/" user-emacs-directory)))

;; Org Table of Contents
(use-package org-toc
  :load-path (lambda () (expand-file-name "org-toc/" user-emacs-directory))
  :config(add-hook 'org-mode-hook 'org-toc-enable))

;; Nice bulleted lists
(use-package org-autolist
  :load-path (lambda () (expand-file-name "org-autolist/" user-emacs-directory))
  :config (add-hook 'org-mode-hook (lambda () (org-autolist-mode))))

;; Nice bulleted lists
(use-package org-bullets
  :if (and window-system
           (not (equal system-type 'windows-nt)))
  :load-path (lambda () (expand-file-name "org-bullets/" user-emacs-directory))
  :config (progn
            (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
            (setq org-bullets-bullet-list
                  '(
                    "▣"
                    "▪"
                    "•"
                    "◦"
                    ))))

(provide 'setup-org)
;;; setup-org.el ends here
