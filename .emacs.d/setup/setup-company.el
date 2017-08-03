;;; setup-company.el ---                             -*- lexical-binding: t; -*-

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
(use-package company
  :diminish company-mode
  :load-path (lambda () (expand-file-name "company-mode/" user-emacs-directory))
  :commands global-company-mode
  :bind (("C-;"             . company-complete-common)
         :map company-mode-map
         ([remap completion-at-point] . company-complete-common)
         ([remap complete-symbol]     . company-complete-common)
         :map company-active-map
         ("C-n"                       . company-select-next)
         ("C-p"                       . company-select-previous)
         ("C-d"                       . company-show-doc-buffer)
         ("TAB"                       . company-complete-common-or-cycle)
         ("<tab>"                     . company-complete-common-or-cycle)
         ("RET"                       . company-complete-selection))
  :init (progn
          ;; set default lighter as nothing so in general it is not displayed
          ;; but will still be shown when completion popup is active to show the
          ;; backend which is in use
          (setq company-lighter-base "")
          (global-company-mode 1))
  :config (progn

            ;; Use Emacs' built-in TAB completion hooks to trigger AC (Emacs >= 23.2)
            (setq tab-always-indent 'complete)
            (add-to-list 'completion-styles 'initials t)

            ;; Enable company in Org mode
            (defun add-pcomplete-to-capf ()
              (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
            (add-hook 'org-mode-hook #'add-pcomplete-to-capf)

            ;; Default company backends
            (setq company-backends
                  '((company-capf           ;; `completion-at-point-functions'
                     company-yasnippet
                     company-abbrev
                     company-files          ;; files & directory
                     company-keywords)
                    (company-dabbrev
                     company-dabbrev-code)))

            ;; Add yasnippet support for all company backends
            ;; https://github.com/syl20bnr/spacemacs/pull/179
            (defvar company-mode/enable-yas t
              "Enable yasnippet for all backends.")
            (defun company-mode/backend-with-yas (backend)
              (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
                  backend
                (append (if (consp backend) backend (list backend))
                        '(:with company-yasnippet))))
            (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

            ;; Add company-ispell as backend for text-mode's only
            ;; http://blog.binchen.org/posts/emacs-auto-completion-for-non-programmers.html
            (add-hook 'text-mode-hook
                      (lambda ()
                        ;; OPTIONAL, if `company-ispell-dictionary' is nil, `ispell-complete-word-dict' is used
                        ;;  but I prefer hard code the dictionary path. That's more portable.
                        (setq company-ispell-dictionary (expand-file-name "dictionaries/words.txt" user-emacs-directory))

                        ;; make `company-backends' local is critical
                        ;; or else, you will have completion in every major mode, that's very annoying!
                        (set (make-local-variable 'company-backends) '((company-capf
                                                                        company-yasnippet
                                                                        company-abbrev
                                                                        company-files)))))

            ;; C-mode setup
            (add-hook 'c-mode-common-hook
                      (lambda ()
                        ;; make `company-backends' local is critical
                        ;; or else, you will have completion in every major mode, that's very annoying!
                        (make-local-variable 'company-backends)
                        (setq company-backends (copy-tree company-backends))

                        ;; company-semantic configuration
                        (if (executable-find "gcc")
                            (setf (car company-backends)
                                  (append '(company-semantic)
                                          (car company-backends))))

                        ;; company-gtags configuration
                        (if (executable-find "global")
                            (setf (car company-backends)
                                  (append '(company-gtags)
                                          (car company-backends))))

                        ;; company-c-headers configuration
                        (if (executable-find "clang")
                            (setf (car company-backends)
                                  (append '(company-c-headers)
                                          (car company-backends))))

                        ;; irony and rtags configuration
                        (if (cmake-ide--locate-cmakelists)
                            (progn
                              (if (executable-find "rdm")
                                  (setf (car company-backends)
                                        (append '(company-rtags)
                                                (car company-backends))))

                              (if (executable-find "irony-server")
                                  (setf (car company-backends)
                                        (append '(company-irony)
                                                (car company-backends))))))))

            (setq company-begin-commands '(self-insert-command
                                           org-self-insert-command
                                           c-electric-lt-gt
                                           c-electric-colon
                                           completion-separator-self-insert-command)
                  company-transformers '(company-sort-by-occurrence
                                         company-sort-by-backend-importance)
                  company-idle-delay 0
                  company-echo-delay                0
                  company-selection-wrap-around     t
                  company-minimum-prefix-length     2
                  company-show-numbers              t
                  company-tooltip-align-annotations t
                  company-tooltip-limit             10
                  company-dabbrev-downcase          nil
                  company-dabbrev-ignore-case       t
                  company-semantic-insert-arguments t
                  company-gtags-insert-arguments    t)

            ;; Enable company in minibufer
            ;; https://gist.github.com/Bad-ptr/7787596
            (defun company-elisp-minibuffer (command &optional arg &rest ignored)
              "`company-mode' completion back-end for Emacs Lisp in the minibuffer."
              (interactive (list 'interactive))
              (case command
                ('prefix (and (minibufferp)
                              (case company-minibuffer-mode
                                ('execute-extended-command (company-grab-symbol))
                                (t (company-capf `prefix)))))
                ('candidates
                 (case company-minibuffer-mode
                   ('execute-extended-command (all-completions arg obarray 'commandp))
                   (t nil)))))

            (defun minibuffer-company ()
              (unless company-mode
                (when (and global-company-mode (or (eq this-command #'execute-extended-command)
                                                   (eq this-command #'eval-expression)))

                  (setq-local company-minibuffer-mode this-command)
                  (setq-local completion-at-point-functions
                              (list (if (fboundp 'elisp-completion-at-point)
                                        #'elisp-completion-at-point
                                      #'lisp-completion-at-point) t))

                  (setq-local company-show-numbers nil)
                  (setq-local company-backends '((company-elisp-minibuffer company-capf)))
                  (setq-local company-tooltip-limit 8)
                  (setq-local company-col-offset 1)
                  (setq-local company-row-offset 1)
                  (setq-local company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                                                  company-preview-if-just-one-frontend))

                  (company-mode 1)
                  (when (eq this-command #'execute-extended-command)
                    (company-complete)))))

            (add-hook 'minibuffer-setup-hook #'minibuffer-company)))

;; Documentation popups for Company
(use-package company-quickhelp
  :after company
  :load-path (lambda () (expand-file-name "company-quickhelp/" user-emacs-directory))
  :if (display-graphic-p)
  :config (progn
            (setq company-quickhelp-delay 0.2)
            (add-hook 'global-company-mode-hook #'company-quickhelp-mode)

            ;; Update front-end tooltip
            (setq company-frontends (delq 'company-echo-metadata-frontend company-frontends))))

;; Company C-headers
(use-package company-c-headers
  :after company
  :if (executable-find "clang")
  :load-path (lambda () (expand-file-name "company-c-headers/" user-emacs-directory))
  :config (progn
            (defun my/ede-object-system-include-path ()
              "Return the system include path for the current buffer."
              (when (and ede-object
                         (strinp (ede-system-include-path)))
                (ede-system-include-path ede-object)))

            (setq company-c-headers-path-system
                  #'my/ede-object-system-include-path)))

;; Company integration with irony
(use-package company-irony
  :after (company irony)
  :if (executable-find "irony-server")
  :load-path (lambda () (expand-file-name "company-irony/" user-emacs-directory)))

;; Irony C/C++ headers
(use-package company-irony-c-headers
  :after (company irony company-irony)
  :if (executable-find "clang++")
  :load-path (lambda () (expand-file-name "company-irony-c-headers/" user-emacs-directory))
  :config (progn
            (setq company-irony-c-headers--compiler-executable (executable-find "clang++"))

            ;; group with company-irony but beforehand so we get first pick
            (add-to-list 'company-backends '(company-irony-c-headers company-irony))))

;; Company integration with rtags
(use-package company-rtags
  :if (executable-find "rdm")
  :load-path (lambda () (expand-file-name "rtags/src/" user-emacs-directory))
  :after (company rtags)
  :config (setq rtags-completions-enabled t))

;; Company bibtex integration
(use-package company-bibtex
  :if (or (executable-find "bibtex")
          (executable-find "biber"))
  :load-path (lambda () (expand-file-name "company-bibtex/" user-emacs-directory))
  :after company
  :config (setq company-bibtex-bibliography (list "~/workspace/Documents/Bibliography/biblio.bib")))

;; Company math
(use-package company-math
  :load-path (lambda () (expand-file-name "company-math/" user-emacs-directory))
  :after (company math-symbol-lists))

;; Company auctex
(use-package company-auctex
  :load-path (lambda () (expand-file-name "company-auctex/" user-emacs-directory))
  :after (company auctex company-math)
  :config (progn
            (defun company-auctex-labels (command &optional arg &rest ignored)
              "company-auctex-labels backend"
              (interactive (list 'interactive))
              (case command
                (interactive (company-begin-backend 'company-auctex-labels))
                (prefix (company-auctex-prefix "\\\\.*ref{\\([^}]*\\)\\="))
                (candidates (company-auctex-label-candidates arg))))

            ;; local configuration for TeX modes
            (defun my/latex-mode-setup ()
              (setq-local company-backends
                          (append '(company-auctex-macros
                                    company-auctex-environments
                                    company-math-symbols-unicode
                                    company-math-symbols-latex
                                    company-auctex-labels
                                    company-auctex-bibs
                                    company-bibtex)
                                  company-backends)))

            (add-hook 'TeX-mode-hook 'my/latex-mode-setup)
            (add-hook 'LaTeX-mode-hook #'company-auctex-init)))

(provide 'setup-company)
;;; setup-company.el ends here
