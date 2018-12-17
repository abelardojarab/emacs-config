;;; setup-company.el ---                             -*- lexical-binding: t; -*-

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
(use-package company
  :defer t
  :diminish company-mode
  :commands (global-company-mode
             company-mode
             add-pcomplete-to-capf
             company-text-setup
             company-c-setup
             company-minibuffer-setup)
  :custom ((company-begin-commands           '(self-insert-command
                                               org-self-insert-command
                                               c-electric-lt-gt
                                               c-electric-colon
                                               completion-separator-self-insert-command
                                               outshine-self-insert-command))
           (company-transformers              '(company-sort-by-backend-importance))
           (company-frontends                 '(company-pseudo-tooltip-unless-just-one-frontend
                                                company-preview-frontend
                                                company-echo-metadata-frontend))
           (company-idle-delay                0)
           (company-echo-delay                0)
           (company-selection-wrap-around     t)
           (company-minimum-prefix-length     2)
           (company-show-numbers              t)
           (company-tooltip-align-annotations t)
           (company-tooltip-limit             10)
           (company-dabbrev-downcase          nil)
           (company-dabbrev-ignore-case       t)
           (company-semantic-insert-arguments nil)
           (company-gtags-insert-arguments    t)
           (company-lighter-base              ""))
  :bind (("C-;"                              . company-complete-common)
	 :map outline-minor-mode-map
         ("TAB"                              . company-indent-for-tab-command)
         ("<tab>"                            . company-indent-for-tab-command)
         :map company-mode-map
         ([remap completion-at-point]        . company-complete-common-or-cycle)
         ([remap complete-symbol]            . company-complete-common)
         ([remap indent-for-tab-command]     . company-indent-for-tab-command)
         :map company-active-map
         ("C-n"                              . company-select-next)
         ("C-p"                              . company-select-previous)
         ("C-d"                              . company-show-doc-buffer)
         ("TAB"                              . company-complete-selection)
         ("<tab>"                            . company-complete-selection)
         ("RET"                              . company-complete-selection))
  :init (global-company-mode t)
  :hook ((org-mode          . add-pcomplete-to-capf)
         (minibuffer-setup  . company-minibuffer-setup)
         ((c-mode c++-mode) . company-c-setup)
         (text-mode         . company-text-setup))
  :config (progn

            ;; Use Emacs' built-in TAB completion hooks to trigger AC (Emacs >= 23.2)
            (setq tab-always-indent 'complete)
            (add-to-list 'completion-styles 'initials t)

            ;; Enable company in Org mode
            (defun add-pcomplete-to-capf ()
              (add-hook 'completion-at-point-functions #'pcomplete-completions-at-point nil t))

            (defvar completion-at-point-functions-saved nil)
            (defun company-indent-for-tab-command (&optional arg)
              (interactive "P")
              (let ((completion-at-point-functions-saved completion-at-point-functions)
                    (completion-at-point-functions '(company-complete-common-wrapper)))
                (indent-for-tab-command arg)))
            (defun company-complete-common-wrapper ()
              (let ((completion-at-point-functions completion-at-point-functions-saved))
                (company-complete-common)))

            ;; Default company backends
            (setq company-backends
                  '((company-capf
                     company-keywords
                     company-dabbrev-code
             :with company-yasnippet)))

            ;; Ignore errors
            (defadvice company-capf (around bar activate)
              (ignore-errors add-do-it))

            ;; Add yasnippet support for all company backends
            (defvar company-mode/enable-yas t
              "Enable yasnippet for all backends.")
            (defun company-mode/backend-with-yas (backend)
              (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
                  backend
                (append (if (consp backend) backend (list backend))
                        '(:with company-yasnippet))))
            (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

            ;; Add company-ispell as backend for text-mode's only
            (defun company-text-setup ()
              ;; OPTIONAL, if `company-ispell-dictionary' is nil, `ispell-complete-word-dict' is used
              ;;  but I prefer hard code the dictionary path. That's more portable.
              (setq company-ispell-dictionary (expand-file-name "dictionaries/words.txt" user-emacs-directory))

              ;; make `company-backends' local is critical
              ;; or else, you will have completion in every major mode, that's very annoying!
              (set (make-local-variable 'company-backends) '((company-capf
                                                              company-files
                                  :with company-yasnippet))))

	    ;; company-lsp: Company completion backend for lsp-mode.
	    (use-package company-lsp
	      :config (push 'company-lsp company-backends))

            ;; Company integration with irony
            (use-package company-irony
              :if (executable-find "irony-server"))

            ;; Company integration with rtags
            (use-package company-rtags
              :if (executable-find "rdm")
              :load-path (lambda () (expand-file-name "rtags/src/" user-emacs-directory))
              :custom (rtags-completions-enabled t))

            ;; C-mode setup
            (defun company-c-setup ()
              ;; make `company-backends' local is critical
              ;; or else, you will have completion in every major mode, that's very annoying!
              (make-local-variable 'company-backends)
              (setq company-backends (copy-tree company-backends))

              ;; Prefer gtags
              (if (and (executable-find "global")
                       (projectile-project-p)
                       (file-exists-p (concat (projectile-project-root)
                                              "GTAGS")))
                  (setf (car company-backends)
                        (append '(company-gtags)
                                (car company-backends)))

                ;; Fallback to cmake/clang backends
                (when (cmake-ide--locate-cmakelists)
                  ;; Prefer rtags
                  (if (executable-find "rdm")
                      (setf (car company-backends)
                            (append '(company-rtags)
                                    (car company-backends)))
                    ;; Fallback to irony
                    (when (executable-find "irony-server")
                      (setf (car company-backends)
                            (append '(company-irony)
                                    (car company-backends))))))))

            ;; Minibuffer setup
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

            (defun company-minibuffer-setup ()
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
                                                  company-preview-if-just-one-frontend
                                                  company-echo-metadata-frontend))

                  (company-mode 1)
                  (when (eq this-command #'execute-extended-command)
                    (company-complete)))))

            ;; Documentation popups for company
            (use-package company-box
              :disabled t
              :if (display-graphic-p)
              :diminish company-box-mode
              :hook (company-mode . company-box-mode))

            ;; Documentation popups for company
            (use-package company-quickhelp
              :defer t
              :custom ((company-quickhelp-delay                0.2)
                       (company-quickhelp-use-propertized-text t))
              :commands company-quickhelp-mode
              :init (company-quickhelp-mode t)
              :config (setq company-frontends (delq 'company-echo-metadata-frontend company-frontends)))

            ;; Company bibtex integration
            (use-package company-bibtex
              :if (or (executable-find "bibtex")
                      (executable-find "biber"))
              :custom (company-bibtex-bibliography '("~/workspace/Documents/Bibliography/biblio.bib")))

            ;; Company math
            (use-package company-math
              :after math-symbol-lists)

            ;; Company auctex
            (use-package company-auctex
              :defer t
              :after (auctex company-math)
              :commands (company-auctex-init
                         my/latex-mode-init)
              :hook ((TeX-mode   . my/latex-mode-init)
                     (LaTeX-mode . company-auctex-init))
              :init (progn
                        (defun company-auctex-labels (command &optional arg &rest ignored)
                          "company-auctex-labels backend"
                          (interactive (list 'interactive))
                          (case command
                            (interactive (company-begin-backend 'company-auctex-labels))
                            (prefix (company-auctex-prefix "\\\\.*ref{\\([^}]*\\)\\="))
                            (candidates (company-auctex-label-candidates arg))))

                        ;; local configuration for TeX modes
                        (defun my/latex-mode-init ()
                          (setq-local company-backends
                                      (append '(company-auctex-macros
                                                company-auctex-environments
                                                company-math-symbols-unicode
                                                company-math-symbols-latex
                                                company-auctex-labels
                                                company-auctex-bibs
                                                company-bibtex)
                                              company-backends)))))))

(provide 'setup-company)
;;; setup-company.el ends here
