;;; setup-company.el ---                             -*- lexical-binding: t; -*-

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
(use-package company
  :defer t
  :diminish company-mode
  :defines (company-dabbrev-ignore-case
            company-dabbrev-downcase)
  :commands (global-company-mode
             company-mode
             add-pcomplete-to-capf
             company-text-setup
             company-c-setup)
  ;; Nicer looking faces
  :custom-face
  (company-tooltip-common
   ((t (:inherit company-tooltip :weight bold :underline nil))))
  (company-tooltip-common-selection
   ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
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
           (company-selection-wrap-around     t)
           (company-minimum-prefix-length     2)
           (company-show-numbers              t)
           (company-tooltip-align-annotations t)
           (company-tooltip-limit             10)
           (company-dabbrev-downcase          nil)
           (company-dabbrev-ignore-case       t)
           (company-dabbrev-code-everywhere   t)
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

            ;; company-lsp: Company completion backend for lsp-mode.
            (use-package company-lsp
              :demand t
              :custom ((company-lsp-async t)
                       (company-lsp-cache-candidates 'auto)))

            ;; Default company backends
            (setq company-backends
                  '((company-lsp
                     company-keywords
                     company-dabbrev-code
                     :with company-yasnippet
                     :with company-capf)))

            ;; Ignore errors
            (defadvice company-capf (around bar activate)
              (ignore-errors add-do-it))

            ;; Add company-ispell as backend for text-mode's only
            (defun company-text-setup ()
              ;; OPTIONAL, if `company-ispell-dictionary' is nil, `ispell-complete-word-dict' is used
              ;;  but I prefer hard code the dictionary path. That's more portable.
              (setq company-ispell-dictionary (expand-file-name "dictionaries/words.txt" user-emacs-directory))

              ;; make `company-backends' local is critical
              ;; or else, you will have completion in every major mode, that's very annoying!
              (set (make-local-variable 'company-backends) '((company-keywords
                                                              :with company-yasnippet
                                                              :with company-capf))))

            ;; Integration of company with TabNine
            (use-package company-tabnine
              :disabled t
              :if (executable-find "TabNine")
              :custom (company-tabnine-max-num-results 5)
              :hook ((lsp-after-open . (lambda ()
                                         (add-to-list 'company-transformers 'company/sort-by-tabnine t)))
                     (kill-emacs . company-tabnine-kill-process))
              :config (progn
                        ;; Need to use a custom path for TabNine
                        (defun company-tabnine--executable-path ()
                          "TabNine")

                        ;; Enable tabnine on default
                        (add-to-list 'company-backends #'company-tabnine)

                        ;; Integrate company-tabnine with lsp-mode
                        (defun company/sort-by-tabnine (candidates)
                          (if (or (functionp company-backend)
                                  (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
                              candidates
                            (let ((candidates-table (make-hash-table :test #'equal))
                                  candidates-lsp
                                  candidates-tabnine)
                              (dolist (candidate candidates)
                                (if (eq (get-text-property 0 'company-backend candidate)
                                        'company-tabnine)
                                    (unless (gethash candidate candidates-table)
                                      (push candidate candidates-tabnine))
                                  (push candidate candidates-lsp)
                                  (puthash candidate t candidates-table)))
                              (setq candidates-lsp (nreverse candidates-lsp))
                              (setq candidates-tabnine (nreverse candidates-tabnine))
                              (nconc (seq-take candidates-tabnine 3)
                                     (seq-take candidates-lsp 6)))))))

            ;; Company integration with irony
            (use-package company-irony
              :if (and (executable-find "irony-server")
                       (not (executable-find "clangd"))))

            ;; Company integration with rtags
            (use-package company-rtags
              :if (and (executable-find "rdm")
                       (not (executable-find "clangd")))
              :load-path (lambda () (expand-file-name "rtags/src/" user-emacs-directory))
              :custom (rtags-completions-enabled t))

            ;; C-mode setup
            (defun company-c-setup ()
              ;; make `company-backends' local is critical
              ;; or else, you will have completion in every major mode, that's very annoying!
              (make-local-variable 'company-backends)
              (setq company-backends (copy-tree company-backends)))

            ;; Documentation popups for company
            (use-package company-box
              ;; Disabled on emacs newer than 27.0
              :if (not (version< emacs-version "27.0"))
              :diminish t
              :functions (my/company-box--make-line
                          my/company-box-icons--elisp
                          company-box--get-color
                          company-box--resolve-colors
                          company-box--add-icon
                          company-box--apply-color
                          company-box--make-line
                          company-box-icons--elisp)
              :defines company-box-icons-all-the-icons
              :hook (company-mode . company-box-mode)
              :init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
              :custom ((company-box-backends-colors       nil)
                       (company-box-show-single-candidate t)
                       (company-box-max-candidates        50)
                       (company-box-doc-delay             0.3))
              :config (progn
                        (defun my/company-box--make-line (candidate)
                          (-let* (((candidate annotation len-c len-a backend) candidate)
                                  (color (company-box--get-color backend))
                                  ((c-color a-color i-color s-color) (company-box--resolve-colors color))
                                  (icon-string (and company-box--with-icons-p (company-box--add-icon candidate)))
                                  (candidate-string (concat (propertize (or company-common "") 'face 'company-tooltip-common)
                                                            (substring (propertize candidate 'face 'company-box-candidate) (length company-common) nil)))
                                  (align-string (when annotation
                                                  (concat " " (and company-tooltip-align-annotations
                                                                   (propertize " " 'display `(space :align-to (- right-fringe ,(or len-a 0) 1)))))))
                                  (space company-box--space)
                                  (icon-p company-box-enable-icon)
                                  (annotation-string (and annotation (propertize annotation 'face 'company-box-annotation)))
                                  (line (concat (unless (or (and (= space 2) icon-p) (= space 0))
                                                  (propertize " " 'display `(space :width ,(if (or (= space 1) (not icon-p)) 1 0.75))))
                                                (company-box--apply-color icon-string i-color)
                                                (company-box--apply-color candidate-string c-color)
                                                align-string
                                                (company-box--apply-color annotation-string a-color)))
                                  (len (length line)))
                            (add-text-properties 0 len (list 'company-box--len (+ len-c len-a)
                                                             'company-box--color s-color)
                                                 line)
                            line))
                        (advice-add #'company-box--make-line :override #'my/company-box--make-line)

                        ;; Prettify icons
                        (defun my/company-box-icons--elisp (candidate)
                          (when (derived-mode-p 'emacs-lisp-mode)
                            (let ((sym (intern candidate)))
                              (cond ((fboundp sym) 'Function)
                                    ((featurep sym) 'Module)
                                    ((facep sym) 'Color)
                                    ((boundp sym) 'Variable)
                                    ((symbolp sym) 'Text)
                                    (t . nil)))))
                        (advice-add #'company-box-icons--elisp :override #'my/company-box-icons--elisp)

                        (when (display-graphic-p)
                          (with-eval-after-load 'all-the-icons
                            (declare-function all-the-icons-faicon 'all-the-icons)
                            (declare-function all-the-icons-material 'all-the-icons)
                            (declare-function all-the-icons-octicon 'all-the-icons)
                            (setq company-box-icons-all-the-icons
                                  `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.85 :v-adjust -0.2))
                                    (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.05))
                                    (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
                                    (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
                                    (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
                                    (Field . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
                                    (Variable . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
                                    (Class . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
                                    (Interface . ,(all-the-icons-material "share" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
                                    (Module . ,(all-the-icons-material "view_module" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
                                    (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
                                    (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.85 :v-adjust -0.2))
                                    (Value . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
                                    (Enum . ,(all-the-icons-material "storage" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
                                    (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.85 :v-adjust -0.2))
                                    (Snippet . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2))
                                    (Color . ,(all-the-icons-material "palette" :height 0.85 :v-adjust -0.2))
                                    (File . ,(all-the-icons-faicon "file-o" :height 0.85 :v-adjust -0.05))
                                    (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.85 :v-adjust -0.2))
                                    (Folder . ,(all-the-icons-faicon "folder-open" :height 0.85 :v-adjust -0.05))
                                    (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
                                    (Constant . ,(all-the-icons-faicon "square-o" :height 0.85 :v-adjust -0.05))
                                    (Struct . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
                                    (Event . ,(all-the-icons-faicon "bolt" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-orange))
                                    (Operator . ,(all-the-icons-material "control_point" :height 0.85 :v-adjust -0.2))
                                    (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.05))
                                    (Template . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2)))
                                  company-box-icons-alist 'company-box-icons-all-the-icons)))))

            ;; Documentation popups for company
            (use-package company-quickhelp
              :disabled t
              :defines company-quickhelp-delay
              :bind (:map company-active-map
                          ([remap company-show-doc-buffer] . company-quickhelp-manual-begin))
              :custom ((company-quickhelp-delay                0.2)
                       (company-quickhelp-use-propertized-text t))
              :commands company-quickhelp-mode
              :hook (global-company-mode . company-quickhelp-mode)
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
