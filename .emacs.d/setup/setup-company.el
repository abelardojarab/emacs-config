;;; setup-company.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Abelardo Jara-Berrocal

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
(use-package company
  :diminish company-mode
  :load-path (lambda () (expand-file-name "company-mode/" user-emacs-directory))
  :config (progn
            (global-company-mode)

            ;; Use Emacs' built-in TAB completion hooks to trigger AC (Emacs >= 23.2)
            (setq tab-always-indent 'complete)  ;; use 'complete when auto-complete is disabled
            (add-to-list 'completion-styles 'initials t)

            ;; Use Company for completion
            (bind-key [remap completion-at-point] #'company-complete company-mode-map)
            (setq company-backends
                  '((company-files          ;; files & directory
                     company-keywords       ;; keywords
                     company-capf           ;; `completion-at-point-functions'
                     company-yasnippet
                     company-abbrev)
                    (company-dabbrev
                     company-dabbrev-code)))

            ;; Add company-ispell as backend for text-mode's only
            ;; http://blog.binchen.org/posts/emacs-auto-completion-for-non-programmers.html
            (add-hook 'text-mode-hook
                      (lambda ()
                        ;; OPTIONAL, if `company-ispell-dictionary' is nil, `ispell-complete-word-dict' is used
                        ;;  but I prefer hard code the dictionary path. That's more portable.
                        (setq company-ispell-dictionary (expand-file-name "dictionaries/words.txt" user-emacs-directory))

                        ;; make `company-backends' local is critical
                        ;; or else, you will have completion in every major mode, that's very annoying!
                        (set (make-local-variable 'company-backends) '((company-yasnippet
                                                                        company-files
                                                                        company-abbrev)))))

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

            (setq company-idle-delay 0.1
                  company-selection-wrap-around t
                  company-minimum-prefix-length 2
                  company-show-numbers t
                  company-tooltip-align-annotations t
                  company-tooltip-limit 20
                  company-dabbrev-downcase nil
                  company-dabbrev-ignore-case t
                  company-semantic-insert-arguments t
                  company-gtags-insert-arguments t)

            (define-key company-active-map (kbd "C-n") 'company-select-next)
            (define-key company-active-map (kbd "C-p") 'company-select-previous)
            (define-key company-active-map (kbd "TAB") 'company-complete-selection)
            (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
            (define-key company-active-map (kbd "RET") 'company-complete-selection)))

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
  :load-path (lambda () (expand-file-name "company-c-headers/" user-emacs-directory)))

;; Company integration with irony
(use-package company-irony
  :after irony
  :if (executable-find "irony-server")
  :load-path (lambda () (expand-file-name "company-irony/" user-emacs-directory)))

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
  :after (company org)
  :config (setq company-bibtex-bibliography (list "~/workspace/Documents/Bibliography/biblio.bib")))

(provide 'setup-company)
;;; setup-company.el ends here
