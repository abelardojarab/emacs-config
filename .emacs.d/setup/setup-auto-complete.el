;;; setup-auto-complete.el ---

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

;; Auto-Complete
(use-package auto-complete
  :load-path (lambda () (expand-file-name "auto-complete/" user-emacs-directory))
  :diminish auto-complete-mode
  :init (require 'auto-complete-config)
  :config (progn

            ;; Use Emacs' built-in TAB completion hooks to trigger AC (Emacs >= 23.2)
            (setq tab-always-indent 'complete)  ;; use 'complete when auto-complete is disabled
            (add-to-list 'completion-styles 'initials t)

            ;; hook AC into completion-at-point
            (defun set-auto-complete-as-completion-at-point-function ()
              (setq completion-at-point-functions '(auto-complete)))
            (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

            ;; ac-complete configuration
            (ac-config-default)
            (ac-set-trigger-key "TAB")
            (ac-set-trigger-key "<tab>")
            (define-key ac-completing-map (kbd "<tab>") 'ac-complete)
            (define-key ac-completing-map (kbd "RET") 'ac-complete)
            (define-key ac-completing-map  (kbd "ESC") 'ac-stop)
            (global-auto-complete-mode t)
            (auto-complete-mode 1)


            ;; Set font face setting
            (set-face-attribute 'ac-candidate-face nil :inherit 'fixed-pitch :bold nil)
            (set-face-attribute 'ac-selection-face nil :inherit 'fixed-pitch :bold nil)

            ;; General settings
            (setq
             ac-show-menu-immediately-on-auto-complete t
             ac-dwim nil ;; get popups with docs even if word is uniquely completed
             ac-auto-start 2
             ac-override-local-map nil
             ac-use-menu-map t
             ac-use-fuzzy nil
             ac-candidate-limit 10
             ac-quick-help-height 30)

            ;; Make it a bit faster
            (setq
             ac-delay 0.5 ;; same as Eclipse
             ac-auto-show-menu 0.5
             ac-quick-help-delay 0.5)

            ;; this is used for trigger ac actions from org-mode also
            (add-to-list 'ac-trigger-commands 'org-self-insert-command)

            ;; disable auto-complete in comments
            (setq ac-disable-faces '(font-lock-string-face font-lock-doc-face))

            ;; Default colors
            (set-face-background 'ac-selection-face "darkgray")

            ;; support for imenu
            (defvar ac-imenu-index nil)
            (ac-clear-variable-every-10-minutes 'ac-imenu-index)
            (defun ac-imenu-candidates ()
              (loop with i = 0
                    with stack = (progn
                                   (unless (local-variable-p 'ac-imenu-index)
                                     (make-local-variable 'ac-imenu-index))
                                   (or ac-imenu-index
                                       (setq ac-imenu-index (ignore-errors (imenu--make-index-alist)))))
                    with result
                    while (and stack (or (not (integerp ac-limit))
                                         (< i ac-limit)))
                    for node = (pop stack)
                    if (consp node)
                    do
                    (let ((car (car node))
                          (cdr (cdr node)))
                      (if (consp cdr)
                          (mapc (lambda (child)
                                  (push child stack))
                                cdr)
                        (when (and (stringp car)
                                   (string-match (concat "^" (regexp-quote ac-prefix)) car))
                          ;; Remove extra characters
                          (if (string-match "^.*\\(()\\|=\\|<>\\)$" car)
                              (setq car (substring car 0 (match-beginning 1))))
                          (push car result)
                          (incf i))))
                    finally return (nreverse result)))
            (ac-define-source imenu
              '((depends imenu)
                (candidates . ac-imenu-candidates)
                (symbol . "s")))

            ;; create and add new words to the dictionary on the fly
            (require 'auto-complete-config)
            (add-to-list 'ac-dictionary-directories (expand-file-name "auto-complete/dict" user-emacs-directory))
            (add-to-list 'ac-dictionary-directories "~/.emacs.cache/ac-dict")
            (setq ac-comphist-file  "~/.emacs.cache/ac-comphist.dat")
            (ac-config-default)
            (setq-default ac-sources '(ac-source-yasnippet
                                       ac-source-imenu
                                       ac-source-abbrev
                                       ac-source-words-in-same-mode-buffers))

            ;; Enable auto-complete on more modes
            (dolist (mode '(magit-log-edit-mode
                            log-edit-mode org-mode text-mode haml-mode
                            sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                            html-mode nxml-mode sh-mode smarty-mode clojure-mode
                            lisp-mode textile-mode markdown-mode tuareg-mode
                            csharp-mode js2-mode js3-mode css-mode less-css-mode
                            vhdl-mode verilog-mode))
              (add-to-list 'ac-modes mode))

            ;; Let's have snippets and TAGS in the auto-complete dropdown
            (defun ac-common-setup ()
              (setq ac-sources (append ac-sources '(ac-source-yasnippet
                                                    ac-source-imenu
                                                    ac-source-abbrev
                                                    ac-source-words-in-same-mode-buffers))))
            (add-hook 'auto-complete-mode-hook 'ac-common-setup)))

;; Clang auto-complete
(use-package auto-complete-clang
  :if (executable-find "clang")
  :load-path (lambda () (expand-file-name "auto-complete-clang/" user-emacs-directory))
  :config (progn
            (defun my-ac-cc-mode-setup ()
              (setq ac-sources (append '(ac-source-clang) ac-sources)))

            (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
            (setq ac-clang-flags
                  (mapcar (lambda (item) (concat "-I" item))
                          (split-string
                           "/usr/local/include
 /usr/include
 /usr/include/c++")))))

;; C-headers
(use-package auto-complete-c-headers
  :load-path (lambda () (expand-file-name "auto-complete-c-headers/" user-emacs-directory))
  :config (progn
            (add-hook 'c++-mode-hook (lambda ()
                                       '(setq ac-sources (append ac-sources '(ac-source-c-headers)))))
            (add-hook 'c-mode-hook (lambda ()
                                     '(setq ac-sources (append ac-sources '(ac-source-c-headers)))))))

;; Irony auto-complete
(use-package ac-irony
  :if (executable-find "irony-server")
  :load-path (lambda () (expand-file-name "ac-irony/" user-emacs-directory))
  :config (progn
            (defun my-ac-irony-setup ()
              (add-to-list 'ac-sources 'ac-source-irony))
            (add-hook 'irony-mode-hook 'my-ac-irony-setup)))

;; Latex math auto-complete
(use-package ac-math
  :load-path (lambda () (expand-file-name "ac-math/" user-emacs-directory)))

;; Latex auto-complete
(use-package auto-complete-latex
  :config (progn
            (add-to-list 'ac-modes 'latex-mode)
            (defun ac-latex-mode-setup ()
              (when (and (require 'auto-complete nil t) (require 'auto-complete-config nil t))
                (make-local-variable 'ac-sources)
                (setq ac-sources (append '(ac-source-math-unicode
                                           ac-source-math-latex) ac-sources))))
            (ac-flyspell-workaround)
            (add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)
            (add-hook 'org-mode-hook 'ac-latex-mode-setup)))

;; Autocomplete for Org
(use-package auto-complete-pcmp
  :load-path (lambda () (expand-file-name "auto-complete-pcmp/" user-emacs-directory)))

(use-package org-ac
  :load-path (lambda () (expand-file-name "org-ac/" user-emacs-directory))
  :config (org-ac/config-default))

;; ispell auto-complete for Org
(use-package ac-ispell
  :load-path (lambda () (expand-file-name "ac-ispell/" user-emacs-directory))
  :if (executable-find "aspell")
  :init (progn
          (setq ac-ispell-requires 4
                ac-ispell-fuzzy-limit 0))
  :config (progn
            (ac-ispell-setup)
            (add-hook 'markdown-mode-hook 'ac-ispell-ac-setup)
            (add-hook 'org-mode-hook 'ac-ispell-ac-setup)
            (add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
            (add-hook 'mail-mode-hook 'ac-ispell-ac-setup)))

;; Tips for auto-complete
(use-package popup-pos-tip
  :config (progn
            (defadvice popup-tip
                (around popup-pos-tip-wrapper (string &rest args) activate)
              (if (display-graphic-p)
                  (apply 'popup-pos-tip string args)
                ad-do-it))

            (defun popup-documentation-at-point ()
              (interactive)
              (let* ((position (point))
                     (string-under-cursor (buffer-substring-no-properties
                                           (progn (skip-syntax-backward "w_") (point))
                                           (progn (skip-syntax-forward "w_") (point)))))
                (goto-char position)
                (popup-tip (ac-symbol-documentation (intern string-under-cursor)))))))

(provide 'setup-auto-complete)
