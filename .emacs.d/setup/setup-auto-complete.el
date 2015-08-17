;;; setup-auto-complete.el ---

;; Copyright (C) 2014, 2015  abelardo.jara-berrocal

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

;; Abbrevs
(setq abbrev-file-name "~/.emacs.cache/abbrev_defs")
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
(add-hook 'kill-emacs-hook
          'write-abbrev-file)

;; Activate template autocompletion
(abbrev-mode t)
(setq save-abbrevs t)
(dolist (hook '(c-common-mode-hook
                python-mode-hook
                js2-mode-hook
                lisp-mode-hook
                emacs-lisp-mode-hook
                org-mode-hook
                text-mode-hook))
  (add-hook hook (lambda () (abbrev-mode 1))))

;; Auto-Complete
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)
(require 'auto-complete)
(ac-config-default)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
(define-key ac-completing-map (kbd "<tab>") 'ac-complete)
(define-key ac-completing-map (kbd "RET") 'ac-complete)
(define-key ac-completing-map  (kbd "ESC") 'ac-stop)
(global-auto-complete-mode t)
(auto-complete-mode 1)
(setq ac-show-menu-immediately-on-auto-complete t)
(setq ac-dwim nil) ;; To get pop-ups with docs even if a word is uniquely completed
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; General settings
(setq
 ac-auto-start 2
 ac-override-local-map nil
 ac-use-menu-map t
 ac-candidate-limit 10
 ac-quick-help-height 30)

;; Make it a bit faster
(setq
 ac-delay 0.5 ;; same as Eclipse
 ac-auto-show-menu 0.5
 ac-quick-help-delay 0.5)

;; this is used for trigger ac actions from org-mode also
(add-to-list 'ac-trigger-commands 'org-self-insert-command)

;; imenu
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
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")
(add-to-list 'ac-dictionary-directories "~/.emacs.cache/ac-dict")
(setq ac-comphist-file  "~/.emacs.cache/ac-comphist.dat")
(ac-config-default)
(setq-default ac-sources '(ac-source-words-in-same-mode-buffers ac-source-imenu))

;; Autocomplete with TAGS
(add-to-list 'load-path "~/.emacs.d/auto-complete-etags")
(require 'auto-complete-etags)
(setq ac-etags-use-document t)

;; Let's have snippets and TAGS in the auto-complete dropdown
(defun ac-common-setup ()
  (setq ac-sources (append ac-sources '(ac-source-yasnippet
                                        ac-source-imenu
                                        ac-source-abbrev
                                        ac-source-etags
                                        ac-source-gtags
                                        ac-source-semantic
                                        ac-source-dictionary
                                        ac-source-words-in-same-mode-buffers))))
(add-hook 'auto-complete-mode-hook 'ac-common-setup)

;; Clang auto-complete
(add-to-list 'load-path "~/.emacs.d/auto-complete-clang")
(require 'auto-complete-clang)

(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang) ac-sources)))

(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
              (split-string
               "
 /usr/local/include
 /usr/include
 /usr/include/c++
")))

;; Latex auto-complete
(add-to-list 'load-path "~/.emacs.d/ac-math")
(require 'ac-math)
(require 'auto-complete-latex)
(add-to-list 'ac-modes 'latex-mode)
(defun ac-latex-mode-setup ()
  (when (and (require 'auto-complete nil t) (require 'auto-complete-config nil t))
    (make-local-variable 'ac-sources)
    (setq ac-sources (append '(ac-source-words-in-same-mode-buffers
                               ac-source-dictionary
                               ac-source-math-unicode
                               ac-source-math-latex) ac-sources))))
(add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)

;; Autocomplete using Aspell
(add-to-list 'load-path "~/.emacs.d/ac-ispell")
(require 'ac-ispell)
(setq ac-ispell-requires 3)
(setq ac-ispell-fuzzy-limit 2)

(eval-after-load "auto-complete"
  '(progn
     (ac-ispell-setup)))

(add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
(add-hook 'mail-mode-hook 'ac-ispell-ac-setup)
(add-hook 'org-mode-hook 'ac-ispell-ac-setup)

;; Enable auto-complete on more modes
(dolist (mode '(magit-log-edit-mode
                log-edit-mode org-mode text-mode haml-mode
                sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                html-mode nxml-mode sh-mode smarty-mode clojure-mode
                lisp-mode textile-mode markdown-mode tuareg-mode
                csharp-mode js2-mode js3-mode css-mode less-css-mode))
  (add-to-list 'ac-modes mode))

;; Autocomplete for Org
(add-to-list 'load-path "~/.emacs.d/log4e")
(add-to-list 'load-path "~/.emacs.d/yaxception")
(add-to-list 'load-path "~/.emacs.d/auto-complete-pcmp")
(add-to-list 'load-path "~/.emacs.d/org-ac")
(require 'log4e)
(require 'yaxception)
(require 'org-ac)
(org-ac/config-default)

;; font face setting
(set-face-background 'ac-candidate-face "darkgray")
(set-face-underline 'ac-candidate-face "gray")
(set-face-background 'ac-selection-face "steelblue")
(defface ac-yasnippet-selection-face
  '((t (:background "coral3" :foreground "white")))
  "Face for the yasnippet selected candidate."
  :group 'auto-complete)
(when (find-font (font-spec :name "Consolas"))
  (set-face-attribute 'ac-candidate-face nil :inherit 'fixed-pitch)
  (set-face-attribute 'ac-selection-face nil :inherit 'fixed-pitch))

;; Tips for auto-complete
(add-to-list 'load-path "~/.emacs.d/pos-tip")
(require 'pos-tip)
(require 'popup-pos-tip)
(defadvice popup-tip
    (around popup-pos-tip-wrapper (string &rest args) activate)
  (if (display-graphic-p)
      (apply 'popup-pos-tip string args)
    ad-do-it))

(provide 'setup-auto-complete)
