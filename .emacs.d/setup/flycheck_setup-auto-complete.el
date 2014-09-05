;;; setup-auto-complete.el ---

;; Copyright (C) 2014  abelardo.jara-berrocal

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
(setq default-abbrev-mode t
      save-abbrevs t)

;; Auto-Complete
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)
(require 'auto-complete)
(ac-config-default)
(setq-default ac-sources '(ac-source-semantic-raw))
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")
(define-key ac-completing-map (kbd "<tab>") 'ac-complete)
(global-auto-complete-mode t)
(auto-complete-mode 1)
(setq ac-show-menu-immediately-on-auto-complete t)
(setq ac-dwim nil) ; To get pop-ups with docs even if a word is uniquely completed
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; General settings
(setq
 ac-auto-start 2
 ac-override-local-map nil
 ac-use-menu-map t
 ac-candidate-limit 30
 ac-quick-help-height 30)

;; Make it a bit faster
(setq
 ac-delay 0.5 ;; same as Eclipse
 ac-auto-show-menu 0.5
 ac-quick-help-delay 0.5)

;; this is used for trigger ac actions from org-mode also
(add-to-list 'ac-trigger-commands 'org-self-insert-command)

;; create and add new words to the dictionary on the fly
(when (require 'auto-complete-config nil 'noerror)
  (add-to-list 'ac-dictionary-directories "~/.emacs.cache/ac-dict")
  (setq ac-comphist-file  "~/.emacs.cache/ac-comphist.dat")
  (ac-config-default))

;; Autocomplete with TAGS
(add-to-list 'load-path "~/.emacs.d/auto-complete-etags")
(require 'auto-complete-etags)
(setq ac-etags-use-document t)

;; Let's have snippets and TAGS in the auto-complete dropdown
(defun ac-common-setup ()
  (setq ac-sources (append ac-sources '(ac-source-yasnippet ac-source-etags ac-source-gtags ac-source-semantic ac-source-semantic-raw ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))))
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
 /usr/lib/gcc/x86_64-redhat-linux/4.8.3/include
 /usr/local/include
 /usr/include
 /usr/include/gio-unix-2.0/
 /usr/lib64/glib-2.0/include
/usr/include/glib-2.0
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

;; Enable auto-complete on more modes
(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
                                    sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                                    html-mode nxml-mode sh-mode smarty-mode clojure-mode
                                    lisp-mode textile-mode markdown-mode tuareg-mode
                                    js3-mode css-mode less-css-mode))
  (add-to-list 'ac-modes mode))

(provide 'setup-auto-complete)
