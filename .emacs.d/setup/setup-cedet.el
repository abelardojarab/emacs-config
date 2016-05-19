;;; setup-cedet.el ---

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

;; Assure .emacs.cache/semanticdb directory exists
(if (not (file-exists-p "~/.emacs.cache/semanticdb"))
    (make-directory "~/.emacs.cache/semanticdb") t)

;; Enable Semantic
(add-to-list 'load-path (expand-file-name "cedet/lisp/cedet/" user-emacs-directory))
(semantic-load-enable-minimum-features)

;; Enable support for parsing additional languages
(require 'semantic/wisent)

;; To use additional features for names completion, and displaying of information for tags & classes,
;; you also need to load the semantic-ia package. This could be performed with following command:
(require 'semantic/ia)

;; semantic support for clang
(if (executable-find "clang")
    (require 'semantic/bovine/clang))

;; Enable case-insensitive searching
(set-default 'semantic-case-fold t)

;; Faster parsing
(setq semantic-idle-work-parse-neighboring-files-flag nil)
(setq semantic-idle-work-update-headers-flag nil)
(setq semantic-idle-scheduler-idle-time 60)
(setq semantic-idle-scheduler-work-idle-time 1800) ;; default is 60
(setq semantic-idle-scheduler-max-buffer-size 1)

;; Disable Semantics for large files
(add-hook 'semantic--before-fetch-tags-hook
          (lambda () (if (and (> (point-max) 500)
                         (not (semantic-parse-tree-needs-rebuild-p)))
                    nil
                  t)))

;; etags support
(when (cedet-ectag-version-check t)
  (semantic-load-enable-primary-ectags-support))

;; Enable semanticdb
(require 'semantic/db)

;; This prevents Emacs to become uresponsive
(defun semanticdb-kill-hook ()
  nil)
(defun semanticdb-create-table-for-file-not-in-buffer (arg)
  nil)

;; Default semanticdb directory
(setq-default semanticdb-default-system-save-directory
              (setq-default semanticdb-default-save-directory "~/.emacs.cache/semanticdb"))

;; semanticdb support for global/gtags
(when (cedet-gnu-global-version-check t)
  (semanticdb-enable-gnu-global-databases 'c-mode t)
  (semanticdb-enable-gnu-global-databases 'c++-mode t))

;; EDE
(global-ede-mode 1)
(ede-enable-generic-projects)

;; Default EDE directory
(setq-default ede-project-placeholder-cache-file "~/.emacs.cache/ede-projects.el")

;; load contrib library
(require 'eassist)

;; Enable support for Qt
(defun qt-cedet-setup ()
  "Set up c-mode and related modes. Includes support for Qt code (signal, slots and alikes)."

  ;; add knowledge of qt to emacs
  (setq qt4-base-dir (concat (getenv "QTDIR") "/include"))
  (semantic-add-system-include (concat qt4-base-dir "/Qt") 'c++-mode)
  (semantic-add-system-include (concat qt4-base-dir "/QtGui") 'c++-mode)
  (semantic-add-system-include (concat qt4-base-dir "/QtCore") 'c++-mode)
  (semantic-add-system-include (concat qt4-base-dir "/QtTest") 'c++-mode)
  (semantic-add-system-include (concat qt4-base-dir "/QtNetwork") 'c++-mode)
  (semantic-add-system-include (concat qt4-base-dir "/QtSvg") 'c++-mode)
  (add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig.h"))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig-large.h"))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qglobal.h"))

  ;; qt keywords and stuff ...
  ;; set up indenting correctly for new qt kewords
  (setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
                                 "\\|protected slot\\|private\\|private slot"
                                 "\\)\\>")
        c-C++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
                                 "\\|public slots\\|protected slots\\|private slots"
                                 "\\)\\>[ \t]*:"))

  ;; modify the colour of slots to match public, private, etc ...
  (font-lock-add-keywords 'c++-mode '(("\\<\\(slots\\|signals\\)\\>" . font-lock-type-face)))
  ;; make new font for rest of qt keywords
  (make-face 'qt-keywords-face)
  (set-face-foreground 'qt-keywords-face "BlueViolet")
  ;; qt keywords
  (font-lock-add-keywords 'c++-mode '(("\\<Q_[A-Z]*\\>" . 'qt-keywords-face)))
  (font-lock-add-keywords 'c++-mode '(("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)))
  (font-lock-add-keywords 'c++-mode '(("\\<Q[A-Z][A-Za-z]*\\>" . 'qt-keywords-face)))
  (font-lock-add-keywords 'c++-mode '(("\\<Q[A-Z_]+\\>" . 'qt-keywords-face)))
  (font-lock-add-keywords 'c++-mode
                          '(("\\<q\\(Debug\\|Wait\\|Printable\\|Max\\|Min\\|Bound\\)\\>" . 'font-lock-builtin-face)))

  (setq c-macro-names-with-semicolon '("Q_OBJECT" "Q_PROPERTY" "Q_DECLARE" "Q_ENUMS"))
  (c-make-macro-with-semi-re))
(when (getenv "QTDIR") (add-hook 'c-mode-common-hook 'qt-cedet-setup))

;; Enable which-function-mode for selected major modes
(setq which-func-modes '(org-mode markdown-mode
                                  ecmascript-mode emacs-lisp-mode lisp-mode java-mode
                                  c-mode c++-mode makefile-mode sh-mode))

;; which-function-mode
(which-func-mode t)
(mapc (lambda (mode)
        (add-hook mode (lambda () (which-function-mode t))))
      '(prog-mode-hook
        org-mode-hook))

(provide 'setup-cedet)
;;; setup-cedet.el ends here
