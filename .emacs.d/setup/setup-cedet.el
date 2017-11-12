;;; setup-cedet.el ---

;; Copyright (C) 2014, 2015, 2016, 2017  Abelardo Jara-Berrocal

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

(use-package semantic
  :config (progn

            ;; To use additional features for names completion, and displaying of information for tags & classes,
            ;; you also need to load the semantic-ia package. Unfortunately, semantic makes Emacs slow
            (use-package semantic/ia)

            ;; Enable support for parsing additional languages
            (use-package semantic/wisent)

            ;; Enabled features
            (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                              global-semanticdb-minor-mode
                                              global-semantic-idle-summary-mode
                                              global-semantic-mru-bookmark-mode
                                              global-semantic-load-enable-code-helpers
                                              global-semantic-load-enable-excessive-code-helpers
                                              global-semantic-idle-completions-mode))

            ;; Assure .emacs.cache/semanticdb directory exists
            (if (not (file-exists-p "~/.emacs.cache/semanticdb"))
                (make-directory "~/.emacs.cache/semanticdb") t)

            ;; Enable case-insensitive searching
            (set-default 'semantic-case-fold t)

            ;; Faster parsing
            (setq semantic-idle-work-parse-neighboring-files-flag nil
                  semantic-idle-work-update-headers-flag      nil
                  semantic-idle-scheduler-idle-time               432000
                  semantic-idle-scheduler-work-idle-time      1800 ;; default is 60
                  semantic-idle-scheduler-max-buffer-size     1)

            ;; Disable Semantics for large files
            (add-hook 'semantic--before-fetch-tags-hook
                      (lambda () (if (and (> (point-max) 500)
                                     (not (semantic-parse-tree-needs-rebuild-p)))
                                nil
                              t)))

            ;; Enable decoration mode
            (global-semantic-decoration-mode t)

            ;; for semantic-ia-fast-jump
            (use-package semantic/analyze/refs)

            ;; Fixing a bug in semantic, see #22287
            (defun semanticdb-save-all-db-idle ()
              "Save all semantic tag databases from idle time.
Exit the save between databases if there is user input."

              ;; save-mark-and-excursion is defined in Emacs 25.1-forward
              (if (fboundp 'save-mark-and-excursion)
                  (semantic-safe "Auto-DB Save: %S"
                    ;; FIXME: Use `while-no-input'?
                    (save-mark-and-excursion ;; <-- added line
                     (semantic-exit-on-input 'semanticdb-idle-save
                       (mapc (lambda (db)
                               (semantic-throw-on-input 'semanticdb-idle-save)
                               (semanticdb-save-db db t))
                             semanticdb-database-list))))
                (if (fboundp 'save-excursion)
                    (save-excursion ;; <-- added line
                      (semantic-exit-on-input 'semanticdb-idle-save
                        (mapc (lambda (db)
                                (semantic-throw-on-input 'semanticdb-idle-save)
                                (semanticdb-save-db db t))
                              semanticdb-database-list))))))

            ;; Disable semanticdb, slows down Emacs
            (global-semanticdb-minor-mode nil)
            (setq semanticdb-search-system-databases t)
            (add-hook 'c-mode-common-hook
                      (lambda ()
                        (setq semanticdb-project-system-databases
                              (list (semanticdb-create-database
                                     semanticdb-new-database-class
                                     "/usr/include")))))

            ;; Set project roots to start from /
            (setq semanticdb-project-roots
                  (list
                   (expand-file-name "/")))

            ;; This prevents Emacs to become uresponsive
            (defun semanticdb-kill-hook ()
              nil)
            (defun semanticdb-create-table-for-file-not-in-buffer (arg)
              nil)

            ;; Ignore errors in semantic completions
            (defadvice semantic-analyze-possible-completions (around bar activate)
              (ignore-errors add-do-it))

            (defadvice semantic-analyze-possible-completions-default (around bar activate)
              (ignore-errors add-do-it))

            ;; Default semanticdb directory
            (setq-default semanticdb-default-save-directory "~/.emacs.cache/semanticdb")

            ;; semanticdb support for global/gtags
            (when (executable-find "global")
              (semanticdb-enable-gnu-global-databases 'c-mode t)
              (semanticdb-enable-gnu-global-databases 'c++-mode t))

            ;; Helper functions to parse source code under directory using Semantic
            (defvar semantic/c-files-regex ".*\\.\\(c\\|cpp\\|h\\|hpp\\)"
              "A regular expression to match any c/c++ related files under a directory")

            (defun semantic/semantic-parse-dir (root regex)
              "This function is an attempt of mine to force semantic to
     parse all source files under a root directory. Arguments:
     -- root: The full path to the root directory
     -- regex: A regular expression against which to match all files in the directory"
              (let (;;make sure that root has a trailing slash and is a dir
                    (root (file-name-as-directory root))
                    (files (directory-files root t )))
                ;; remove current dir and parent dir from list
                (setq files (delete (format "%s." root) files))
                (setq files (delete (format "%s.." root) files))
                (while files
                  (setq file (pop files))
                  (if (not(file-accessible-directory-p file))
                      ;;if it's a file that matches the regex we seek
                      (progn (when (string-match-p regex file)
                               (save-excursion
                                 (semanticdb-file-table-object file))))
                    ;;else if it's a directory
                    (semantic/semantic-parse-dir file regex)))))

            (defun semantic/semantic-parse-current-dir (regex)
              "Parses all files under the current directory matching regex"
              (semantic/semantic-parse-dir (file-name-directory(buffer-file-name)) regex))

            (defun semantic/parse-curdir-c ()
              "Parses all the c/c++ related files under the current directory
     and inputs their data into semantic"
              (interactive)
              (semantic/semantic-parse-current-dir semantic/c-files-regex))

            (defun semantic/parse-dir-c (dir)
              "Prompts the user for a directory and parses all c/c++ related files
     under the directory"
              (interactive (list (read-directory-name "Provide the directory to search in:")))
              (semantic/semantic-parse-dir (expand-file-name dir) semantic/c-files-regex))))

;; Load contrib library
(use-package eassist)

;; EDE project managment, slows down Emacs
(use-package ede
  :disabled t
  :config (progn
            (global-ede-mode 1)
            (ede-enable-generic-projects)
            (setq ede-project-directories t)

            ;; Default EDE directory
            (setq-default ede-project-placeholder-cache-file "~/.emacs.cache/ede-projects.el")

            ;; Redefine ede add projects to avoid errors
            (defun ede-add-project-to-global-list (proj)
              "Add the project PROJ to the master list of projects.
On success, return the added project."
              (ignore-errors
                (when (not proj)
                  (error "No project created to add to master list"))
                (when (not (eieio-object-p proj))
                  (error "Attempt to add non-object to master project list"))
                (when (not (obj-of-class-p proj ede-project-placeholder))
                  (error "Attempt to add a non-project to the ede projects list"))
                (if (stringp proj)
                    (add-to-list 'ede-projects proj)))
              proj)))

;; Show function in mode-line
(use-package which-func
  :defer t
  :commands which-function-mode
  :config (progn
            ;; Enable which-function-mode for selected major modes
            (setq which-func-unknown "⊥"
                  which-func-maxout 1024
                  which-func-modes '(latex-mode
                                     markdown-mode
                                     c-mode
                                     emacs-lisp-mode
                                     org-mode
                                     c++-mode)
                  which-func-format
                  `(" "
                    (:propertize which-func-current local-map
                                 (keymap
                                  (mode-line keymap
                                             (mouse-3 . end-of-defun)
                                             (mouse-2 . narrow-to-defun)
                                             (mouse-1 . beginning-of-defun)))
                                 face which-func
                                 mouse-face mode-line-highlight
                                 help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end")
                    " "))))

(provide 'setup-cedet)
;;; setup-cedet.el ends here
