;;; setup-cedet.el ---

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

(use-package semantic
  :demand t
  :custom ((format-args                                     t)
           (semantic-idle-work-parse-neighboring-files-flag nil)
           (semantic-idle-work-update-headers-flag          nil)
           (semantic-idle-scheduler-idle-time               10)
           (semantic-idle-scheduler-work-idle-time          60)
           (semantic-idle-scheduler-max-buffer-size         1)
           (semanticdb-search-system-databases              t))
  :config (progn

            ;; To use additional features for names completion, and displaying of information for tags & classes,
            ;; you also need to load the semantic-ia package. Unfortunately, semantic makes Emacs slow
            (use-package semantic/ia)

            ;; Enable support for parsing additional languages
            (use-package semantic/wisent)

            ;; The Semantic Database (SemanticDB) caches the results of parsing source code files.
            (use-package semantic/db)

            ;; Enabled features
            (setq semantic-default-submodes '(global-semanticdb-minor-mode
                                              global-semantic-mru-bookmark-mode
                                              global-semantic-load-enable-code-helpers
                                              global-semantic-idle-scheduler-mode))

            ;; Assure .emacs.cache/semanticdb directory exists
            (if (not (file-exists-p (concat (file-name-as-directory
                                             my/emacs-cache-dir)
                                            "semanticdb")))
                (make-directory (concat (file-name-as-directory
                                         my/emacs-cache-dir)
                                        "semanticdb") t))

            ;; Disable semantics for large files
            (add-hook 'semantic--before-fetch-tags-hook
                      (lambda () (if (and (>= (point-max) 0)
                                     (not (semantic-parse-tree-needs-rebuild-p)))
                                nil
                              t)))

            ;; Don't use information from system include files, by removing system
            (setq-mode-local c-mode semanticdb-find-default-throttle
                             '(project unloaded system recursive))
            (setq-mode-local c++-mode semanticdb-find-default-throttle
                             '(project unloaded system recursive))

            ;; Enable decoration mode
            ;; (global-semantic-decoration-mode t)

            ;; Define missing variable and disable mode
            (defvar global-semantic-idle-summary-mode nil)
            (setq global-semantic-idle-summary-mode nil)
            (global-semantic-idle-summary-mode nil)

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
            (add-hook 'c-mode-common-hook
                      (lambda ()
                        (setq semanticdb-project-system-databases
                              (list (semanticdb-create-database
                                     semanticdb-new-database-class
                                     "/usr/include")))))

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

            (defun semantic-force-refresh ()
              "Force a full refresh of the current buffer's tags.
Throw away all the old tags, and recreate the tag database."
              (interactive)
              (semantic-clear-toplevel-cache)
              (semantic-fetch-tags))

            ;; Default semanticdb directory
            (setq-default semanticdb-default-save-directory (concat (file-name-as-directory
                                                                     my/emacs-cache-dir)
                                                                    "semanticdb"))

            ;; semanticdb support for global/gtags
            (when (executable-find "global")
              (semanticdb-enable-gnu-global-databases 'c-mode t)
              (semanticdb-enable-gnu-global-databases 'c++-mode t))))

;; Show function in mode-line
(use-package which-func
  :demand t
  :commands which-function-mode
  :custom ((which-func-unknown "⊥")
           (which-func-maxout  1024)
           (which-func-modes   '(latex-mode
                                 markdown-mode
                                 org-mode
                                 emacs-lisp-mode
                                 python-mode
                                 c-mode
                                 c++-mode)))
  :config (progn
            (which-function-mode -1)
            (ignore-errors
              (defun which-func-update () nil)
              (cancel-function-timers 'which-func-update))))

(provide 'setup-cedet)
;;; setup-cedet.el ends here
