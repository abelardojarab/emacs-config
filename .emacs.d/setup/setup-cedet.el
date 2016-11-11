;;; setup-cedet.el ---

;; Copyright (C) 2014, 2015, 2016  abelardo.jara-berrocal

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

;; Assure .emacs.cache/semanticdb directory exists
(if (not (file-exists-p "~/.emacs.cache/semanticdb"))
    (make-directory "~/.emacs.cache/semanticdb") t)

;; To use additional features for names completion, and displaying of information for tags & classes,
;; you also need to load the semantic-ia package. Unfortunately, semantic makes Emacs slow
(require 'semantic/ia)

;; Enable support for parsing additional languages
(require 'semantic/wisent)

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

;; Enable decoration mode
(global-semantic-decoration-mode t)

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

;; Enable semanticdb, slows down Emacs
(global-semanticdb-minor-mode nil)

;; This prevents Emacs to become uresponsive
(defun semanticdb-kill-hook ()
  nil)
(defun semanticdb-create-table-for-file-not-in-buffer (arg)
  nil)

;; Default semanticdb directory
(setq-default semanticdb-default-save-directory "~/.emacs.cache/semanticdb")

;; semanticdb support for global/gtags
(when (executable-find "global")
  (semanticdb-enable-gnu-global-databases 'c-mode t)
  (semanticdb-enable-gnu-global-databases 'c++-mode t))

;; Load contrib library
(require 'eassist)

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
