;;; setup-tags.el ---

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

(use-package etags
  :config (progn
            (unless (fboundp 'push-tag-mark)
              (defun push-tag-mark ()
                "Push the current position to the ring of markers so that
                \\[pop-tag-mark] can be used to come back to current position."
                (interactive)
                (ring-insert find-tag-marker-ring (point-marker))))

            ;; Increase the warning threshold to be more than normal TAGS file sizes
            (setq large-file-warning-threshold (* 50 1024 1024)) ; 50MB
            (setq tags-revert-without-query t)
            (setq tags-always-build-completion-table t)

            ;; Assure .gtags directory exists
            (if (not (file-exists-p "~/.gtags"))
                (make-directory "~/.gtags") t)
            (setenv "GTAGSLIBPATH" "~/.gtags")

            (if (file-exists-p "~/.gtags/TAGS")
                (visit-tags-table "~/.gtags/TAGS")
              (with-temp-buffer (write-file "~/.gtags/TAGS")))
            (setq tags-file-name "~/.gtags/TAGS")
            (setq tags-table-list (list tags-file-name))
            (setq tags-add-tables t)

            ;; etags creation
            (defun create-etags (dir-name)
              "Create tags file."
              (interactive "Directory: ")
              (eshell-command
               (format "find %s -follow -type f -name \"*.[ch]\" | etags -" dir-name)))

            ;; Fix etags bugs (https://groups.google.com/forum/#!msg/gnu.emacs.help/Ew0sTxk0C-g/YsTPVEKTBAAJ)
            (defvar etags--table-line-limit 10)

            (defun etags-tags-completion-table ()   ; Doc string?
              (let (table
                    (progress-reporter
                     (make-progress-reporter
                      (format "Making tags completion table for %s..." buffer-file-name)
                      (point-min) (point-max))))
                (save-excursion
                  (goto-char (point-min))
                  ;; This regexp matches an explicit tag name or the place where
                  ;; it would start.
                  (while (not (eobp))
                    (if (not (re-search-forward
                              "[\f\t\n\r()=,; ]?\177\\\(?:\\([^\n\001]+\\)\001\\)?"
                              ;; Avoid lines that are too long (bug#20703).
                              (+ (point) etags--table-line-limit) t))
                        (forward-line 1)
                      (push (prog1 (if (match-beginning 1)
                                       ;; There is an explicit tag name.
                                       (buffer-substring (match-beginning 1) (match-end 1))
                                     ;; No explicit tag name.  Backtrack a little,
                                     ;; and look for the implicit one.
                                     (goto-char (match-beginning 0))
                                     (skip-chars-backward "^\f\t\n\r()=,; ")
                                     (prog1
                                         (buffer-substring (point) (match-beginning 0))
                                       (goto-char (match-end 0))))
                              (progress-reporter-update progress-reporter (point)))
                            table))))
                table))

            (defun tags-completion-table ()
              "Build `tags-completion-table' on demand.
The tags included in the completion table are those in the current
tags table and its (recursively) included tags tables."
              (or tags-completion-table
                  ;; No cached value for this buffer.
                  (condition-case ()
                      (let (current-table combined-table)
                        (message "Making tags completion table for %s..." buffer-file-name)
                        (save-excursion
                          ;; Iterate over the current list of tags tables.
                          (while (visit-tags-table-buffer (and combined-table t))
                            ;; Find possible completions in this table.
                            (setq current-table (funcall tags-completion-table-function))
                            ;; Merge this buffer's completions into the combined table.
                            (if combined-table
                                (mapatoms
                                 (lambda (sym) (intern (symbol-name sym) combined-table))
                                 current-table)
                              (setq combined-table current-table))))
                        (message "Making tags completion table for %s...done"
                                 buffer-file-name)
                        ;; Cache the result in a buffer-local variable.
                        (setq tags-completion-table combined-table))
                    (quit (message "Tags completion table construction cancelled")
                          (setq tags-completion-table nil)))))))

;; Implementing my own copy of this function since it is required by
;; semantic-ia-fast-jump but this function is not defined in etags.el
;; of GNU emacs
(use-package etags-select
  :commands (etags-select-find-tag ido-find-tag)
  :load-path (lambda () (expand-file-name "etags-select/" user-emacs-directory))
  :config (progn
            ;; Use ido to list tags, but then select via etags-select (best of both worlds!)
            (defun ido-find-tag ()
              "Find a tag using ido"
              (interactive)
              (tags-completion-table)
              (let (tag-names)
                (mapatoms (lambda (x)
                            (push (prin1-to-string x t) tag-names))
                          tags-completion-table)
                (find-tag (replace-regexp-in-string "\\\\" "" (ido-completing-read "Tag: " tag-names)))))))

;; Etags table
(use-package etags-table
  :after projectile
  :config (progn
            (setq etags-table-alist
                  (list
                   ;; For jumping to standard headers:
                   '(".*\\.\\([ch]\\|cpp\\)" "~/.gtags/TAGS")))

            ;; Max depth to search up for a tags file.  nil means don't search.
            (setq etags-table-search-up-depth 2)

            ;; Below function comes useful when you change the project-root
            ;; symbol to a different value (when switching projects)
            (defun update-etags-table ()
              "Update `etags-table-alist' based on the current project directory."
              (interactive)
              (add-to-list 'etags-table-alist
                           `(,(concat (projectile-project-root) ".*")
                             ,(concat (projectile-project-root) "TAGS"))
                           t))))

;; Ctags
(use-package ctags
  :if (executable-find "ctags")
  :after projectile
  :config (progn
            ;; Helper functions for etags/ctags
            (defun create-ctags (dir-name)
              "Create tags file."
              (interactive "Directory: ")
              (shell-command
               (format "ctags -f %s -e -R %s" path-to-ctags (projectile-project-root))))))

;; Gtags
(use-package ggtags
  :if (executable-find "global")
  :after (eldoc)
  :commands (ggtags-mode ggtags-find-tag-dwim ggtags-eldoc-function ggtags-show-definition)
  :diminish ggtags-mode
  :load-path (lambda () (expand-file-name "ggtags/" user-emacs-directory))
  :config (progn
            (add-hook 'c-mode-common-hook
                      (lambda ()
                        (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                          (ggtags-mode 1)
                          (setq-local eldoc-documentation-function #'ggtags-eldoc-function)
                          (setq-local imenu-create-index-function #'ggtags-build-imenu-index))))

            (defun gtags-create-or-update ()
              "Create or update the GNU-Global tag file"
              (interactive
               (if (zerop (call-process "global" nil nil nil "-p"))
                   ;; case 1: tag file exists: update
                   (progn
                     (shell-command "global -u -q 2>/dev/null")
                     (message "Tagfile updated"))
                 ;; case 2: no tag file yet: create it
                 (when (yes-or-no-p "Create tagfile?")
                   (let ((olddir default-directory)
                         (default-directory
                           (read-directory-name
                            "gtags: top of source tree:" default-directory)))
                     (shell-command "gtags -i -q 2>/dev/null")
                     (message "Created tagfile"))))))))

(provide 'setup-tags)
;;; setup-tags.el ends here
