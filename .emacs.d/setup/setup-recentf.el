;;; setup-recentf.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2023  Abelardo Jara-Berrocal

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

(use-package recentf
  :hook (on-first-file-hook . recentf-mode)
  :commands (recentf-mode
             recentf-add-file
             recentf-apply-filename-handlers)
  :custom
  (recentf-save-file-modes #o600)
  (recentf-max-saved-items 1024)
  (recentf-auto-cleanup 600)
  (recentf-exclude '(;; compressed files and archives
                     "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$"
                     "\\.bz2$" "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zpaq$"
                     "\\.lz$" "\\.lrz$" "\\.lzo$" "\\.lzma$" "\\.shar$" "\\.kgb$"
                     "\\.zip$" "\\.Z$" "\\.7z$" "\\.rar$"
                     ;; TRAMP paths
                     "^/sudo:" "^/ssh:"))
  (recentf-auto-cleanup 60)
  :init (progn
          (setq recentf-filename-handlers '(abbreviate-file-name))
          (setq recentf-save-file (concat (file-name-as-directory
                                           my/emacs-cache-dir)
                                          "recentf"))

          (defun recentf-save-list ()
            "Save the recent list.
Load the list from the file specified by `recentf-save-file',
merge the changes of your current session, and save it back to
the file."
            (interactive)
            (let ((instance-list (cl-copy-list recentf-list)))
              (recentf-load-list)
              (recentf-merge-with-default-list instance-list)
              (recentf-write-list-to-file)))

          (defun recentf-merge-with-default-list (other-list)
            "Add all items from `other-list' to `recentf-list'."
            (dolist (oitem other-list)
              ;; add-to-list already checks for equal'ity
              (add-to-list 'recentf-list oitem)))

          (defun recentf-write-list-to-file ()
            "Write the recent files list to file.
Uses `recentf-list' as the list and `recentf-save-file' as the
file to write to."
            (condition-case error
                (with-temp-buffer
                  (erase-buffer)
                  (set-buffer-file-coding-system recentf-save-file-coding-system)
                  (insert (format recentf-save-file-header (current-time-string)))
                  (recentf-dump-variable 'recentf-list recentf-max-saved-items)
                  (recentf-dump-variable 'recentf-filter-changer-current)
                  (insert "\n \n;;; Local Variables:\n"
                          (format ";;; coding: %s\n" recentf-save-file-coding-system)
                          ";;; End:\n")
                  (write-file (expand-file-name recentf-save-file))
                  (when recentf-save-file-modes
                    (set-file-modes recentf-save-file recentf-save-file-modes))
                  nil)
              (error
               (warn "recentf mode: %s" (error-message-string error)))))

          (add-to-list 'recentf-exclude no-littering-var-directory)
          (add-to-list 'recentf-exclude no-littering-etc-directory)

          (advice-add #'recentf-cleanup :after #'(lambda (&rest _ignored)
                                                   ;; Don't show the message in the bottom of the screen
                                                   (message nil)))))

(provide 'setup-recentf)
;;; setup-recentf.el ends here
