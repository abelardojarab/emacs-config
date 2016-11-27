;;; setup-recentf.el ---

;; Copyright (C) 2014, 2015, 2016  abelardo.jara-berrocal

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

(use-package recentf
  :defer 10
  :commands (recentf-mode
             recentf-add-file
             recentf-apply-filename-handlers)
  :init (progn
          (setq recentf-filename-handlers '(abbreviate-file-name))
          (setq recentf-save-file "~/.emacs.cache/recentf")
          (setq recentf-max-saved-items 300
                recentf-exclude '("/auto-install/" ".recentf" "/repos/" "/elpa/"
                                  "\\.mime-example" "\\.ido.last" "COMMIT_EDITMSG"
                                  ".gz"
                                  "~$" "/tmp/" "/ssh:" "/sudo:" "/scp:")
                recentf-auto-cleanup 600)
          (when (not noninteractive) (recentf-mode 1))

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

          (recentf-mode 1)))

(provide 'setup-recentf)
;;; setup-recentf.el ends here
