;;; setup-calendar.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2020  Abelardo Jara-Berrocal

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

;; Calendar and diary file
(use-package calendar
  :defer t
  :commands (calendar-mode)
  :hook ((calendar-today-visible      . calendar-mark-today)
         (today-visible-calendar-hook . calendar-mark-today))
  :custom ((calendar-set-date-style            'iso)
           (calendar-week-start-day            1)
           (calendar-view-diary-initially-flag t)
           (calendar-mark-diary-entries-flag   t)
           (calendar-today-marker              'calendar-today-face))
  :config (progn
            ;; Set up Org default files
            (let ((todo_workspace "~/workspace/Documents/Org/todo.org")
                  (todo_dropbox "~/Dropbox/Documents/Org/todo.org")
                  (todo_googledrive "~/Google Drive/Documents/Org/todo.org"))
              (when (or (file-readable-p todo_workspace)
                        (file-readable-p todo_dropbox)
                        (file-readable-p todo_googledrive))

                ;; Set base Org directory
                (if (file-exists-p todo_dropbox)
                    (setq org-directory "~/Dropbox/Documents/Org")
                  (if (file-exists-p todo_workspace)
                      (setq org-directory "~/workspace/Documents/Org")
                    (setq org-directory "~/Google Drive/Documents/Org")))))

            (setq diary-file (concat org-directory "/agenda.org"))

            ;; Enable Org date formatting
            (setq diary-date-forms (cons '(" *" "[<\* ]*" " *"
                                           dayname "[, ]*" " *"
                                           monthname "[, ]*" " *"
                                           day "[, ]*" " *"
                                           year "[^0-9]*[>]*")
                                         diary-date-forms))

            (defun my/insert-current-date (&optional omit-day-of-week-p)
              "Insert today's date using the current locale.
  With a prefix argument, the date is inserted without the day of
  the week."
              (interactive "P*")
              (insert (calendar-date-string (calendar-current-date) nil
                                            omit-day-of-week-p)))

            ;; Enable diary file in Org mode
            (eval-after-load 'org-agenda
              (setq org-agenda-include-diary (file-exists-p diary-file)))))

(provide 'setup-calendar)
;;; setup-calendar.el ends here
