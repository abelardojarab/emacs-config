;;; setup-org-agenda.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojara@ubuntu-MacBookPro>
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

(use-package org-agenda
  :after org
  :config (progn
            ;; Org log
            (setq org-log-done t
                  org-enforce-todo-dependencies t)

            ;; Agenda settings
            (setq org-agenda-inhibit-startup t ;; 50x speedup
                  org-agenda-use-tag-inheritance nil ;; 3-4x speedup
                  org-agenda-tags-column -117
                  org-agenda-include-diary nil
                  org-agenda-dim-blocked-tasks t
                  org-agenda-default-appointment-duration 60
                  org-agenda-skip-deadline-prewarning-if-scheduled t
                  org-agenda-ignore-drawer-properties '(effort appt category)
                  ;; Current window gets agenda
                  org-agenda-window-setup 'current-window
                  ;; Use sticky agenda's so they persist
                  org-agenda-sticky t
                  ;; Compact the block agenda view
                  org-agenda-compact-blocks t
                  ;; span 14 days of agenda
                  org-agenda-span 14
                  ;; start today not on Monday
                  org-agenda-start-on-weekday nil
                  org-agenda-show-log t
                  org-agenda-show-all-dates t
                  org-agenda-skip-scheduled-if-done t
                  org-agenda-skip-deadline-if-done t
                  org-deadline-warning-days 7
                  org-agenda-time-grid
                  '((daily today require-timed)
                    "----------------"
                    (800 1000 1200 1400 1600 1800)))
            (add-hook 'org-agenda-finalize-hook 'hl-line-mode)

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
                    (setq org-directory "~/Google Drive/Documents/Org")))

                ;; Set remaining Org files
                (setq diary-file (concat org-directory "/diary"))
                (setq org-id-locations-file "~/.emacs.cache/org-id-locations")
                (setq org-default-notes-file (concat org-directory "/notes.org"))
                (setq org-default-refile-file (concat org-directory "/refile.org"))
                (setq org-agenda-diary-file (concat org-directory "/todo.org"))
                (setq org-mobile-file (concat org-directory "/mobileorg.org"))
                (setq org-mobile-directory (concat org-directory "/mobile-org"))
                (setq org-agenda-files (list
                                        (concat org-directory "/diary.org")
                                        (concat org-directory "/agenda.org")
                                        (concat org-directory "/todo.org")
                                        (concat org-directory "/notes.org")
                                        (concat org-directory "/mobileorg.org")
                                        (concat org-directory "/refile.org")))))

            ;; define todo states: set time stamps one waiting, delegated and done
            (setq org-todo-keywords
                  '((sequence "TODO(t!)" "|" "IN PROGRESS(p@/!)" "DONE(d!)" "CANCELLED(c@/!)" "HOLD(h!)")
                    (sequence "TASK(t!)" "|" "DONE(d!)" "IN PROGRESS(p@/!)" "CANCELLED(c@/!)")
                    (sequence "IDEA(i!)" "MAYBE(y!)" "STAGED(s!)" "WORKING(k!)" "|" "USED(u!/@)")))
            (setq org-todo-keyword-faces
                  '(("IN PROGRESS" . 'warning)
                    ("HOLD" . 'font-lock-keyword-face)
                    ("TASK" . 'font-lock-builtin-face)
                    ("CANCELLED" . 'font-lock-doc-face)))

            ;; Tag tasks with GTD-ish contexts
            (setq org-tag-alist '(("@work" . ?b)
                                  ("@home" . ?h)
                                  ("@place" . ?p)
                                  ("@writing" . ?w)
                                  ("@errands" . ?e)
                                  ("@family" . ?f)
                                  ("@coding" . ?c)
                                  ("@tasks" . ?t)
                                  ("@learning" . ?l)
                                  ("@reading" . ?r)
                                  ("time" . ?q)
                                  ("high-energy" . ?1)))

            ;; Enable filtering by effort estimates
            (add-to-list 'org-global-properties
                         '("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00"))

            ;; Refiling options
            (setq org-refile-targets (quote ((org-agenda-files :maxlevel . 9)))
                  org-refile-use-outline-path nil
                  ;; Stop using paths for refile targets - we file directly with helm
                  org-refile-use-outline-path nil
                  ;; Allow refile to create parent tasks with confirmation
                  org-refile-allow-creating-parent-nodes '(confirm))

            ;; Exclude DONE state tasks from refile targets
            (defun my/verify-refile-target ()
              (not (member (nth 2 (org-heading-components)) org-done-keywords)))
            (setq org-refile-target-verify-function 'my/verify-refile-target)))

;; Calendar viewer
(use-package calfw
  :defer t
  :commands cfw:open-org-calendar
  :load-path (lambda () (expand-file-name "calfw/" user-emacs-directory))
  :config (use-package calfw-org))

;; Lorg calendar
(use-package lorg-calendar
  :load-path (lambda () (expand-file-name "lorg-calendar/" user-emacs-directory)))

(provide 'setup-org-agenda)
;;; setup-org-agenda.el ends here
