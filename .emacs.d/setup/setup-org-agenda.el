;;; setup-org-agenda.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Abelardo Jara-Berrocal

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

;; appointment notification functions
(use-package appt
  :defer t
  :commands (appt-activate org-agenda-to-appt)
  :config (progn
            (setq appt-message-warning-time        30
                  appt-display-interval            15
                  appt-display-mode-line           t
                  calendar-mark-diary-entries-flag t
                  appt-display-diary               nil
                  appt-display-format              'window)

            ;; Make appt aware of appointments from the agenda
            (defun org-agenda-to-appt ()
              "Activate appointments found in `org-agenda-files'."
              (interactive)
              (let* ((today (org-date-to-gregorian
                             (time-to-days (current-time))))
                     (files org-agenda-files) entries file)
                (while (setq file (pop files))
                  (setq entries (append entries (org-agenda-get-day-entries
                                                 file today :timestamp))))
                (setq entries (delq nil entries))
                (mapc (lambda(x)
                        (let* ((event (org-trim (get-text-property 1 'txt x)))
                               (time-of-day (get-text-property 1 'time-of-day x)) tod)
                          (when time-of-day
                            (setq tod (number-to-string time-of-day)
                                  tod (when (string-match
                                             "\\([0-9]\\{1,2\\}\\)\\([0-9]\\{2\\}\\)" tod)
                                        (concat (match-string 1 tod) ":"
                                                (match-string 2 tod))))
                            (if tod (appt-add tod event))))) entries)))))

(use-package org-agenda
  :demand t
  :after (org calendar)
  :commands (org-agenda org-archive-done-tasks)
  :bind (("C-c A" . org-agenda)
         :map org-mode-map
         ("C-c i" . my/org-add-line-item-task)
         :map org-agenda-mode-map
         ("Y"     . org-agenda-todo-yesterday)
         ("x"     . my/org-agenda-done)
         ("X"     . my/org-agenda-mark-done-and-add-followup)
         ("N"     . my/org-agenda-new))
  :config (progn

            (defadvice org-agenda (around split-vertically activate)
              (let ((split-width-threshold 100)) ;; or whatever width makes sense for you
                ad-do-it))

            ;; Org log
            (setq org-log-done                  'time ;; Insert only timestamp when closing an Org TODO item
                  org-enforce-todo-dependencies t)

            ;; Agenda settings
            (setq org-agenda-inhibit-startup                       t
                  org-agenda-dim-blocked-tasks                     t
                  org-agenda-default-appointment-duration          60
                  org-agenda-skip-deadline-prewarning-if-scheduled t
                  org-agenda-window-setup                          'current-window
                  org-agenda-sticky                                t
                  org-agenda-compact-blocks                        t
                  org-agenda-span                                  14
                  org-agenda-start-on-weekday                      nil
                  org-agenda-show-log                              t
                  org-agenda-show-all-dates                        t
                  org-agenda-skip-scheduled-if-done                t
                  org-agenda-skip-deadline-if-done                 t
                  org-deadline-warning-days                        7
                  org-agenda-time-grid
                  '((daily today require-timed)
                    "----------------"
                    (800 1000 1200 1400 1600 1800)))

            ;; Formatting org-agenda view
            (setq org-columns-default-format "%50ITEM %12SCHEDULED %TODO %3PRIORITY %Effort{:} %TAGS")

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

                ;; This will give you <Thu Jan 26 2016> for date timestamps
                ;; or <Thu Jan 26 2016 11:30> for timestamps with times.
                (setq-default org-display-custom-times t)
                (setq org-time-stamp-custom-formats '("<%A, %B %e, %Y>" . "<%A, %B %e, %Y %H:%M>"))

                ;; Set remaining Org files
                (setq org-id-locations-file (concat (file-name-as-directory
                                                     my/emacs-cache-dir)
                                                    "org-id-locations"))
                (setq org-default-notes-file  (concat org-directory "/notes.org")
                      org-default-refile-file (concat org-directory "/refile.org")
                      org-agenda-default-file (concat org-directory "/agenda.org")
                      org-agenda-diary-file   (concat org-directory "/todo.org")
                      org-agenda-todo-file    (concat org-directory "/todo.org")
                      org-mobile-file         (concat org-directory "/mobile.org")
                      org-mobile-directory    (concat org-directory "/mobile"))
                (setq org-agenda-files (list
                                        (concat org-directory "/diary.org")
                                        (concat org-directory "/agenda.org")
                                        (concat org-directory "/todo.org")
                                        (concat org-directory "/notes.org")
                                        (concat org-directory "/mobile.org")
                                        (concat org-directory "/refile.org")
                                        (concat org-directory "/refile-beorg.org")))))

            ;; Sync org-agenda with appt package
            (add-hook 'org-finalize-agenda-hook
                      (lambda ()
                        (hl-line-mode t)
                        (org-agenda-to-appt)                      ;; copy all agenda schedule to appointments
                        (appt-activate 1)))                       ;; active appt (appointment notification)

            ;; Fontify done checkbox items in org-mode
            (font-lock-add-keywords
             'org-mode
             `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-headline-done prepend))
             'append)

            ;; define todo states: set time stamps one waiting, delegated and done
            (setq org-todo-keywords
                  '((sequence "TODO(t!)" "|" "IN PROGRESS(p@/!)" "DONE(d!)" "CANCELLED(c@/!)" "HOLD(h!)")
                    (sequence "TASK(t!)" "|" "DONE(d!)" "IN PROGRESS(p@/!)" "CANCELLED(c@/!)")
                    (sequence "IDEA(i!)" "MAYBE(y!)" "STAGED(s!)" "WORKING(k!)" "|" "USED(u!/@)")))
            (setq org-todo-keyword-faces
                  '(("IN PROGRESS" . 'warning)
                    ("HOLD"        . 'font-lock-keyword-face)
                    ("TASK"        . 'font-lock-builtin-face)
                    ("CANCELLED"   . 'font-lock-doc-face)))

            ;; Tag tasks with GTD-ish contexts
            (setq org-tag-alist '(("@work"       . ?b)
                                  ("@home"       . ?h)
                                  ("@place"      . ?p)
                                  ("@writing"    . ?w)
                                  ("@errands"    . ?e)
                                  ("@family"     . ?f)
                                  ("@coding"     . ?c)
                                  ("@tasks"      . ?t)
                                  ("@learning"   . ?l)
                                  ("@reading"    . ?r)
                                  ("quantified"  . ?q)
                                  ("high-energy" . ?1)))

            ;; Projects
            (setq org-tags-exclude-from-inheritance '("PROJECT"))

            ;; Enable filtering by effort estimates
            (add-to-list 'org-global-properties
                         '("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00"))

            ;; Archive done tasks
            (defun org-archive-done-tasks ()
              (interactive)
              (org-map-entries
               (lambda ()
                 (org-archive-subtree)
                 (setq org-map-continue-from (outline-previous-heading)))
               "/DONE" 'tree))

            ;; Modifying org agenda so that I can display a subset of tasks
            (defvar my/org-agenda-limit-items nil "Number of items to show in agenda to-do views; nil if unlimited.")
            (defadvice org-agenda-finalize-entries (around my activate)
              (if my/org-agenda-limit-items
                  (progn
                    (setq list (mapcar 'org-agenda-highlight-todo list))
                    (if nosort
                        (setq ad-return-value
                              (subseq list 0 my/org-agenda-limit-items))
                      (when org-agenda-before-sorting-filter-function
                        (setq list (delq nil (mapcar org-agenda-before-sorting-filter-function list))))
                      (setq ad-return-value
                            (mapconcat 'identity
                                       (delq nil
                                             (subseq
                                              (sort list 'org-entries-lessp)
                                              0
                                              my/org-agenda-limit-items))
                                       "\n"))))
                ad-do-it))

            ;; If today is Friday, I want +fri to be next Friday.
            (defun org-read-date-get-relative (s today default)
              "Check string S for special relative date string.
TODAY and DEFAULT are internal times, for today and for a default.
Return shift list (N what def-flag)
WHAT       is \"d\", \"w\", \"m\", or \"y\" for day, week, month, year.
N          is the number of WHATs to shift.
DEF-FLAG   is t when a double ++ or -- indicates shift relative to
           the DEFAULT date rather than TODAY."
              (use-package parse-time)
              (when (and
                     (string-match
                      (concat
                       "\\`[ \t]*\\([-+]\\{0,2\\}\\)"
                       "\\([0-9]+\\)?"
                       "\\([hdwmy]\\|\\(" (mapconcat 'car parse-time-weekdays "\\|") "\\)\\)?"
                       "\\([ \t]\\|$\\)") s)
                     (or (> (match-end 1) (match-beginning 1)) (match-end 4)))
                (let* ((dir (if (> (match-end 1) (match-beginning 1))
                                (string-to-char (substring (match-string 1 s) -1))
                              ?+))
                       (rel (and (match-end 1) (= 2 (- (match-end 1) (match-beginning 1)))))
                       (n (if (match-end 2) (string-to-number (match-string 2 s)) 1))
                       (what (if (match-end 3) (match-string 3 s) "d"))
                       (wday1 (cdr (assoc (downcase what) parse-time-weekdays)))
                       (date (if rel default today))
                       (wday (nth 6 (decode-time date)))
                       delta)
                  (if wday1
                      (progn
                        (setq delta (mod (+ 7 (- wday1 wday)) 7))
                        (if (= delta 0) (setq delta 7))
                        (if (= dir ?-)
                            (progn
                              (setq delta (- delta 7))
                              (if (= delta 0) (setq delta -7))))
                        (if (> n 1) (setq delta (+ delta (* (1- n) (if (= dir ?-) -7 7)))))
                        (list delta "d" rel))
                    (list (* n (if (= dir ?-) -1 1)) what rel)))))

            ;; Org agenda custom commands
            (defvar my/org-agenda-contexts
              '((tags-todo "+@work/!-SOMEDAY")
                (tags-todo "+@coding/!-SOMEDAY")
                (tags-todo "+@writing/!-SOMEDAY")
                (tags-todo "+@learning/!-SOMEDAY")
                (tags-todo "+@home/!-SOMEDAY")
                (tags-todo "+@errands/!-SOMEDAY"))
              "Usual list of contexts.")
            (defun my/org-agenda-skip-scheduled ()
              (org-agenda-skip-entry-if 'scheduled 'deadline
                                        'regexp "\n]+>"))
            (setq org-agenda-custom-commands
                  `(("T" todo-tree "TODO")
                    ("n" todo "notes.org"
                     ((org-agenda-files (list org-default-notes-file))))
                    ("b" todo "agenda.org"
                     ((org-agenda-files (list org-agenda-default-file))))
                    ("w" todo "todo.org"
                     ((org-agenda-files (list org-agenda-todo-file))))
                    ("v" todo "Column View"
                     ((org-agenda-prefix-format "")
                      (org-agenda-cmp-user-defined 'my/org-sort-agenda-items-todo)
                      (org-agenda-view-columns-initially t)))
                    ;; Weekly review
                    ("w" "Weekly review" agenda "" ((org-agenda-span 7) (org-agenda-log-mode 1)))
                    ("W" "Weekly review sans routines" agenda ""
                     ((org-agenda-span 7) (org-agenda-log-mode 1)
                      (org-agenda-files (list org-agenda-todo-file org-agenda-default-file))))
                    ("2" "Bi-weekly review" agenda "" ((org-agenda-span 14) (org-agenda-log-mode 1)))
                    ("gw" "Work" tags-todo "@work"
                     ((org-agenda-view-columns-initially t)))
                    ("gt" "Tasks" tags-todo "@tasks"
                     ((org-agenda-view-columns-initially t)))
                    ("gc" "Coding" tags-todo "@coding"
                     ((org-agenda-view-columns-initially t)))
                    ("gh" "Home" tags-todo "@home"
                     ((org-agenda-view-columns-initially t)))
                    ("ge" "Errands" tags-todo "@errands"
                     ((org-agenda-view-columns-initially t)))
                    ("0" "Top 3 by context"
                     ,my/org-agenda-contexts
                     ((org-agenda-sorting-strategy '(priority-up effort-down))
                      (my/org-agenda-limit-items 3)))
                    (")" "All by context"
                     ,my/org-agenda-contexts
                     ((org-agenda-sorting-strategy '(priority-down effort-down))
                      (my/org-agenda-limit-items nil)))
                    ("9" "Unscheduled top 3 by context"
                     ,my/org-agenda-contexts
                     ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
                      (org-agenda-sorting-strategy '(priority-down effort-down))
                      (my/org-agenda-limit-items 3)))
                    ("(" "All unscheduled by context"
                     ,my/org-agenda-contexts
                     ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
                      (org-agenda-sorting-strategy '(priority-down effort-down))
                      ))
                    ("d" "Timeline for today" ((agenda "" ))
                     ((org-agenda-ndays 1)
                      (org-agenda-show-log t)
                      (org-agenda-log-mode-items '(clock closed))
                      (org-agenda-clockreport-mode t)
                      (org-agenda-entry-types '())))
                    ("w" "Waiting for" todo "HOLD")
                    ("u" "Unscheduled tasks" alltodo ""
                     ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
                      (org-agenda-overriding-header "Unscheduled TODO entries: ")))
                    ("P" "By priority"
                     ((tags-todo "+PRIORITY=\"A\"")
                      (tags-todo "+PRIORITY=\"B\"")
                      (tags-todo "+PRIORITY=\"\"")
                      (tags-todo "+PRIORITY=\"C\""))
                     ((org-agenda-prefix-format "%-10c %-10T %e ")
                      (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
                    ("p" tags "+PROJECT"
                     ((org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))) )
                    ("S" tags-todo "TODO=\"IN PROGRESS\"")
                    ("2" "List projects with tasks" my/org-agenda-projects-and-tasks
                     "+PROJECT"
                     ((my/org-agenda-limit-items 3)))))


            ;; Templates
            (setq org-capture-templates `(

                                          ;; For notes or something regarding more work
                                          ("t"               ;; key
                                           "TODO"            ;; name
                                           entry             ;; type
                                           (file+headline ,org-agenda-todo-file "Work")  ;; target
                                           "* TODO %^{Todo} %(org-set-tags)  :@work:
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:Created: %U
:END:
%i
%?"  ;; template
                                           :clock-in t
                                           :clock-resume t
                                           :prepend t        ;; properties
                                           :empty-lines 1    ;; properties
                                           :created t        ;; properties
                                           :kill-buffer t)   ;; properties

                                          ;; For capturing minutes of the meeting
                                          ("m"                              ;; key
                                           "Meeting or Appointment"         ;; name
                                           entry                            ;; type
                                           (file+datetree ,org-agenda-default-file "Meeting")  ;; target
                                           "* %^{Title} %(org-set-tags)  :@meeting:
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:Created: %U
:END:%i
** Agenda:
%?

** Minutes of the meeting:
"  ;; template
                                           :clock-in t
                                           :clock-resume t
                                           :prepend t        ;; properties
                                           :empty-lines 1    ;; properties
                                           :created t        ;; properties
                                           :kill-buffer t)   ;; properties

                                          ;; For taking notes on random things
                                          ("n"               ;; key
                                           "Note"            ;; name
                                           entry             ;; type
                                           (file+headline ,org-default-notes-file "Notes")  ;; target
                                           "* %? %(org-set-tags)  :@note:
:PROPERTIES:
:Created: %U
:Linked: %A
:END:
Captured %<%Y-%m-%d %H:%M>
%i"  ;; template
                                           :prepend t        ;; properties
                                           :empty-lines 1    ;; properties
                                           :created t        ;; properties
                                           :kill-buffer t)   ;; properties

                                          ;; Ledger is a CLI accounting system
                                          ("l"               ;; key
                                           "Ledger"          ;; name
                                           entry             ;; type
                                           (file+datetree ,org-default-notes-file "Ledger")  ;; target
                                           "* %^{expense} %(org-set-tags)  :@accounts:
:PROPERTIES:
:Created: %U
:END:
%i
#+NAME: %\\1-%t
\#+BEGIN_SRC ledger :noweb yes
%^{Date of expense (yyyy/mm/dd)} %^{'*' if cleared, else blank} %\\1
    %^{Account name}                                $%^{Amount}
    %?
\#+END_SRC
"  ;; template
                                           :prepend t        ;; properties
                                           :empty-lines 1    ;; properties
                                           :created t        ;; properties
                                           :kill-buffer t)   ;; properties

                                          ;; For code snippets
                                          ("c"               ;; key
                                           "Coding"          ;; name
                                           entry             ;; type
                                           (file+headline ,org-default-notes-file "Coding")  ;; target
                                           "* %^{TITLE} %(org-set-tags)  :@coding:
:PROPERTIES:
:Created: %U
:END:
%i\#+BEGIN_SRC %^{language}
%?
\#END_SRC"  ;; template
                                           :prepend t        ;; properties
                                           :empty-lines 1    ;; properties
                                           :created t        ;; properties
                                           :kill-buffer t)   ;; properties

                                          ;; For capturing some things that are worth reading
                                          ("r"               ;; key
                                           "Reading"         ;; name
                                           entry             ;; type
                                           (file+headline ,org-default-notes-file "Reading")  ;; target
                                           "* %^{Title} %(org-set-tags)  :@reading:
:PROPERTIES:
:Created: %U
:END:
%i
%?"  ;; template
                                           :prepend t        ;; properties
                                           :empty-lines 1    ;; properties
                                           :created t        ;; properties
                                           :kill-buffer t)   ;; properties

                                          ;; To capture ideas for writing
                                          ("w"               ;; key
                                           "Writing"            ;; name
                                           entry             ;; type
                                           (file+headline ,org-default-notes-file "Writing")  ;; target
                                           "* %^{Title} %(org-set-tags)  :@writing:
:PROPERTIES:
:Created: %U
:END:
%i
%?"  ;; template
                                           :prepend t        ;; properties
                                           :empty-lines 1    ;; properties
                                           :created t        ;; properties
                                           :kill-buffer t)   ;; properties

                                          ;; To capture errands
                                          ("e"               ;; key
                                           "Errands"         ;; name
                                           entry             ;; type
                                           (file+headline ,org-agenda-todo-file "Errands")  ;; target
                                           "* TODO %^{Todo} %(org-set-tags)  :@errands:
:PROPERTIES:
:Created: %U
:END:
%i
%?"  ;; template
                                           :prepend t        ;; properties
                                           :empty-lines 1    ;; properties
                                           :created t        ;; properties
                                           :kill-buffer t)   ;; properties

                                          ;; To capture diary entries
                                          ("d"               ;; key
                                           "Diary"           ;; name
                                           entry             ;; type
                                           (file+datetree ,org-agenda-diary-file) ;; target
                                           "* TODO %^{Todo} %(org-set-tags)
:PROPERTIES:
:Created: %U
:END:
%i
%?"  ;; template
                                           :clock-in t
                                           :clock-resume t
                                           :prepend t        ;; properties
                                           :empty-lines 1    ;; properties
                                           :created t        ;; properties
                                           :kill-buffer t)   ;; properties

                                          )) ;; properties

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

;; Org and Jira integration
(use-package org-jira
  :defer t
  :commands (org-jira-get-projects
             org-jira-browse-issue
             org-jira-get-issues
             org-jira-get-issues-headonly
             org-jira-get-issues-from-filter-headonly
             org-jira-get-issues-from-filter
             org-jira-update-issue
             org-jira-progress-issue
             org-jira-refresh-issue
             org-jira-create-issue
             org-jira-copy-current-issue-key
             org-jira-create-subtask
             org-jira-get-subtasks
             org-jira-update-comment
             org-jira-todo-to-jira)
  :custom (jiralib-url "http://jaraberrocal.readmyblog.org:8080")
  :config (setq org-jira-working-dir  (concat org-directory "/JIRA")))

(provide 'setup-org-agenda)
;;; setup-org-agenda.el ends here
