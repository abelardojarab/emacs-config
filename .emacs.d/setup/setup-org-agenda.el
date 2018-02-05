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

(use-package org-agenda
  :defer t
  :after (org calendar)
  :commands org-agenda
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
            (setq org-log-done t
                  org-enforce-todo-dependencies t)

            ;; Agenda settings
            (setq org-agenda-inhibit-startup t ;; 50x speedup
                  org-agenda-dim-blocked-tasks t
                  org-agenda-default-appointment-duration 60
                  org-agenda-skip-deadline-prewarning-if-scheduled t
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

                ;; http://emacs.stackexchange.com/questions/19863/how-to-set-my-own-date-format-for-org
                ;; This will give you <Thu Jan 26 2016> for date timestamps
                ;; or <Thu Jan 26 2016 11:30> for timestamps with times.
                (setq-default org-display-custom-times t)
                (setq org-time-stamp-custom-formats '("<%A, %B %e, %Y>" . "<%A, %B %e, %Y %H:%M>"))

                ;; Set remaining Org files
                (setq org-id-locations-file (concat (file-name-as-directory
                                                     my/emacs-cache-dir)
                                                    "org-id-locations"))
                (setq org-default-notes-file (concat org-directory "/notes.org"))
                (setq org-default-refile-file (concat org-directory "/refile.org"))
                (setq org-agenda-default-file (concat org-directory "/agenda.org"))
                (setq org-agenda-diary-file (concat org-directory "/todo.org"))
                (setq org-agenda-todo-file (concat org-directory "/todo.org"))
                (setq org-mobile-file (concat org-directory "/mobile.org"))
                (setq org-mobile-directory (concat org-directory "/mobile"))
                (setq org-agenda-files (list
                                        (concat org-directory "/diary.org")
                                        (concat org-directory "/agenda.org")
                                        (concat org-directory "/todo.org")
                                        (concat org-directory "/notes.org")
                                        (concat org-directory "/mobile.org")
                                        (concat org-directory "/refile.org")))))

            ;; Make appt aware of appointments from the agenda
            ;; http://sachachua.com/blog/2007/11/setting-up-appointment-reminders-in-org/
            (defun org-agenda-to-appt ()
              "Activate appointments found in `org-agenda-files'."
              (interactive)
              (require 'org)
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
                            (if tod (appt-add tod event))))) entries)))
            (add-hook 'org-finalize-agenda-hook
                      (lambda ()
                        (hl-line-mode t)
                        (setq appt-message-warning-time 10        ;; warn 10 min in advance
                              appt-display-diary nil              ;; do not display diary when (appt-activate) is called
                              appt-display-mode-line t            ;; show in the modeline
                              appt-display-format 'window         ;; display notification in window
                              calendar-mark-diary-entries-flag t) ;; mark diary entries in calendar
                        (org-agenda-to-appt)                      ;; copy all agenda schedule to appointments
                        (appt-activate 1)))                       ;; active appt (appointment notification)

            ;; Fontify done checkbox items in org-mode
            ;; https://fuco1.github.io/2017-05-25-Fontify-done-checkbox-items-in-org-mode.html
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
            (setq org-tag-alist '(("@work"      . ?b)
                                  ("@home"      . ?h)
                                  ("@place"     . ?p)
                                  ("@writing"       . ?w)
                                  ("@errands"       . ?e)
                                  ("@family"        . ?f)
                                  ("@coding"        . ?c)
                                  ("@tasks"     . ?t)
                                  ("@learning"      . ?l)
                                  ("@reading"       . ?r)
                                  ("quantified"     . ?q)
                                  ("high-energy"    . ?1)))

            ;; Projects
            (setq org-tags-exclude-from-inheritance '("PROJECT"))

            ;; Enable filtering by effort estimates
            (add-to-list 'org-global-properties
                         '("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00"))

            ;; From “Add an effort estimate on the fly when clocking in” on the Org Hacks page:
            (add-hook 'org-clock-in-prepare-hook
                      'my/org-mode-ask-effort)

            (defun my/org-mode-ask-effort ()
              "Ask for an effort estimate when clocking in."
              (unless (org-entry-get (point) "Effort")
                (let ((effort
                       (completing-read
                        "Effort: "
                        (org-entry-get-multivalued-property (point) "Effort"))))
                  (unless (equal effort "")
                    (org-set-property "Effort" effort)))))

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

            ;; If today is Friday, I want +fri to be next Friday. I submitted a patch for this, but I’m not on the git version yet.
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

            ;; Make it easy to mark a task as done
            (defun my/org-agenda-done (&optional arg)
              "Mark current TODO as done.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
              (interactive "P")
              (org-agenda-todo "DONE"))

            ;; Make it easy to mark a task as done and create follow-up task
            (defun my/org-agenda-mark-done-and-add-followup ()
              "Mark the current TODO as done and add another task after it.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
              (interactive)
              (org-agenda-todo "DONE")
              (org-agenda-switch-to)
              (org-capture 0 "t"))

            ;; Capture something based on the agenda
            (defun my/org-agenda-new ()
              "Create a new note or task at the current agenda item.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
              (interactive)
              (org-agenda-switch-to)
              (org-capture 0))

            ;; Sorting by date and priority
            (setq org-agenda-sorting-strategy
                  '((agenda time-up category-keep priority-down tag-up effort-up)
                    (todo user-defined-up todo-state-up priority-down effort-up)
                    (tags user-defined-up)
                    (search category-keep)))
            (setq org-agenda-cmp-user-defined 'my/org-sort-agenda-items-user-defined)
            (defun my/org-get-context (txt)
              "Find the context."
              (car (member-if
                    (lambda (item) (string-match "@" item))
                    (get-text-property 1 'tags txt))))

            (defun my/org-compare-dates (a b)
              "Return 1 if A should go after B, -1 if B should go after A, or 0 if a = b."
              (cond
               ((and (= a 0) (= b 0)) nil)
               ((= a 0) 1)
               ((= b 0) -1)
               ((> a b) 1)
               ((< a b) -1)
               (t nil)))

            (defun my/org-complete-cmp (a b)
              (let* ((state-a (or (get-text-property 1 'todo-state a) ""))
                     (state-b (or (get-text-property 1 'todo-state b) "")))
                (or
                 (if (member state-a org-done-keywords-for-agenda) 1)
                 (if (member state-b org-done-keywords-for-agenda) -1))))

            (defun my/org-date-cmp (a b)
              (let* ((sched-a (or (get-text-property 1 'org-scheduled a) 0))
                     (sched-b (or (get-text-property 1 'org-scheduled b) 0))
                     (deadline-a (or (get-text-property 1 'org-deadline a) 0))
                     (deadline-b (or (get-text-property 1 'org-deadline b) 0)))
                (or
                 (my/org-compare-dates
                  (my/org-min-date sched-a deadline-a)
                  (my/org-min-date sched-b deadline-b)))))

            (defun my/org-min-date (a b)
              "Return the smaller of A or B, except for 0."
              (funcall (if (and (> a 0) (> b 0)) 'min 'max) a b))

            (defun my/org-sort-agenda-items-user-defined (a b)
              ;; compare by deadline, then scheduled date; done tasks are listed at the very bottom
              (or
               (my/org-complete-cmp a b)
               (my/org-date-cmp a b)))

            (defun my/org-context-cmp (a b)
              "Compare CONTEXT-A and CONTEXT-B."
              (let ((context-a (my/org-get-context a))
                    (context-b (my/org-get-context b)))
                (cond
                 ((null context-a) +1)
                 ((null context-b) -1)
                 ((string< context-a context-b) -1)
                 ((string< context-b context-a) +1)
                 (t nil))))

            (defun my/org-sort-agenda-items-todo (a b)
              (or
               (org-cmp-time a b)
               (my/org-complete-cmp a b)
               (my/org-context-cmp a b)
               (my/org-date-cmp a b)
               (org-cmp-todo-state a b)
               (org-cmp-priority a b)
               (org-cmp-effort a b)))

            ;; Preventing things from falling through the cracks
            (defun my/org-agenda-list-unscheduled (&rest ignore)
              "Create agenda view for tasks that are unscheduled and not done."
              (let* ((org-agenda-todo-ignore-with-date t)
                     (org-agenda-overriding-header "List of unscheduled tasks: "))
                (org-agenda-get-todos)))
            (setq org-stuck-projects
                  '("+PROJECT-MAYBE-DONE"
                    ("TODO")
                    nil
                    "\\<IGNORE\\>"))

            ;; Weekly review
            (defun my/quantified-get-hours (category time-summary)
              "Return the number of hours based on the time summary."
              (if (stringp category)
                  (if (assoc category time-summary) (/ (cdr (assoc category time-summary)) 3600.0) 0)
                (apply '+ (mapcar (lambda (x) (my/quantified-get-hours x time-summary)) category))))
            (defun my/org-summarize-focus-areas ()
              "Summarize previous and upcoming tasks as a list."
              (interactive)
              (let ((base-date (apply 'encode-time (org-read-date-analyze "-fri" nil '(0 0 0))))
                    work relationships life work-next relationships-next life-next string start end time-summary
                    biz-time)
                (setq start (format-time-string "%Y-%m-%d" (days-to-time (- (time-to-number-of-days base-date) 6))))
                (setq end (format-time-string "%Y-%m-%d" (days-to-time (1+ (time-to-number-of-days base-date)))))
                (setq time-summary (quantified-summarize-time start end))
                (setq biz-time (my/quantified-get-hours "Work" time-summary))
                (save-window-excursion
                  (org-agenda nil "w")
                  (setq string (buffer-string))
                  (with-temp-buffer
                    (insert string)
                    (goto-char (point-min))
                    (while (re-search-forward "^  \\([^:]+\\): +\\(Sched[^:]+: +\\)?TODO \\(.*?\\)\\(?:[      ]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[        ]*$" nil t)
                      (cond
                       ((string= (match-string 1) "routines") nil) ; skip routine tasks
                       ((string= (match-string 1) "work")
                        (add-to-list 'work-next (concat "  - [ ] " (match-string 3))))
                       ((string= (match-string 1) "people")
                        (add-to-list 'relationships-next (concat "  - [ ] " (match-string 3))))
                       (t (add-to-list 'life-next (concat "  - [ ] " (match-string 3))))))))
                (save-window-excursion
                  (org-agenda nil "w")
                  (org-agenda-later -1)
                  (org-agenda-log-mode 16)
                  (setq string (buffer-string))
                  ;; Get any completed tasks from the current week as well
                  (org-agenda-later 1)
                  (org-agenda-log-mode 16)
                  (setq string (concat string "\n" (buffer-string)))
                  (with-temp-buffer
                    (insert string)
                    (goto-char (point-min))
                    (while (re-search-forward "^  \\([^:]+\\): +.*?State:.*?\\(?:TODO\\|DONE\\) \\(.*?\\)\\(?:[       ]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[        ]*$" nil t)
                      (cond
                       ((string= (match-string 1) "routines") nil) ; skip routine tasks
                       ((string= (match-string 1) "work")
                        (add-to-list 'work (concat "  - [X] " (match-string 2))))
                       ((string= (match-string 1) "people")
                        (add-to-list 'relationships (concat "  - [X] " (match-string 2))))
                       (t (add-to-list 'life (concat "  - [X] " (match-string 2))))))))
                (setq string
                      (concat
                       (format "- *Work* (%.1fh - %d%%)\n" biz-time (/ biz-time 1.68))
                       (mapconcat 'identity (sort work 'string<) "\n") "\n"
                       (mapconcat 'identity (sort work-next 'string<) "\n")
                       "\n"
                       (format "  - *Earn* (%.1fh - %d%% of Work)\n"
                               (my/quantified-get-hours "Work - Earn" time-summary)
                               (/ (my/quantified-get-hours "Work - Earn" time-summary) (* 0.01 biz-time)))
                       (format "  - *Build* (%.1fh - %d%% of Work)\n"
                               (my/quantified-get-hours "Work - Build" time-summary)
                               (/ (my/quantified-get-hours "Work - Build" time-summary) (* 0.01 biz-time)))
                       (format "    - *Coding* (%.1fh)\n"
                               (my/quantified-get-hours '("Work - Build - Coding" "Work - Build - Documentation")  time-summary))
                       (format "    - *Writing* (%.1fh)\n"
                               (my/quantified-get-hours "Work - Build - Writing" time-summary))
                       (format "    - *Research* (%.1fh)\n"
                               (my/quantified-get-hours "Work - Build - Research" time-summary))
                       (format "    - *Learning* (%.1fh)\n"
                               (my/quantified-get-hours "Work - Build - Learning"  time-summary))
                       (format "  - *Connect* (%.1fh - %d%% of Work)\n"
                               (my/quantified-get-hours "Work - Connect" time-summary)
                               (/ (my/quantified-get-hours "Work - Connect" time-summary) (* 0.01 biz-time)))
                       (format "- *Relationships* (%.1fh - %d%%)\n"
                               (my/quantified-get-hours '("Discretionary - Social" "Discretionary - Family") time-summary)
                               (/ (my/quantified-get-hours '("Discretionary - Social" "Discretionary - Family") time-summary) 1.68))
                       (mapconcat 'identity (sort relationships 'string<) "\n") "\n"
                       (mapconcat 'identity (sort relationships-next 'string<) "\n")
                       "\n"
                       (format "- *Discretionary - Productive* (%.1fh - %d%%)\n"
                               (my/quantified-get-hours "Discretionary - Productive" time-summary)
                               (/ (my/quantified-get-hours "Discretionary - Productive" time-summary) 1.68))
                       (mapconcat 'identity (sort life 'string<) "\n") "\n"
                       (mapconcat 'identity (sort life-next 'string<) "\n") "\n"
                       (format "  - *Writing* (%.1fh)\n"
                               (my/quantified-get-hours "Discretionary - Productive - Writing" time-summary))
                       (format "- *Discretionary - Play* (%.1fh - %d%%)\n"
                               (my/quantified-get-hours "Discretionary - Play" time-summary)
                               (/ (my/quantified-get-hours "Discretionary - Play" time-summary) 1.68))
                       (format "- *Personal routines* (%.1fh - %d%%)\n"
                               (my/quantified-get-hours "Personal" time-summary)
                               (/ (my/quantified-get-hours "Personal" time-summary) 1.68))
                       (format "- *Unpaid work* (%.1fh - %d%%)\n"
                               (my/quantified-get-hours "Unpaid work" time-summary)
                               (/ (my/quantified-get-hours "Unpaid work" time-summary) 1.68))
                       (format "- *Sleep* (%.1fh - %d%% - average of %.1f per day)\n"
                               (my/quantified-get-hours "Sleep" time-summary)
                               (/ (my/quantified-get-hours "Sleep" time-summary) 1.68)
                               (/ (my/quantified-get-hours "Sleep" time-summary) 7)
                               )))
                (if (called-interactively-p 'any)
                    (insert string)
                  string)))

            ;; I use this to put together a quick summary of how I spent my time.
            ;; The following code makes it easy to add a line:
            (defun my/org-add-line-item-task (task)
              (interactive "MTask: ")
              (org-insert-heading)
              (insert "[ ] " task)
              (let ((org-capture-entry '("t" "Tasks" entry
                                         (file+headline org-agenda-todo-file "Tasks")
                                         "")))
                (org-capture nil "t")
                (insert "TODO " task "\nSCHEDULED: <" (org-read-date) ">")))

            ;; Now we put it all together
            (defun my/org-prepare-weekly-review ()
              "Prepare weekly review template."
              (interactive)
              (let ((base-date (apply 'encode-time (org-read-date-analyze "-fri" nil '(0 0 0))))
                    start end)
                (setq start (format-time-string "%Y-%m-%d" (days-to-time (- (time-to-number-of-days base-date) 6))))
                (setq end (format-time-string "%Y-%m-%d" (days-to-time (1+ (time-to-number-of-days base-date)))))
                (insert
                 (concat
                  "*** Weekly review: Week ending " (format-time-string "%B %e, %Y" base-date) "  :weekly:\n"
                  "*PhD Research*\n\n"
                  "*Code Development*\n\n"
                  "*Work summary round-up*\n\n"
                  "\n\n*Focus areas and time review*\n\n"
                  (my/org-summarize-focus-areas)
                  "\n"))))

            ;; From https://sriramkswamy.github.io/dotemacs/#orgheadline16
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

(provide 'setup-org-agenda)
;;; setup-org-agenda.el ends here
