;;; setup-jj.el --- Magit-style interface for Jujutsu (jj) on Piper  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2026  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojarab@gmail.com>
;; Keywords: vc, tools

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

;; A magit-shaped front end for jj, including the jj-on-Piper bits: upload a
;; commit to Critique, show its CL, run a presubmit.
;;
;;   C-c v     `jj-menu'    -- transient menu, from anywhere in a jj repo
;;   C-c v s   `jj-status'  -- the status buffer; g refreshes, ? opens the menu
;;
;; Every read goes through `jj--read', which passes --ignore-working-copy and
;; discards stderr.  Both matter.  jj snapshots the working copy on every
;; invocation, which in a repo holding large files prints a multi-kilobyte
;; "Refused to snapshot some files" block -- 35kB of it in ~/studies -- and
;; takes the working-copy lock while doing it.  Reads that snapshot are slow,
;; noisy, and contend with each other.  The status buffer snapshots once, on
;; purpose, in `jj-status'.
;;
;; Deliberately absent: `jj piper submit'.  Landing a CL is not an operation
;; that should be one keystroke away in a transient menu, and nothing here
;; should be able to do it by accident.  `jj piper mail' is bound but asks
;; first.

;;; Code:

(require 'ansi-color)
(require 'subr-x)

(defgroup jj nil
  "Jujutsu integration."
  :group 'tools
  :prefix "jj-")

(defcustom jj-executable "jj"
  "The jj binary."
  :type 'string
  :group 'jj)

(defcustom jj-log-limit 60
  "How many revisions `jj-status' and `jj-log' show."
  :type 'integer
  :group 'jj)

(defcustom jj-log-revset nil
  "Revset for the log.  nil means jj's own default."
  :type '(choice (const :tag "jj default" nil) string)
  :group 'jj)

(defvar jj--log-template
  (concat
   "change_id.shortest(8)"
   " ++ \"\\t\""
   " ++ if(current_working_copy, \"@\", \" \")"
   " ++ if(conflict, \"x\", \" \")"
   " ++ if(empty, \"o\", \" \")"
   " ++ if(immutable, \"#\", \" \")"
   " ++ \" \" ++ pad_end(9, change_id.shortest(8))"
   " ++ pad_end(14, author.timestamp().ago())"
   " ++ pad_end(17, bookmarks.join(\" \"))"
   " ++ if(description, description.first_line(), \"(no description)\")"
   " ++ \"\\n\"")
  "Template emitting \"CHANGEID\\tDISPLAY\" per revision.

Colour is applied in Emacs rather than by jj: with --color=always jj
rewrites the ESC in a template string literal into a visible U+241B
symbol, and it also colours the change id, which has to stay plain
because it is what gets handed back to jj as a revision.")

;;; Process plumbing ---------------------------------------------------------

(defun jj-available-p ()
  "Non-nil when the jj binary exists on this machine.

Checked per call rather than at load time: this config is shared across
machines, and `executable-find' is a PATH lookup, so it costs nothing.
Everything below degrades to a single clear message when jj is absent,
instead of the raw \"Searching for program\" error from `call-process'."
  (and jj-executable (executable-find jj-executable) t))

(defun jj--assert-available ()
  "Signal a useful error unless jj is installed."
  (unless (jj-available-p)
    (user-error "jj is not installed on this machine (set `jj-executable')")))

(defun jj--root (&optional dir)
  "Return the jj workspace root containing DIR, or nil.

Pure filesystem: `locate-dominating-file' for .jj, no subprocess.  This
is called constantly -- by the mode line, the VC backend, diff-hl and
every command -- and shelling out to `jj root' each time cost 105ms a
call, six calls deep on a single file visit.  `file-truename' so the
answer matches what `jj root' would have said through the symlinked
~/jj_workspaces paths, which keeps cache keys consistent."
  (let ((hit (locate-dominating-file (or dir default-directory) ".jj")))
    (when hit
      (directory-file-name (file-truename hit)))))

(defun jj--assert-root ()
  "Return the jj root or signal a useful error."
  (jj--assert-available)
  (or (jj--root)
      (user-error "Not inside a jj workspace: %s" default-directory)))

(defun jj--read (&rest args)
  "Run a read-only jj command with ARGS and return stdout.
Adds --ignore-working-copy and throws stderr away; see the Commentary."
  (jj--assert-available)
  (let ((default-directory (or (jj--root) default-directory)))
    (with-temp-buffer
      (apply #'call-process jj-executable nil (list t nil) nil
             "--ignore-working-copy" args)
      (buffer-string))))

(defun jj--run (&rest args)
  "Run a mutating jj command with ARGS.  Return (EXIT . OUTPUT)."
  (jj--assert-available)
  (let ((default-directory (or (jj--root) default-directory)))
    (with-temp-buffer
      (let ((exit (apply #'call-process jj-executable nil (list t t) nil args)))
        (cons exit (string-trim (buffer-string)))))))

(defun jj--run-report (&rest args)
  "Run ARGS through `jj--run', echo the result, refresh the status buffer."
  (pcase-let ((`(,exit . ,out) (apply #'jj--run args)))
    (if (zerop exit)
        (message "jj %s: %s" (string-join args " ")
                 (if (string-empty-p out) "ok" (car (split-string out "\n"))))
      (message "jj %s failed: %s" (string-join args " ") out))
    (jj--refresh-status-buffers)
    ;; Any mutation can move @, so the cached mode-line string is now suspect.
    (when (fboundp 'jj-mode-line-refresh) (jj-mode-line-refresh))
    exit))

;;; Status buffer ------------------------------------------------------------

(defvar-local jj--buffer-root nil
  "The jj root this buffer was rendered from.")

(defun jj--insert-ansi (string)
  "Insert STRING, rendering its ANSI escapes."
  (let ((start (point)))
    (insert string)
    (ansi-color-apply-on-region start (point))))

(defun jj--insert-log ()
  "Insert the log, propertising each line with its change id."
  (let* ((args (append (list "log" "--no-graph" "-n"
                             (number-to-string jj-log-limit))
                       (when jj-log-revset (list "-r" jj-log-revset))
                       (list "-T" jj--log-template)))
         (out (apply #'jj--read args)))
    (dolist (line (split-string out "\n" t))
      (let* ((parts (split-string line "\t"))
             (change (car parts))
             (display (or (cadr parts) "")))
        (insert (propertize display 'jj-change change
                            'face (cond
                                   ((string-prefix-p "@" display) 'bold)
                                   ((string-match-p "\\`.\\{3\\}#" display)
                                    'shadow))))
        (insert "\n")))))

(defun jj-status-visit ()
  "Work on the change at point, or show whatever else point is on.

RET means \"show\" in magit, and that is what it did here -- selecting a
CL printed its message and left the working copy exactly where it was,
which is not what a stack browser is for.  In this buffer the dominant
action on a change is \"work on this CL\", so RET does that; `v' still
shows.  `jj-switch-commit' asks before moving @, so RET cannot move the
working copy by accident."
  (interactive)
  (let ((file (jj-file-at-point))
        (change (jj-change-at-point)))
    (cond
     ;; A changed file in the Working copy section: just open it.
     (file (find-file file))
     (change (jj-switch-commit change))
     ;; Do not fall through to an interactive prompt.  Lines in the status
     ;; section mention change descriptions without carrying a change
     ;; property, and prompting there reads as the command having hung.
     (t (message "Point is not on a file or a change")))))

(defun jj-change-at-point ()
  "The change id on the current line, or nil."
  (get-text-property (line-beginning-position) 'jj-change))

(defun jj--change-or-prompt (&optional prompt)
  "Change id at point, else read one from the minibuffer."
  (or (jj-change-at-point)
      (read-string (or prompt "Revision: ") "@")))

(defun jj--refresh-status-buffers ()
  "Re-render every live jj status buffer."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'jj-status-mode)
        (jj-status-refresh)))))

(defmacro jj--with-section (spec heading &rest body)
  "Wrap BODY in a magit section when magit is present, else just insert HEADING."
  (declare (indent 2))
  `(if jj--have-magit-section
       (magit-insert-section ,spec
         (magit-insert-heading ,heading)
         ,@body)
     (insert (propertize (concat ,heading "\n") 'face 'bold))
     ,@body))

(defconst jj--status-line-re
  "\\`\\([AMDRC]\\) \\(.+\\)\\'"
  "A `jj status' change line: a one-letter kind and a path.")

(defun jj--insert-status-lines ()
  "Insert `jj status', with each changed file carrying its path as a property.

Parsed rather than dumped through `jj--insert-ansi': the point of the
section is that you can land on a file and act on it, which needs the
path attached to the line.  Colour is applied here instead of asking jj
for it, because jj's own escapes would have to be stripped back off to
read the path."
  (let ((root jj--buffer-root))
    (dolist (line (split-string (jj--read "status") "\n"))
      (if (not (string-match jj--status-line-re line))
          (insert line "\n")
        (let* ((kind (match-string 1 line))
               (rel (match-string 2 line))
               (abs (expand-file-name rel root)))
          ;; The property goes on the whole line, newline included: point sits
          ;; at column 0 after `n'/`p', so attaching it to the path alone made
          ;; RET miss unless you had moved onto the text.
          (let ((start (point)))
            (insert kind " " rel "\n")
            (add-text-properties start (point) (list 'jj-file abs))
            (add-text-properties start (1+ start)
                                 (list 'face (pcase kind
                                               ("A" 'diff-added)
                                               ("D" 'diff-removed)
                                               (_   'diff-changed))))))))))

(defun jj-file-at-point ()
  "Absolute path of the changed file on this line, or nil."
  (or (get-text-property (point) 'jj-file)
      (get-text-property (line-beginning-position) 'jj-file)))

(defun jj--insert-stack-section ()
  "The CL stack: one foldable subsection per change, CLs called out."
  (jj--with-section (jjstack) "Stack"
    (let* ((args (append (list "log" "--no-graph" "-n"
                               (number-to-string jj-log-limit))
                         (when jj-log-revset (list "-r" jj-log-revset))
                         (list "-T" jj--log-template)))
           (out (apply #'jj--read args)))
      (dolist (line (split-string out "\n" t))
        (let* ((parts (split-string line "\t"))
               (change (string-trim (car parts)))
               (display (or (cadr parts) "")))
          (unless (string-empty-p change)
            (if jj--have-magit-section
                (magit-insert-section (jjchange change t)
                  (insert (propertize display 'jj-change change))
                  (insert "\n"))
              (insert (propertize display 'jj-change change) "\n"))))))
    (insert "\n")))

(defun jj-status-refresh ()
  "Re-render this status buffer."
  (interactive)
  (unless (derived-mode-p 'jj-status-mode)
    (user-error "Not a jj status buffer"))
  (let ((inhibit-read-only t)
        (line (line-number-at-pos))
        (default-directory jj--buffer-root))
    (erase-buffer)
    (jj--with-section (jjroot)
        (propertize (format "jj: %s" jj--buffer-root)
                    'face (if (facep 'magit-section-heading)
                              'magit-section-heading 'bold))
      (insert (propertize
               "TAB fold  g refresh  RET open file / work on change  v show  d diff  u upload  k squash\n\n"
               'face 'magit-dimmed))
      (jj--with-section (jjwc) "Working copy"
        (jj--insert-status-lines)
        (insert "\n"))
      (jj--insert-stack-section))
    (goto-char (point-min))
    (forward-line (1- line))))

(defun jj-status-next ()
  "Next section with magit, next line without it."
  (interactive)
  (if (fboundp 'magit-section-forward) (magit-section-forward) (forward-line 1)))

(defun jj-status-prev ()
  "Previous section with magit, previous line without it."
  (interactive)
  (if (fboundp 'magit-section-backward) (magit-section-backward) (forward-line -1)))

(defun jj-status-diff-dwim ()
  "Diff the file at point, or the change at point."
  (interactive)
  (let ((file (jj-file-at-point)))
    (if file
        (jj--view "*jj file diff*" "diff" "--color=always" "--"
                  (file-relative-name file (jj--assert-root)))
      (call-interactively #'jj-diff))))

(defun jj-status-toggle ()
  "Fold the section under point; a no-op without magit."
  (interactive)
  (if (fboundp 'magit-section-toggle)
      (call-interactively #'magit-section-toggle)
    (message "Section folding needs magit-section")))

(defvar jj-status-mode-map (make-sparse-keymap)
  "Keymap for `jj-status-mode'.")

;; Populated at top level with `define-key' rather than inside the `defvar'.
;; `defvar' does not re-evaluate when the variable is already bound, so a
;; keymap built inside one is frozen at whatever the first load produced --
;; every later edit to the bindings silently does nothing until Emacs restarts.
;; magit-section is optional.  Referencing `magit-section-mode-map' at top
;; level made this whole file fail to load with (void-variable
;; magit-section-mode-map) on a machine without magit -- taking `jj-dispatch'
;; and every other jj command down with it.
(when (boundp 'magit-section-mode-map)
  (set-keymap-parent jj-status-mode-map magit-section-mode-map))
(dolist (b '(("g"   . jj-status-refresh)
             ("RET" . jj-status-visit)
             ("v"   . jj-show)
             ("d"   . jj-status-diff-dwim)
             ("D"   . jj-describe)
             ("n"   . jj-status-next)
             ("p"   . jj-status-prev)
             ("N"   . jj-new)
             ("e"   . jj-edit)
             ("S"   . jj-squash)
             ("k"   . jj-stash-into-parent)
             ("K"   . jj-amend-and-reupload)
             ("u"   . jj-upload)
             ("c"   . jj-show-cl)
             ("a"   . jj-annotate)
             ("x"   . jj-abandon)
             ("l"   . jj-log)
             ("L"   . jj-stack)
             ("f"   . jj-files)
             ("M"   . jj-move-file-to-cl)
             ("]"   . jj-next-cl)
             ("["   . jj-prev-cl)
             ("TAB" . jj-status-toggle)
             ("q"   . quit-window)
             ("?"   . jj-dispatch)))
  (define-key jj-status-mode-map (kbd (car b)) (cdr b)))

(defconst jj--have-magit-section (require 'magit-section nil t)
  "Whether magit-section is available to derive the status buffer from.")

(defun jj--status-mode-setup ()
  "Shared body for both variants of `jj-status-mode'."
  (setq buffer-read-only t
        truncate-lines t)
  ;; company binds TAB in its own minor-mode map, which outranks any major
  ;; mode -- TAB here ran `company-indent-for-tab-command' instead of folding.
  ;; Turning it off in the body is not enough, `global-company-mode' switches
  ;; it back on, so `company-global-modes' carries the exclusion too.
  (when (bound-and-true-p company-mode) (company-mode -1))
  (when (bound-and-true-p jj-inline-blame-mode) (jj-inline-blame-mode -1)))

;; Two definitions rather than one with a computed parent: `define-derived-mode'
;; takes the parent as a literal symbol and fails macro-expansion on a form.
;; Only one branch is evaluated at load.  Without magit the buffer still works;
;; it just loses section folding.
(if jj--have-magit-section
    (define-derived-mode jj-status-mode magit-section-mode "jj-status"
      "Major mode for the jj status buffer, with magit-style sections."
      (jj--status-mode-setup))
  (define-derived-mode jj-status-mode special-mode "jj-status"
    "Major mode for the jj status buffer (no magit: sections do not fold)."
    (jj--status-mode-setup)))

;;;###autoload
(defun jj-status-buffer (&optional root)
  "Return the rendered status buffer for ROOT, without displaying it.

Split out of `jj-status' so a window manager -- `setup-ide-layout' keeps
this buffer in a permanent side window -- can build and refresh it
without `pop-to-buffer' yanking point out of the file being edited."
  (jj--assert-available)
  (require 'magit-section)
  (let ((root (or root (jj--assert-root))))
    ;; The one place that deliberately snapshots: everything rendered below
    ;; reads with --ignore-working-copy, so without this the view could be
    ;; stale against files just edited in Emacs.
    (let ((default-directory root))
      (call-process jj-executable nil nil nil "log" "-r" "@" "-n" "1" "-T" "\"\""))
    (with-current-buffer (get-buffer-create (format "*jj: %s*"
                                                    (file-name-nondirectory
                                                     (directory-file-name root))))
      (jj-status-mode)
      (setq jj--buffer-root root
            default-directory root)
      (jj-status-refresh)
      (current-buffer))))

;;;###autoload
(defun jj-status ()
  "Open the jj status buffer for the current workspace."
  (interactive)
  (pop-to-buffer (jj-status-buffer)))


;;; Read-only views ----------------------------------------------------------

(defun jj--view (name &rest args)
  "Render `jj ARGS' into a buffer called NAME."
  (let ((root (jj--assert-root))
        (out (apply #'jj--read args)))
    (with-current-buffer (get-buffer-create name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (jj--insert-ansi out)
        (goto-char (point-min)))
      (special-mode)
      (setq default-directory root)
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun jj-show (&optional rev)
  "Show revision REV."
  (interactive (list (jj--change-or-prompt "Show revision: ")))
  (jj--view "*jj show*" "show" "--color=always" "-r" rev))

;;;###autoload
(defun jj-diff (&optional rev)
  "Diff revision REV."
  (interactive (list (jj--change-or-prompt "Diff revision: ")))
  (jj--view "*jj diff*" "diff" "--color=always" "-r" rev))

;;;###autoload
(defun jj-log ()
  "Show the graph log."
  (interactive)
  (jj--view "*jj log*" "log" "--color=always"
            "-n" (number-to-string jj-log-limit)))

;;;###autoload
(defun jj-op-log ()
  "Show the operation log -- jj's undo history."
  (interactive)
  (jj--view "*jj op log*" "op" "log" "--color=always" "-n" "40"))

;;; Mutating commands --------------------------------------------------------

;;;###autoload
(defun jj-describe (&optional rev)
  "Edit the description of REV in a buffer."
  (interactive (list (jj--change-or-prompt "Describe revision: ")))
  (let* ((root (jj--assert-root))
         (current (string-trim
                   (jj--read "log" "--no-graph" "-r" rev "-T" "description")))
         (buf (get-buffer-create "*jj describe*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert current)
      (text-mode)
      (setq default-directory root)
      (setq-local header-line-format
                  (format "Describe %s -- C-c C-c to save, C-c C-k to cancel" rev))
      (local-set-key (kbd "C-c C-c")
                     (lambda ()
                       (interactive)
                       (let ((msg (buffer-string)))
                         (jj--run-report "describe" "-r" rev "-m" msg)
                         (quit-window t))))
      (local-set-key (kbd "C-c C-k")
                     (lambda () (interactive) (quit-window t)))
      (pop-to-buffer buf))))

;;;###autoload
(defun jj-new (&optional rev)
  "Create a new working-copy commit on top of REV."
  (interactive (list (jj--change-or-prompt "New child of: ")))
  (jj--run-report "new" rev))

;;;###autoload
(defun jj-edit (&optional rev)
  "Make REV the working-copy commit.

Asks first: moving @ mid-stack is how divergent change ids appear on
jj-on-Piper, and it repoints blaze-bin under anything already running."
  (interactive (list (jj--change-or-prompt "Edit revision: ")))
  (when (yes-or-no-p (format "jj edit %s (moves the working copy)? " rev))
    (jj--run-report "edit" rev)))

;;;###autoload
(defun jj-squash (&optional rev)
  "Squash the working copy into REV."
  (interactive (list (jj--change-or-prompt "Squash into: ")))
  (when (y-or-n-p (format "Squash @ into %s? " rev))
    (jj--run-report "squash" "--into" rev)))

;;;###autoload
(defun jj-abandon (&optional rev)
  "Abandon REV."
  (interactive (list (jj--change-or-prompt "Abandon revision: ")))
  (when (yes-or-no-p (format "Abandon %s -- this discards the commit? " rev))
    (jj--run-report "abandon" rev)))

;;;###autoload
(defun jj-undo ()
  "Undo the last jj operation."
  (interactive)
  (when (y-or-n-p "Undo the last jj operation? ")
    (jj--run-report "undo")))

;;; Piper / Critique ---------------------------------------------------------

(defun jj--cl-of (rev)
  "The Piper CL number exported for REV, or nil."
  (let ((out (jj--read "log" "--no-graph" "-r" rev
                       "-T" "bookmarks.join(\" \")")))
    (when (string-match "cl/\\([0-9]+\\)" out)
      (match-string 1 out))))

;;;###autoload
(defun jj-upload (&optional rev)
  "Export REV to Critique as a changelist (`jj piper upload').

This creates or updates a CL.  It does not mail it and does not submit
it; see `jj-mail' and the Commentary."
  (interactive (list (jj--change-or-prompt "Upload revision: ")))
  (message "Uploading %s to Critique..." rev)
  (pcase-let ((`(,exit . ,out) (jj--run "piper" "upload" "-r" rev)))
    (jj--refresh-status-buffers)
    (if (zerop exit)
        (let ((cl (or (and (string-match "cl/\\([0-9]+\\)" out)
                           (match-string 1 out))
                      (jj--cl-of rev))))
          (if cl
              (progn (kill-new (format "cl/%s" cl))
                     (message "Uploaded %s -> cl/%s (copied to kill ring)" rev cl))
            (message "Uploaded %s: %s" rev (or (car (last (split-string out "\n" t))) "ok"))))
      (message "Upload failed: %s" out))))

;;;###autoload
(defun jj-show-cl (&optional rev)
  "Open the Critique page for REV's changelist."
  (interactive (list (jj--change-or-prompt "Revision: ")))
  (let ((cl (jj--cl-of rev)))
    (if cl
        (browse-url (format "http://cl/%s" cl))
      (user-error "No CL exported for %s yet -- upload it first" rev))))

;;;###autoload
(defun jj-presubmit (&optional rev)
  "Run a Piper presubmit for REV."
  (interactive (list (jj--change-or-prompt "Presubmit revision: ")))
  (let ((default-directory (jj--assert-root)))
    (compilation-start
     (format "%s piper presubmit -r %s" jj-executable (shell-quote-argument rev))
     nil (lambda (&rest _) "*jj presubmit*"))))

;;;###autoload
(defun jj-mail (&optional rev)
  "Mail REV's CL for review.

Asks twice on purpose.  Mailing puts the change in front of reviewers,
which is not something a mistyped key should do."
  (interactive (list (jj--change-or-prompt "Mail revision: ")))
  (let ((cl (jj--cl-of rev)))
    (unless cl
      (user-error "No CL exported for %s yet -- upload it first" rev))
    (when (yes-or-no-p (format "Mail cl/%s for review? " cl))
      (let ((reviewers (read-string "Reviewers (comma separated, empty to keep existing): ")))
        (when (string-match-p "-oncall" reviewers)
          (user-error "Refusing to add an -oncall alias as a reviewer"))
        (when (yes-or-no-p (format "Really mail cl/%s%s? " cl
                                   (if (string-empty-p reviewers) ""
                                     (format " to %s" reviewers))))
          (apply #'jj--run-report
                 (append (list "piper" "mail" "-r" rev)
                         (unless (string-empty-p reviewers)
                           (list "--reviewers" reviewers)))))))))

(defun jj--uploaded-p (rev)
  "Non-nil if REV already has a CL."
  (and (jj--cl-of rev) t))

;;;###autoload
(defun jj-stash-into-parent ()
  "Squash the working copy into its parent, the commit below it in the stack.

The common move when you are iterating on a CL: you are sitting on an
empty or scratch @ above the change you are actually editing, and you
want the edits folded down into it."
  (interactive)
  (jj--assert-available)
  (let* ((parent (string-trim (jj--read "log" "--no-graph" "-r" "@-"
                                        "-T" "change_id.shortest(8)")))
         (desc (string-trim (jj--read "log" "--no-graph" "-r" "@-"
                                      "-T" "description.first_line()")))
         (cl (jj--cl-of "@-")))
    (when (string-empty-p parent)
      (user-error "No parent commit to squash into"))
    (when (y-or-n-p (format "Squash @ into %s%s (%s)? " parent
                            (if cl (format " [cl/%s]" cl) "")
                            (if (string-empty-p desc) "no description" desc)))
      (when (zerop (jj--run-report "squash" "--into" "@-"))
        (when (and cl (y-or-n-p (format "Re-upload cl/%s to Critique? " cl)))
          (jj-upload "@-"))))))

;;;###autoload
(defun jj-reupload (&optional rev)
  "Re-export REV to Critique, updating the CL it already has.

`jj-upload' does the same thing; this exists so the intent reads
correctly in the menu, and it refuses when there is no CL yet so you do
not silently create one where you meant to update."
  (interactive (list (jj--change-or-prompt "Re-upload revision: ")))
  (let ((cl (jj--cl-of rev)))
    (unless cl
      (user-error "%s has no CL yet -- use jj-upload to create one" rev))
    (jj-upload rev)))

;;;###autoload
(defun jj-amend-and-reupload ()
  "Fold @ into its parent and push the result back to the same CL.

The whole edit-review-edit loop in one command: squash down, then
re-upload if the parent is already on Critique."
  (interactive)
  (jj--assert-available)
  (let ((cl (jj--cl-of "@-")))
    (unless cl
      (user-error "The parent has no CL; use `jj-stash-into-parent' instead"))
    (when (y-or-n-p (format "Fold @ into cl/%s and re-upload? " cl))
      (when (zerop (jj--run-report "squash" "--into" "@-"))
        (jj-upload "@-")))))

;;;###autoload
(defun jj-describe-and-reupload (&optional rev)
  "Edit REV's description, then push it to Critique."
  (interactive (list (jj--change-or-prompt "Describe and re-upload: ")))
  (let ((current (string-trim
                  (jj--read "log" "--no-graph" "-r" rev "-T" "description"))))
    (let ((msg (read-string "Description: " current)))
      (when (and msg (not (string= msg current)))
        (when (zerop (jj--run-report "describe" "-r" rev "-m" msg))
          (when (jj--uploaded-p rev)
            (jj-upload rev)))))))

;;; Completion: helm, counsel/ivy, vertico -----------------------------------

;; One `completing-read' rather than three integrations.  helm-mode, ivy-mode
;; and vertico all take over `completing-read', so this is automatically a
;; helm buffer under helm and an ivy minibuffer under ivy, and it keeps
;; working if you change front end.  `helm-jj' and `counsel-jj' below are
;; aliases, for discoverability and M-x.

(defun jj--candidates (&optional limit)
  "Alist of (DISPLAY . CHANGEID) for the log."
  (let* ((n (number-to-string (or limit jj-log-limit)))
         (args (append (list "log" "--no-graph" "-n" n)
                       (when jj-log-revset (list "-r" jj-log-revset))
                       (list "-T" jj--log-template)))
         (out (apply #'jj--read args))
         cands)
    (dolist (line (split-string out "\n" t) (nreverse cands))
      (let ((parts (split-string line "\t")))
        (when (cadr parts)
          ;; Strip the ANSI the template emits; completion frameworks match
          ;; against the raw string and escapes would poison the match.
          (push (cons (string-trim
                       (replace-regexp-in-string "\033\\[[0-9;]*m" "" (cadr parts)))
                      (string-trim (car parts)))
                cands))))))

(defun jj-read-change (&optional prompt)
  "Pick a change with completion.  Returns a change id."
  (jj--assert-available)
  (let* ((cands (jj--candidates))
         (choice (completing-read (or prompt "Change: ")
                                  (mapcar #'car cands) nil t)))
    (or (cdr (assoc choice cands)) choice)))

;;;###autoload
(defun jj-switch-commit (&optional rev)
  "Pick a change and make it the working copy (`jj edit').

This is the \"which commit am I on\" switcher.  It asks before moving
@: on jj-on-Piper that is how divergent change ids appear, and it
repoints blaze-bin under anything already building."
  (interactive)
  (let ((rev (or rev (jj-read-change "Work on change: "))))
    (when (yes-or-no-p (format "Make %s the working copy (jj edit)? " rev))
      (jj--run-report "edit" rev)
      (jj-mode-line-refresh)
      (message "Now working on %s" rev))))

;;;###autoload
(defun jj-browse-commit ()
  "Pick a change with completion and show it."
  (interactive)
  (jj-show (jj-read-change "Show change: ")))

;;;###autoload
(defun jj-upload-pick ()
  "Pick a change with completion and upload it to Critique."
  (interactive)
  (jj-upload (jj-read-change "Upload change: ")))

;;;###autoload
(defalias 'helm-jj #'jj-switch-commit
  "Pick a jj change (renders as a helm buffer when helm-mode is on).")
;;;###autoload
(defalias 'counsel-jj #'jj-switch-commit
  "Pick a jj change (renders as an ivy minibuffer when ivy-mode is on).")

;; A native helm source as well, for helm users who want actions on the
;; candidate rather than a single default action.
(with-eval-after-load 'helm
  (defvar jj-helm-source
    (helm-build-sync-source "jj changes"
      :candidates #'jj--candidates
      :action (list (cons "Show"                (lambda (c) (jj-show c)))
                    (cons "Diff"                (lambda (c) (jj-diff c)))
                    (cons "Work on it (jj edit)" (lambda (c) (jj-switch-commit c)))
                    (cons "Describe"            (lambda (c) (jj-describe c)))
                    (cons "Upload to Critique"  (lambda (c) (jj-upload c)))
                    (cons "Open CL"             (lambda (c) (jj-show-cl c)))))
    "Helm source over the jj log.")

  (defun helm-jj-changes ()
    "Browse jj changes with helm, with an action menu."
    (interactive)
    (jj--assert-available)
    (helm :sources 'jj-helm-source :buffer "*helm jj*")))


;;; Files in the current change ------------------------------------------------

;; Browsing the whole workspace is rarely what you want mid-stack -- a CitC
;; workspace is all of google3, and projectile is deliberately stopped from
;; enumerating it.  What you almost always want is "the files this CL touches",
;; which jj answers directly and instantly.

(defun jj--files-in (&optional rev)
  "Relative paths touched by REV (default @)."
  (let ((out (jj--read "diff" "-r" (or rev "@") "--name-only")))
    (split-string out "\n" t)))

(defun jj--file-candidates (&optional rev)
  "(DISPLAY . ABSOLUTE-PATH) for the files REV touches."
  (let ((root (jj--assert-root)))
    (mapcar (lambda (rel)
              (cons rel (expand-file-name rel root)))
            (jj--files-in rev))))

;;;###autoload
(defun jj-files (&optional rev)
  "Open a file from the ones REV touches (default: the current commit).

With a prefix argument, pick the change first, so you can browse the
files of any CL in the stack without moving the working copy."
  (interactive
   (list (if current-prefix-arg (jj-read-change "Files of change: ") "@")))
  (jj--assert-available)
  (let ((cands (jj--file-candidates rev)))
    (unless cands
      (user-error "%s touches no files" (or rev "@")))
    (if (fboundp 'ivy-read)
        (ivy-read (format "file in %s: " (or rev "@")) cands
                  :require-match t
                  :caller 'jj-files
                  :action (lambda (c) (find-file (cdr c))))
      (let ((choice (completing-read (format "file in %s: " (or rev "@"))
                                     (mapcar #'car cands) nil t)))
        (find-file (cdr (assoc choice cands)))))))

(with-eval-after-load 'ivy
  (ivy-set-actions
   'jj-files
   '(("d" (lambda (c) (jj--view "*jj file diff*" "diff" "--color=always"
                                "-r" "@" "--" (car c)))
      "diff this file")
     ("m" (lambda (c) (jj-move-file-to-cl (cdr c))) "move to another CL"))))

(with-eval-after-load 'helm
  (defvar jj-helm-files-source
    (helm-build-sync-source "files in this change"
      :candidates (lambda () (jj--file-candidates "@"))
      :action (list (cons "Open" #'find-file)
                    (cons "Move change to another CL" #'jj-move-file-to-cl)))
    "Helm source over the files the current change touches.")

  (defun helm-jj-files ()
    "Open a file from the current change, with helm."
    (interactive)
    (jj--assert-available)
    (helm :sources 'jj-helm-files-source :buffer "*helm jj files*")))

;;; Moving one file's changes down the stack -------------------------------------

;;;###autoload
(defun jj-move-file-to-cl (&optional file target)
  "Move FILE's changes out of the working copy and into TARGET.

The everyday mistake this fixes: you are sitting on CL5, you edit a file
that really belongs to CL2, and you want that edit moved down without
disturbing anything else in CL5.  `jj squash' takes paths, so only this
file moves -- verified: squashing one path out of a two-file change left
the other file untouched in the source.

Descendants are rebased, which jj does automatically.  It is recorded as
one operation, so `jj-undo' (or `jj undo') puts it back."
  (interactive)
  (jj--assert-available)
  (let* ((root (jj--assert-root))
         (file (or file buffer-file-name
                   (user-error "No file -- call this from a file buffer or pick one")))
         (rel (file-relative-name (file-truename file) (file-truename root)))
         (target (or target (jj-read-change (format "Move %s into which change? " rel)))))
    (when (string-prefix-p ".." rel)
      (user-error "%s is outside this workspace" file))
    (when (y-or-n-p (format "Move %s's changes from @ into %s? " rel target))
      (pcase-let ((`(,exit . ,out) (jj--run "squash" "--from" "@" "--into" target rel)))
        (jj--refresh-status-buffers)
        (when (fboundp 'jj-mode-line-refresh) (jj-mode-line-refresh))
        (if (zerop exit)
            (progn
              (message "Moved %s into %s -- jj-undo reverts this" rel target)
              (when (and buffer-file-name
                         (string= (file-truename buffer-file-name) (file-truename file)))
                (revert-buffer t t t))
              (when (y-or-n-p (format "Re-upload %s to Critique? " target))
                (jj-upload target)))
          (message "Move failed: %s" out))))))

;;;###autoload
(defun jj-move-file-to-parent ()
  "Move this file's changes into the parent commit (the CL below)."
  (interactive)
  (jj-move-file-to-cl nil "@-"))

;;; Traversing the CL stack ----------------------------------------------------

(defun jj--stack-candidates ()
  "(DISPLAY . CHANGEID) for the stack, CL numbers to the front.

Only changes that carry a cl/NNNN bookmark, plus the working copy, so
this is the list of things actually under review rather than the whole
log."
  (let* ((out (jj--read "log" "--no-graph" "-n" (number-to-string jj-log-limit)
                        "-T" (concat "change_id.shortest(8) ++ \"\\t\""
                                     " ++ bookmarks.join(\" \") ++ \"\\t\""
                                     " ++ if(current_working_copy, \"@\", \" \") ++ \"\\t\""
                                     " ++ if(description, description.first_line(), \"(no description)\")"
                                     " ++ \"\\n\"")))
         cands)
    (dolist (line (split-string out "\n" t) (nreverse cands))
      (pcase (split-string line "\t")
        (`(,change ,marks ,wc ,desc)
         (let ((cl (and (string-match "cl/\\([0-9]+\\)" marks)
                        (match-string 1 marks))))
           (when (or cl (string= wc "@"))
             (push (cons (format "%-2s %-13s %-9s %s"
                                 wc
                                 (if cl (concat "cl/" cl) "")
                                 (string-trim change)
                                 desc)
                         (string-trim change))
                   cands))))))))

(defun jj--stack-act (change action)
  "Run ACTION on CHANGE.  Shared by every front end."
  (pcase action
    ('edit    (jj-switch-commit change))
    ('show    (jj-show change))
    ('diff    (jj-diff change))
    ('upload  (jj-upload change))
    ('cl      (jj-show-cl change))
    ('describe (jj-describe-and-reupload change))
    ('files   (jj--view "*jj files*" "diff" "-r" change "--summary"))
    (_        (jj-show change))))

;;;###autoload
(defun jj-stack ()
  "Traverse the CL stack.

Uses ivy when it is available, so \\<ivy-minibuffer-map>\\[ivy-dispatching-done] \
offers the actions below; otherwise plain completion, which helm-mode and
vertico pick up just as well."
  (interactive)
  (jj--assert-available)
  (let ((cands (jj--stack-candidates)))
    (unless cands (user-error "No CLs in this stack yet"))
    (if (fboundp 'ivy-read)
        (ivy-read "CL stack: " cands
                  :require-match t
                  :caller 'jj-stack
                  :action (lambda (c) (jj--stack-act (cdr c) 'show)))
      (let ((choice (completing-read "CL stack: " (mapcar #'car cands) nil t)))
        (jj--stack-act (cdr (assoc choice cands)) 'show)))))

(with-eval-after-load 'ivy
  (ivy-set-actions
   'jj-stack
   '(("e" (lambda (c) (jj--stack-act (cdr c) 'edit))     "work on it (jj edit)")
     ("d" (lambda (c) (jj--stack-act (cdr c) 'diff))     "diff")
     ("f" (lambda (c) (jj--stack-act (cdr c) 'files))    "files changed")
     ("u" (lambda (c) (jj--stack-act (cdr c) 'upload))   "upload to Critique")
     ("c" (lambda (c) (jj--stack-act (cdr c) 'cl))       "open CL")
     ("D" (lambda (c) (jj--stack-act (cdr c) 'describe)) "describe + reupload")))
  ;; Same actions on the plain change picker.
  (ivy-set-actions
   'jj-read-change
   '(("e" (lambda (c) (jj--stack-act (cdr c) 'edit))   "work on it")
     ("d" (lambda (c) (jj--stack-act (cdr c) 'diff))   "diff")
     ("u" (lambda (c) (jj--stack-act (cdr c) 'upload)) "upload"))))

(with-eval-after-load 'helm
  (defvar jj-helm-stack-source
    (helm-build-sync-source "jj CL stack"
      :candidates #'jj--stack-candidates
      :action (list (cons "Show"                (lambda (c) (jj--stack-act c 'show)))
                    (cons "Work on it (jj edit)" (lambda (c) (jj--stack-act c 'edit)))
                    (cons "Diff"                (lambda (c) (jj--stack-act c 'diff)))
                    (cons "Files changed"       (lambda (c) (jj--stack-act c 'files)))
                    (cons "Upload to Critique"  (lambda (c) (jj--stack-act c 'upload)))
                    (cons "Open CL"             (lambda (c) (jj--stack-act c 'cl)))
                    (cons "Describe + reupload" (lambda (c) (jj--stack-act c 'describe)))))
    "Helm source over the CL stack.")

  (defun helm-jj-stack ()
    "Traverse the CL stack with helm."
    (interactive)
    (jj--assert-available)
    (helm :sources 'jj-helm-stack-source :buffer "*helm jj stack*")))

;;;###autoload
(defun jj-next-cl ()
  "Move the working copy to the next CL up the stack."
  (interactive)
  (jj--assert-available)
  (let ((next (string-trim (jj--read "log" "--no-graph" "-r" "@+"
                                     "-T" "change_id.shortest(8)"))))
    (if (string-empty-p next)
        (user-error "Already at the top of the stack")
      (jj-switch-commit next))))

;;;###autoload
(defun jj-prev-cl ()
  "Move the working copy to the previous CL down the stack."
  (interactive)
  (jj--assert-available)
  (let ((prev (string-trim (jj--read "log" "--no-graph" "-r" "@-"
                                     "-T" "change_id.shortest(8)"))))
    (if (string-empty-p prev)
        (user-error "Already at the bottom of the stack")
      (jj-switch-commit prev))))

;;; Workspaces: one jj workspace per CL stack ----------------------------------

;; `jjd' switches between workspaces.  Two things make it usable rather than a
;; wall of directory names:
;;
;;   * Only directories that actually contain a .jj are offered.  Of the 107
;;     entries under ~/jj_workspaces here, 20 are real workspaces; the rest are
;;     leftovers and were pure noise in the list.
;;   * Most recently touched first, which is almost always the ordering you
;;     want and costs one stat per entry (40ms for all 107).
;;
;; The CL and description shown against each workspace come from a cache that
;; is warmed in the background.  Reading them synchronously costs ~236ms per
;; workspace -- about five seconds for twenty -- so the first `jjd' shows plain
;; names and fills in as the answers arrive; later ones are annotated
;; immediately.  `C-u jjd' re-warms.

(defcustom jj-workspaces-dir "~/jj_workspaces"
  "Directory holding jj workspaces, one per CL stack."
  :type 'directory
  :group 'jj)

(defvar jj--workspace-cache (make-hash-table :test 'equal)
  "Workspace path -> annotation string.")

(defvar jj--workspace-warm-process nil)

(defcustom jj-workspace-name-regexp "\\`\\(b\\|ajb\\)"
  "Only directory names matching this are considered jj workspaces.

The name filter runs before any filesystem access, and that ordering is
the whole point.  These directories are CitC mounts, so `file-exists-p'
on \".jj\" inside one is a network stat: probing all 107 entries under
~/jj_workspaces took 9.7 seconds, which made the switcher unusable.
Matching the 16 names that can possibly be workspaces first, and
stat-ing only those, takes 14ms."
  :type 'regexp
  :group 'jj)

(defun jj--workspaces ()
  "(NAME . PATH) for every real jj workspace, most recently touched first."
  (let ((dir (expand-file-name jj-workspaces-dir)))
    (when (file-directory-p dir)
      (let (found)
        (dolist (name (directory-files dir nil jj-workspace-name-regexp))
          (let ((path (file-name-as-directory (expand-file-name name dir))))
            (when (file-exists-p (expand-file-name ".jj" path))
              (push (list name path
                          (or (file-attribute-modification-time
                               (file-attributes path))
                              0))
                    found))))
        (mapcar (lambda (e) (cons (nth 0 e) (nth 1 e)))
                (sort found (lambda (a b) (time-less-p (nth 2 b) (nth 2 a)))))))))

(defun jj--workspace-warm-cache (&optional force)
  "Fill `jj--workspace-cache' in the background.

One shell process walks every workspace rather than one process each:
the cost is dominated by jj start-up, and twenty of those serialised in
Emacs would block the UI for seconds."
  (when (and (or force (zerop (hash-table-count jj--workspace-cache)))
             (not (process-live-p jj--workspace-warm-process)))
    (let* ((ws (jj--workspaces))
           (script (mapconcat
                    (lambda (w)
                      (format "printf '%%s\\t' %s; %s --ignore-working-copy -R %s log --no-graph -r @ -T 'bookmarks.join(\" \") ++ \"  \" ++ if(description, description.first_line(), \"(no description)\")' 2>/dev/null | head -1; echo"
                              (shell-quote-argument (cdr w))
                              jj-executable
                              (shell-quote-argument (cdr w))))
                    ws "; ")))
      (when ws
        (setq jj--workspace-warm-process
              (make-process
               :name "jj-workspace-warm"
               :buffer (generate-new-buffer " *jj-ws-warm*")
               :noquery t
               :command (list "bash" "-c" script)
               :sentinel
               (lambda (proc _event)
                 (when (memq (process-status proc) '(exit signal))
                   (with-current-buffer (process-buffer proc)
                     (dolist (line (split-string (buffer-string) "\n" t))
                       (let ((parts (split-string line "\t")))
                         (when (cadr parts)
                           (puthash (car parts) (string-trim (cadr parts))
                                    jj--workspace-cache)))))
                   (kill-buffer (process-buffer proc)))))))))
  jj--workspace-cache)

(defun jj--workspace-candidates ()
  "(DISPLAY . PATH) for every workspace, annotated from cache."
  (mapcar (lambda (w)
            (let* ((note (gethash (cdr w) jj--workspace-cache))
                   (here (and (jj--root)
                              (string= (file-truename (cdr w))
                                       (file-name-as-directory (file-truename (jj--root)))))))
              (cons (format "%-2s %-34s %s"
                            (if here "*" "")
                            (car w)
                            (or note ""))
                    (cdr w))))
          (jj--workspaces)))

(defun jj--workspace-open (path &optional action)
  "Open workspace PATH.  ACTION is `status', `dired', `files' or `stack'."
  (let ((default-directory (file-name-as-directory path)))
    (when (fboundp 'projectile-add-known-project)
      (projectile-add-known-project default-directory))
    (pcase (or action 'status)
      ('dired (dired default-directory))
      ('files (cond ((fboundp 'counsel-projectile-find-file) (counsel-projectile-find-file))
                    ((fboundp 'helm-projectile-find-file) (helm-projectile-find-file))
                    ((fboundp 'projectile-find-file) (projectile-find-file))
                    (t (dired default-directory))))
      ('stack (jj-stack))
      (_ (jj-status)))))

;;;###autoload
(defun jj-switch-workspace (&optional refresh)
  "Switch to another jj workspace, i.e. another CL stack.

Each workspace is its own stack; `jj-stack' moves within one.  With a
prefix argument REFRESH, re-read the CL annotations."
  (interactive "P")
  ;; Fail fast: listing workspaces only needs the filesystem, but every action
  ;; on one needs jj, so prompting first would just waste the choice.
  (jj--assert-available)
  (jj--workspace-warm-cache refresh)
  (let ((cands (jj--workspace-candidates)))
    (unless cands
      (user-error "No jj workspaces under %s" jj-workspaces-dir))
    (if (fboundp 'ivy-read)
        (ivy-read "jj workspace: " cands
                  :require-match t
                  :caller 'jj-switch-workspace
                  :action (lambda (c) (jj--workspace-open (cdr c) 'status)))
      (let ((choice (completing-read "jj workspace: " (mapcar #'car cands) nil t)))
        (jj--workspace-open (cdr (assoc choice cands)) 'status)))))

;;;###autoload
(defalias 'jjd #'jj-switch-workspace
  "Switch jj workspace directory (the CL stack you are working on).")

(with-eval-after-load 'ivy
  (ivy-set-actions
   'jj-switch-workspace
   '(("d" (lambda (c) (jj--workspace-open (cdr c) 'dired))  "dired")
     ("f" (lambda (c) (jj--workspace-open (cdr c) 'files))  "find file")
     ("L" (lambda (c) (jj--workspace-open (cdr c) 'stack))  "CL stack")
     ("s" (lambda (c) (jj--workspace-open (cdr c) 'status)) "jj status"))))

(with-eval-after-load 'helm
  (defvar jj-helm-workspace-source
    (helm-build-sync-source "jj workspaces"
      :candidates (lambda () (jj--workspace-candidates))
      :action (list (cons "jj status" (lambda (p) (jj--workspace-open p 'status)))
                    (cons "Find file" (lambda (p) (jj--workspace-open p 'files)))
                    (cons "CL stack"  (lambda (p) (jj--workspace-open p 'stack)))
                    (cons "Dired"     (lambda (p) (jj--workspace-open p 'dired)))))
    "Helm source over jj workspaces.")

  (defun helm-jjd ()
    "Switch jj workspace with helm."
    (interactive)
    (jj--workspace-warm-cache)
    (helm :sources 'jj-helm-workspace-source :buffer "*helm jj workspaces*")))

;;;###autoload
(defun jj-projectile-register-workspaces ()
  "Add every jj workspace to projectile's known projects."
  (interactive)
  (let ((n 0))
    (dolist (w (jj--workspaces))
      (when (fboundp 'projectile-add-known-project)
        (projectile-add-known-project (cdr w))
        (setq n (1+ n))))
    (message "Registered %d jj workspace(s) with projectile" n)))

;;; The menu, as a completing-read (helm / ivy / vertico) --------------------

;; The transient is the discoverable front end; this is the same set as a
;; flat, searchable list, because "I know roughly what it is called" is a
;; different question from "show me what exists".  One `completing-read', so
;; helm-mode, ivy and vertico each render it natively rather than needing
;; three integrations.

(defconst jj-command-table
  '(("status: open the jj status buffer"            . jj-status)
    ("stack: traverse the CL stack"                 . jj-stack)
    ("stack: next CL up"                            . jj-next-cl)
    ("stack: previous CL down"                      . jj-prev-cl)
    ("stack: switch workspace (jjd)"                . jj-switch-workspace)
    ("stack: register workspaces with projectile"   . jj-projectile-register-workspaces)
    ("work on a change (jj edit)"                   . jj-switch-commit)
    ("files: open a file from this change"          . jj-files)
    ("files: move this file's change to another CL" . jj-move-file-to-cl)
    ("files: move this file's change to the parent" . jj-move-file-to-parent)
    ("browse changes"                               . jj-browse-commit)
    ("log"                                          . jj-log)
    ("operation log"                                . jj-op-log)
    ("diff a change"                                . jj-diff)
    ("show a change"                                . jj-show)
    ("describe a change"                            . jj-describe)
    ("new child commit"                             . jj-new)
    ("squash into parent"                           . jj-stash-into-parent)
    ("fold @ into parent and re-upload"             . jj-amend-and-reupload)
    ("describe and re-upload"                       . jj-describe-and-reupload)
    ("upload to Critique"                           . jj-upload)
    ("re-upload (needs an existing CL)"             . jj-reupload)
    ("open the CL in Critique"                      . jj-show-cl)
    ("presubmit"                                    . jj-presubmit)
    ("mail for review (asks twice)"                 . jj-mail)
    ("abandon a change (asks)"                      . jj-abandon)
    ("undo the last jj operation"                   . jj-undo)
    ("blame: annotate buffer (vc-annotate)"         . jj-annotate)
    ("blame: this line"                             . jj-blame-line)
    ("blame: toggle inline"                         . jj-inline-blame-mode)
    ("refresh the mode line"                        . jj-mode-line-refresh))
  "(DESCRIPTION . COMMAND) for everything on the jj menu.")

;;;###autoload
(defun jj-commands ()
  "Pick a jj command by name."
  (interactive)
  (jj--assert-available)
  (let* ((choice (completing-read "jj: " (mapcar #'car jj-command-table) nil t))
         (cmd (cdr (assoc choice jj-command-table))))
    (when cmd (call-interactively cmd))))

;;;###autoload
(defalias 'counsel-jj-commands #'jj-commands
  "Pick a jj command (an ivy minibuffer when ivy-mode is on).")

(with-eval-after-load 'helm
  (defvar jj-helm-command-source
    (helm-build-sync-source "jj commands"
      :candidates (lambda () jj-command-table)
      :action (list (cons "Run" #'call-interactively)))
    "Helm source over `jj-command-table'.")

  (defun helm-jj-commands ()
    "Pick a jj command with helm."
    (interactive)
    (jj--assert-available)
    (helm :sources 'jj-helm-command-source :buffer "*helm jj commands*"))

  ;; One helm entry point for all three: commands, the stack, and raw changes.
  (defun helm-jj-everything ()
    "Everything jj, in one helm buffer."
    (interactive)
    (jj--assert-available)
    (helm :sources '(jj-helm-command-source jj-helm-stack-source jj-helm-source)
          :buffer "*helm jj*")))

;;; Mode line ----------------------------------------------------------------

;; The mode line is redrawn constantly, so it must never shell out.  The
;; string is computed only when something actually changes -- visiting a file,
;; finishing a jj command, or asking for it -- and cached per workspace root;
;; redisplay does a hash lookup and nothing else.

(defcustom jj-mode-line-enabled t
  "Whether to show the current jj change in the mode line."
  :type 'boolean
  :group 'jj)

(defvar jj--mode-line-cache (make-hash-table :test 'equal)
  "Workspace root -> mode-line string.")

(defun jj--cache-key (root)
  "Normalise ROOT for use as a `jj--mode-line-cache' key.

`jj root' returns no trailing slash and `locate-dominating-file' adds
one, so writer and reader disagreed and every lookup missed -- which
quietly turned the cache off and put a jj subprocess on the path of
every single file you opened."
  (and root (file-name-as-directory (expand-file-name root))))

(defvar-local jj--mode-line-root nil
  "Cached workspace root for this buffer, or `none'.")

(defun jj--buffer-root ()
  "This buffer's jj root, cached; `none' when there is not one."
  (when (eq jj--mode-line-root nil)
    (setq jj--mode-line-root (or (and (jj-available-p) (jj--root)) 'none)))
  (and (stringp jj--mode-line-root) jj--mode-line-root))

(defun jj-mode-line-refresh (&optional root)
  "Recompute the cached mode-line string for ROOT."
  (interactive)
  (let ((root (or root (jj--buffer-root))))
    (when root
      (let* ((out (jj--read "log" "--no-graph" "-r" "@" "-T"
                            (concat "change_id.shortest(8)"
                                    " ++ \"\\t\" ++ bookmarks.join(\" \")"
                                    " ++ \"\\t\" ++ if(conflict, \"!\", \"\")"
                                    " ++ if(empty, \"e\", \"\")")))
             (parts (split-string (string-trim out) "\t"))
             (change (or (nth 0 parts) "?"))
             (cl (and (nth 1 parts)
                      (string-match "cl/\\([0-9]+\\)" (nth 1 parts))
                      (match-string 1 (nth 1 parts))))
             (flags (or (nth 2 parts) "")))
        (puthash (jj--cache-key root)
                 (format " jj:%s%s%s"
                         (if cl (concat "cl/" cl) change)
                         (if cl (format "(%s)" change) "")
                         (if (string-empty-p flags) "" (concat " " flags)))
                 jj--mode-line-cache)))
    (force-mode-line-update t)))

(defun jj-mode-line-string ()
  "Cached mode-line text for the current buffer.  Never shells out.

Stays quiet when the VC backend is registered, since `vc-mode' is then
already showing the same thing in the standard mode-line slot and two
copies of it is just noise."
  (when (and jj-mode-line-enabled
             (not (and jj-register-vc-backend
                       (bound-and-true-p vc-mode))))
    (let ((root (and (stringp jj--mode-line-root) jj--mode-line-root)))
      (or (and root (gethash (jj--cache-key root) jj--mode-line-cache)) ""))))

(defun jj--mode-line-setup ()
  "Populate the cache for a newly visited file, once per root."
  (when (and jj-mode-line-enabled buffer-file-name)
    (let ((root (jj--buffer-root)))
      (when (and root (not (gethash (jj--cache-key root) jj--mode-line-cache)))
        (jj-mode-line-refresh root)))))

(add-hook 'find-file-hook #'jj--mode-line-setup)

;; `global-mode-string' is honoured by the stock mode line and by every custom
;; one worth using, so this shows up without touching setup-modeline.el.
(unless (member '(:eval (jj-mode-line-string)) global-mode-string)
  (setq global-mode-string
        (append global-mode-string '((:eval (jj-mode-line-string))))))

;;; Emacs VC backend ---------------------------------------------------------

;; A deliberately small VC backend: enough that `vc-mode' knows which change
;; you are on, not a reimplementation of jj.  `jj-menu' stays the real
;; interface.
;;
;; The hard constraint is that `vc-registered' runs on *every* file you visit,
;; for every backend in `vc-handled-backends'.  So the registration test is
;; pure filesystem -- `locate-dominating-file' for .jj, no subprocess -- and
;; the revision lookup is served from the same per-root cache the mode line
;; uses.  A backend that shells out per file would make opening anything in a
;; CitC tree crawl.

(defun vc-jj-registered (file)
  "Non-nil if FILE is inside a jj workspace.  No subprocess."
  (and (stringp file)
       (locate-dominating-file file ".jj")
       t))

(defun vc-jj-state (_file)
  "Report FILE as edited; jj has no staging area to distinguish."
  'edited)

(defun vc-jj-working-revision (file)
  "The current change id, from cache where possible."
  (let ((root (and (stringp file) (locate-dominating-file file ".jj"))))
    (if (not root)
        "unknown"
      (let* ((cached (gethash (jj--cache-key root) jj--mode-line-cache)))
        (if cached
            (string-trim (replace-regexp-in-string "\\` *jj:" "" cached))
          (let ((default-directory root))
            (string-trim (jj--read "log" "--no-graph" "-r" "@"
                                   "-T" "change_id.shortest(8)"))))))))

(defun vc-jj-revision-granularity () 'repository)
(defun vc-jj-checkout-model (_files) 'implicit)
(defun vc-jj-mode-line-string (file)
  "Mode-line text for FILE: the CL when there is one, else the change id."
  (let* ((root (locate-dominating-file file ".jj"))
         (cached (and root (gethash (jj--cache-key root) jj--mode-line-cache))))
    (if cached
        (string-trim cached)
      (concat "jj:" (vc-jj-working-revision file)))))

;; VC resolves a backend's functions by `require'ing vc-BACKEND, so putting JJ
;; in `vc-handled-backends' makes it look for a vc-jj library.  There is not
;; one -- the functions live here -- and without this EVERY file visit fails
;; with "Cannot open load file: vc-jj", in any repo, because `vc-registered'
;; consults every backend in the list.  Declaring the feature satisfies
;; `require' out of `features' without touching the filesystem.
(provide 'vc-jj)

;;; Fringe indicators via diff-hl --------------------------------------------

;; diff-hl asks VC for the diff -- `(vc-call-backend BACKEND 'diff (list file)
;; diff-hl-reference-revision nil buffer)' -- so implementing one function
;; gives jj buffers the same +/- fringe marks git buffers get, with no
;; diff-hl patching at all.
;;
;; A nil base means "the previous commit in the stack", which for jj is `@-\':
;; the parent of the working copy.  That is the diffbase you care about on a
;; stacked-CL workflow, not some merge-base against trunk.

(defcustom jj-diff-snapshot t
  "Snapshot the working copy before computing a fringe diff.

On means the marks reflect what is on disk, at the cost of ~90ms per
update (measured: 0.29s with, 0.20s without).  Off is faster but the
marks lag behind the buffer until something else snapshots."
  :type 'boolean
  :group 'jj)

(defun vc-jj-diff (files &optional rev1 rev2 buffer _async)
  "Insert a diff of FILES between REV1 and REV2 into BUFFER.
Returns 0 when there is no difference, 1 when there is -- the contract
`vc-call-backend' expects."
  (let* ((buffer (get-buffer-create (or buffer "*vc-diff*")))
         (first (car files))
         (root (or (jj--root (file-name-directory first))
                   (file-name-directory first)))
         (rels (mapcar (lambda (f) (file-relative-name f root)) files))
         (from (or rev1 "@-"))
         (args (append (unless jj-diff-snapshot (list "--ignore-working-copy"))
                       (list "--no-pager" "diff" "--git" "--from" from)
                       (when rev2 (list "--to" rev2))
                       (list "--")
                       rels)))
    (with-current-buffer buffer
      ;; Set the directory in the target buffer, not around it: it is
      ;; buffer-local and a surrounding `let' would be shadowed on entry.
      (setq default-directory (file-name-as-directory root))
      ;; Erase first.  diff-hl reuses one buffer across files and parses all
      ;; of it, so appending makes every file after the first report the
      ;; *previous* file's hunks -- three different files all claiming
      ;; "70 lines inserted" because the first one was 70 lines long.
      ;; This call receives every file it is asked about at once, so clearing
      ;; here is also correct for `vc-diff' over a whole directory.
      (let ((inhibit-read-only t)) (erase-buffer))
      (let ((start (point)))
        ;; stderr discarded: a snapshot can emit kilobytes of "Refused to
        ;; snapshot some files" that would otherwise land in the diff.
        (apply #'call-process jj-executable nil (list (current-buffer) nil)
               nil args)
        (if (= start (point)) 0 1)))))

(defun vc-jj-revision-completion-table (_files)
  "Changes available for completion, for `vc-diff' prompts."
  (mapcar #'cdr (jj--candidates)))

(defun vc-jj-checkout (file &optional rev destfile &rest _)
  "Write FILE at REV to DESTFILE (or FILE).

`vc-find-revision' reaches this through its save path, and without it
the whole thing fails with (vc-not-supported checkout JJ).  diff-hl
swallows that error and quietly falls back to diffing the file against
itself -- which is why the fringe stayed empty while `vc-jj-diff' was
demonstrably producing a correct diff."
  (let ((out (or destfile file)))
    (with-temp-file out
      (vc-jj-find-revision file (or rev "@-") (current-buffer)))))

(defun vc-jj-find-revision (file rev buffer)
  "Put FILE's contents at REV into BUFFER.

`diff-hl-flydiff-mode' -- which this config turns on for prog-mode --
diffs the live buffer against the base revision rather than against the
file on disk, and it materialises that base through `vc-find-revision'.
Without this the flydiff path fails quietly and you get no fringe marks
at all, even though `vc-jj-diff' is working perfectly."
  (let* ((root (or (jj--root (file-name-directory file))
                   (file-name-directory file)))
         (rel (file-relative-name file root)))
    (with-current-buffer buffer
      (setq default-directory (file-name-as-directory root))
      (erase-buffer)
      (call-process jj-executable nil (list (current-buffer) nil) nil
                    "--ignore-working-copy" "--no-pager"
                    "file" "show" "-r" (or rev "@-") rel))))

;;; Inline blame, and getting out of blamer's way ------------------------------

;; blamer.el is git-only: it shells out through vc-git.  Registering the JJ
;; backend makes `vc-mode' non-nil in jj buffers, which is enough for blamer
;; to decide the file is version controlled and start running `git' against a
;; tree that has no git in it.  Measured with the profiler: two
;; `vc-git--run-command-string' calls at ~165ms each, i.e. ~340ms added to
;; every file you open, for output that can never be right.
;;
;; So blamer (and git-gutter, git-only for the same reason) are turned off in
;; jj-only buffers, and replaced with the same idea driven by jj.

(defcustom jj-inline-blame-idle 0.5
  "Idle seconds before the inline blame for the current line appears."
  :type 'number
  :group 'jj)

(defface jj-inline-blame-face
  '((t :inherit (shadow italic)))
  "Face for the inline blame annotation."
  :group 'jj)

(defvar-local jj--blame-table nil
  "Vector of per-line \"author, when -- description\" strings, or nil.")
(defvar-local jj--blame-overlay nil)
(defvar-local jj--blame-line nil)
(defvar jj--blame-timer nil)

(defun jj--blame-build ()
  "Annotate the whole buffer once and cache it per line.

Per-line annotation would mean one jj process per cursor move; the file
is annotated once instead and invalidated when it changes."
  (when (and buffer-file-name (jj-available-p) (jj--root))
    (let* ((root (jj--root))
           (rel (file-relative-name buffer-file-name root))
           (default-directory root)
           (out (jj--read "--no-pager" "file" "annotate" rel))
           (lines (split-string out "\n"))
           (descs (make-hash-table :test 'equal))
           (vec (make-vector (1+ (length lines)) nil))
           (i 0))
      (dolist (l lines)
        (setq i (1+ i))
        (when (string-match vc-jj--annotate-re l)
          (let* ((change (match-string 1 l))
                 (who (match-string 2 l))
                 (date (format "%s-%s-%s" (match-string 3 l)
                               (match-string 4 l) (match-string 5 l)))
                 (desc (or (gethash change descs)
                           (puthash change
                                    (string-trim
                                     (jj--read "log" "--no-graph" "-r" change
                                               "-T" "description.first_line()"))
                                    descs))))
            (aset vec i (format "   %s, %s * %s"
                                who date
                                (if (string-empty-p desc) change desc))))))
      (setq jj--blame-table vec))))

(defun jj--blame-clear ()
  (when jj--blame-overlay (delete-overlay jj--blame-overlay))
  (setq jj--blame-overlay nil jj--blame-line nil))

(defun jj--blame-show ()
  "Overlay the blame for the current line, if it has moved."
  (when (and jj-inline-blame-mode buffer-file-name)
    (let ((line (line-number-at-pos)))
      (unless (eq line jj--blame-line)
        (jj--blame-clear)
        (setq jj--blame-line line)
        (unless jj--blame-table (jj--blame-build))
        (let ((text (and jj--blame-table
                         (< line (length jj--blame-table))
                         (aref jj--blame-table line))))
          (when text
            (setq jj--blame-overlay (make-overlay (line-end-position)
                                                  (line-end-position)))
            (overlay-put jj--blame-overlay 'after-string
                         (propertize text 'face 'jj-inline-blame-face))))))))

(define-minor-mode jj-inline-blame-mode
  "Show who last touched the current line, jj's answer rather than git's."
  :lighter " jjblame"
  (if jj-inline-blame-mode
      (progn
        (unless jj--blame-timer
          (setq jj--blame-timer
                (run-with-idle-timer jj-inline-blame-idle t #'jj--blame-show)))
        (add-hook 'after-save-hook #'jj--blame-invalidate nil t))
    (jj--blame-clear)
    (setq jj--blame-table nil)
    (remove-hook 'after-save-hook #'jj--blame-invalidate t)))

(defun jj--blame-invalidate ()
  "Drop the cached annotation; the file changed."
  (setq jj--blame-table nil)
  (jj--blame-clear))

(defcustom jj-disable-flydiff t
  "Turn off the global `diff-hl-flydiff-mode' when a jj buffer is opened.

flydiff shows unsaved edits; it cannot show a commit against its parent.
See the comment in `jj--setup-buffer-vc-ui'."
  :type 'boolean
  :group 'jj)

(defcustom jj-replace-git-blame t
  "In jj-only buffers, turn off blamer/git-gutter and use the jj equivalents."
  :type 'boolean
  :group 'jj)

(defun jj--setup-buffer-vc-ui ()
  "Swap the git-only UI for the jj one in a jj-without-git buffer."
  (when (and jj-replace-git-blame
             buffer-file-name
             (jj-available-p)
             (jj--root)
             (not (jj--git-repo-p)))
    ;; Both of these drive git directly and cannot work here.
    (when (bound-and-true-p blamer-mode) (blamer-mode -1))
    (when (bound-and-true-p git-gutter-mode) (git-gutter-mode -1))
    (jj-inline-blame-mode 1)
    ;; diff-hl goes through VC, so it works now that vc-jj-diff exists.
    ;;
    ;; Point it at `@-' explicitly.  Left alone, diff-hl (and flydiff) compare
    ;; against the *working* revision, so a commit you are building shows no
    ;; marks at all -- everything in it is already committed.  Against the
    ;; parent you get what this change adds and removes relative to the
    ;; previous commit in the stack, which is the diffbase that matters when
    ;; the stack is the unit of review.
    (setq-local diff-hl-reference-revision "@-")
    ;; diff-hl-flydiff has to go, and it is a *global* mode so this turns it
    ;; off everywhere, git repos included.
    ;;
    ;; flydiff replaces `diff-hl-changes-buffer' with one that diffs the live
    ;; buffer text against the reference.  That answers "what have I typed
    ;; since I saved", which is a different question from "what does this
    ;; commit change relative to the previous one in the stack" -- and for a
    ;; saved buffer the first answer is always "nothing", so the fringe stays
    ;; empty.  Measured on one file: flydiff on, 0 hunks; flydiff off, 1 hunk
    ;; of 70 inserted lines.  Set `jj-disable-flydiff' to nil to keep flydiff
    ;; and give up the against-parent marks.
    (when (and jj-disable-flydiff (bound-and-true-p diff-hl-flydiff-mode))
      (diff-hl-flydiff-mode -1)
      (message "jj: turned off diff-hl-flydiff so the fringe can show changes against @-"))
    ;; Enabling the mode already triggers one update; calling it again here
    ;; just paid for a second `jj diff'.
    (when (fboundp 'diff-hl-mode) (diff-hl-mode 1))))



(add-hook 'find-file-hook #'jj--setup-buffer-vc-ui 90)

;;; Blame / annotate ----------------------------------------------------------

;; `jj file annotate' lines look like
;;     qlptrlzn jaraberr 2026-09-15 03:12:50    1: #include "..."
;; which is close enough to the shape VC expects that implementing the three
;; annotate hooks gives you stock `vc-annotate' -- C-x v g -- with its age
;; colouring, `a' to re-annotate the revision under point, and all the rest.
;; No separate blame UI to learn.

(defconst vc-jj--annotate-re
  "^\\([a-z]+\\) +\\([^ ]+\\) +\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) \\([0-9:]+\\) +[0-9]+: "
  "Matches one `jj file annotate' line: change, author, date, time.")

(defun vc-jj-annotate-command (file buf &optional rev)
  "Insert an annotated FILE into BUF, optionally at REV."
  ;; Resolve the root and relative path *before* switching buffers.
  ;; `default-directory' is buffer-local, so a `let' wrapped around
  ;; `with-current-buffer' is shadowed by the target buffer's own value the
  ;; moment you enter it.  jj then runs in the wrong directory, cannot see the
  ;; path, and silently annotates nothing at all.
  (let* ((root (or (jj--root (file-name-directory file))
                   (file-name-directory file)))
         (rel (file-relative-name file root))
         (args (append (list "--ignore-working-copy" "--no-pager"
                             "file" "annotate")
                       (when rev (list "-r" rev))
                       (list rel))))
    (with-current-buffer buf
      (setq default-directory (file-name-as-directory root))
      (apply #'call-process jj-executable nil (list buf nil) nil args))))

(defun vc-jj-annotate-time ()
  "Day number for the line at point, as `vc-annotate' wants."
  ;; `vc-annotate-convert-time' lives in vc-annotate.el, which VC does not
  ;; necessarily have loaded when it calls this.
  (require 'vc-annotate)
  (save-excursion
    (beginning-of-line)
    (when (looking-at vc-jj--annotate-re)
      (let ((y (string-to-number (match-string 3)))
            (m (string-to-number (match-string 4)))
            (d (string-to-number (match-string 5))))
        (vc-annotate-convert-time
         (encode-time 0 0 0 d m y))))))

(defun vc-jj-annotate-extract-revision-at-line ()
  "The change id on the annotated line at point."
  (save-excursion
    (beginning-of-line)
    (when (looking-at vc-jj--annotate-re)
      (match-string 1))))

;;;###autoload
(defun jj-annotate (&optional rev)
  "Blame the current file with `vc-annotate' (C-x v g), at REV.

In the annotate buffer: `RET' visits the source line, `a' annotates the
revision that introduced it, `n'/`p' move through revisions, `q' quits."
  (interactive (list (when current-prefix-arg
                       (jj-read-change "Annotate at change: "))))
  (jj--assert-available)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file"))
  (require 'vc-annotate)
  (vc-annotate buffer-file-name (or rev "@")))

;;;###autoload
(defun jj-blame-line ()
  "Echo who last touched the current line, and offer to show that change."
  (interactive)
  (jj--assert-available)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file"))
  (let* ((root (jj--assert-root))
         (rel (file-relative-name buffer-file-name root))
         (n (line-number-at-pos))
         (default-directory root)
         (out (jj--read "--no-pager" "file" "annotate" rel))
         (line (nth (1- n) (split-string out "\n"))))
    (if (not (and line (string-match vc-jj--annotate-re line)))
        (message "No blame information for line %d" n)
      (let ((change (match-string 1 line))
            (who (match-string 2 line))
            (when- (match-string 3 line)))
        (message "line %d: %s by %s on %s%s" n change who when-
                 (let ((desc (string-trim
                              (jj--read "log" "--no-graph" "-r" change
                                        "-T" "description.first_line()"))))
                   (if (string-empty-p desc) "" (format " -- %s" desc))))))))

(defcustom jj-register-vc-backend t
  "Whether to add JJ to `vc-handled-backends'.

Off means `vc-mode' ignores jj workspaces entirely; the mode-line
indicator above is independent of this and keeps working."
  :type 'boolean
  :group 'jj)

(when jj-register-vc-backend
  (with-eval-after-load 'vc-hooks
    (add-to-list 'vc-handled-backends 'JJ t)))

;;; Magit ---------------------------------------------------------------------

;; magit cannot drive a jj-on-Piper repo: there is no .git for it to read.
;; Rather than have `magit-status' fail there, send it to `jj-status', so the
;; same keystroke does the right thing in both kinds of repo.  A colocated
;; jj+git checkout still gets real magit, since .git is present.

(defcustom jj-take-over-magit t
  "Whether `magit-status' should open `jj-status' in jj-only repos."
  :type 'boolean
  :group 'jj)

(defun jj--git-repo-p ()
  "Non-nil if there is a .git here, i.e. magit can actually work."
  (and (locate-dominating-file default-directory ".git") t))

(defun jj--magit-status-dispatch (orig &rest args)
  "Open `jj-status' when this is a jj repo with no git."
  (if (and jj-take-over-magit
           (jj-available-p)
           (jj--root)
           (not (jj--git-repo-p)))
      (progn
        (message "jj repo with no .git -- opening jj-status instead of magit")
        (jj-status))
    (apply orig args)))

(with-eval-after-load 'magit
  (advice-add 'magit-status :around #'jj--magit-status-dispatch))

;;; Transient menu -----------------------------------------------------------

(with-eval-after-load 'transient
  (transient-define-prefix jj-menu ()
    "Jujutsu."
    [["View"
      ("s" "status"        jj-status)
      ("l" "log"           jj-log)
      ("d" "diff"          jj-diff)
      ("RET" "show"        jj-show)
      ("o" "operation log" jj-op-log)]
     ["Change"
      ("D" "describe"      jj-describe)
      ("n" "new"           jj-new)
      ("e" "edit  (asks)"  jj-edit)
      ("S" "squash"        jj-squash)
      ("x" "abandon (asks)" jj-abandon)
      ("z" "undo op"       jj-undo)]
     ["Blame"
      ("a" "annotate (vc-annotate)" jj-annotate)
      ("A" "blame this line"        jj-blame-line)
      ("i" "inline blame toggle"    jj-inline-blame-mode)]
     ["Stack travel"
      ("L" "CL stack..."       jj-stack)
      ("]" "next CL up"        jj-next-cl)
      ("[" "previous CL down"  jj-prev-cl)
      ("W" "switch workspace"  jj-switch-workspace)
      ("P" "register with projectile" jj-projectile-register-workspaces)]
     ["Pick / switch"
      ("/" "find command..."   jj-commands)
      ("w" "work on change..." jj-switch-commit)
      ("b" "browse changes..." jj-browse-commit)
      ("U" "upload a change..." jj-upload-pick)
      ("m" "refresh mode line" jj-mode-line-refresh)]
     ["Files"
      ("f" "files in this change" jj-files)
      ("M" "move file to a CL"    jj-move-file-to-cl)
      ("," "move file to parent"  jj-move-file-to-parent)]
     ["Stack"
      ("k" "squash into parent"   jj-stash-into-parent)
      ("K" "fold @ + reupload CL" jj-amend-and-reupload)
      ("E" "describe + reupload"  jj-describe-and-reupload)]
     ["Piper / Critique"
      ("u" "upload to Critique" jj-upload)
      ("R" "re-upload (needs CL)" jj-reupload)
      ("c" "open CL"            jj-show-cl)
      ("p" "presubmit"          jj-presubmit)
      ("M" "mail  (asks twice)" jj-mail)]]))

;; `jj-menu' is only defined once transient loads; give the binding something
;; real to call either way.
;;;###autoload
(defun jj-dispatch ()
  "Open `jj-menu', loading transient first.
Says so plainly when jj is not installed, rather than failing inside a
transient."
  (interactive)
  (if (not (jj-available-p))
      (message "jj is not installed on this machine")
    (require 'transient nil t)
    (if (fboundp 'jj-menu)
        (call-interactively #'jj-menu)
      (call-interactively #'jj-status))))

;; Keep company out of the read-only jj buffers for good; see `jj-status-mode'.
(with-eval-after-load 'company
  ;; delete-dups because this file gets reloaded, and a plain `append' grew
  ;; the exclusion list a duplicate pair every time.
  (setq company-global-modes
        (cond ((eq company-global-modes t) '(not jj-status-mode vc-annotate-mode))
              ((and (consp company-global-modes) (eq (car company-global-modes) 'not))
               (cons 'not (delete-dups
                           (append (cdr company-global-modes)
                                   '(jj-status-mode vc-annotate-mode)))))
              (t company-global-modes))))

;; C-x j, not C-c v: `setup-eshell' binds C-c v to `vterm-toggle' and
;; `setup-org' to `org-reveal', and both load after this file, so C-c v never
;; reached jj-dispatch at all.  C-x j is free and sits next to C-x v, the VC
;; prefix that `jj-annotate' hangs off.
(global-set-key (kbd "C-x j") #'jj-dispatch)

(provide 'setup-jj)
;;; setup-jj.el ends here
