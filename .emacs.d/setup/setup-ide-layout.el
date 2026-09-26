;;; setup-ide-layout.el --- Permanent IDE frame layout  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojarab@gmail.com>
;; Keywords: convenience, tools

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

;; One fixed frame layout, in a terminal or a GUI:
;;
;;   +----------+-----------------------+------------+
;;   |          |                       |            |
;;   |   tree   |        editor         |  versions  |
;;   |  (left)  |                       |  (right)   |
;;   |          |                       |            |
;;   +------------------------------------------------+
;;   |                  terminal                      |
;;   +------------------------------------------------+
;;
;; * left    treemacs, always
;; * right   `jj-status' in a jj workspace, `magit-status' in a git one,
;;           refreshed on save and while you are idle
;; * bottom  vterm if it is built, otherwise eshell, rooted at the repo
;;
;; `M-x my/ide-layout' turns the whole thing on and off.
;;
;; Permanent means permanent: these are Emacs *side windows*, so
;; `delete-other-windows' leaves them alone and `find-file' never lands a
;; source buffer in one; and while the layout is on, a poll puts back any
;; piece that got closed.  Turning it off with `my/ide-layout' sets a frame
;; parameter, so it stays off until you ask for it again.
;;
;; Four things in this configuration fight a layout like this:
;;
;; * `shackle-mode' conses its own catch-all onto the *front* of
;;   `display-buffer-alist' when it starts, and one of its rules is `^ ?\\*'
;;   with :autokill and :autoclose.  That matches the status buffer, the
;;   terminal, and -- because treemacs names its buffer with a leading space
;;   -- the tree as well.  First match wins, so our entries are pushed ahead
;;   of it, and this file must load after `setup-windows'.
;;
;; * `window-purpose' installs itself as `display-buffer-base-action', and
;;   `purpose-x-magit-single-on' wants magit in a window of its own.  The
;;   alist is consulted before either, so going through the alist wins.
;;
;; * treemacs re-asserts `window-slot' 0 on its own window on every window
;;   configuration change.  It owns the left side, so nothing collides.
;;
;; * `jj-status-refresh' is two jj subprocesses, about half a second.  From
;;   `after-save-hook' that is a stall you feel on every C-x C-s, so refreshes
;;   are debounced onto an idle timer and skipped when nothing is showing.
;;
;; Opening treemacs on a google3 workspace costs 31 seconds untuned and 73
;; milliseconds tuned; see `my/ide-layout-tune-treemacs'.
;;
;; Degrades quietly: no treemacs means a dired sidebar, no vterm means eshell,
;; no jj and no magit means no right-hand window.

;;; Code:

(require 'seq)

(defgroup my/ide-layout nil
  "A permanent IDE frame layout."
  :group 'convenience
  :prefix "my/ide-layout-")

(defcustom my/ide-layout-components '(explorer vcs terminal)
  "Which side windows the layout puts up."
  :type '(set (const :tag "File tree, left"   explorer)
              (const :tag "Version control, right" vcs)
              (const :tag "Terminal, bottom"  terminal))
  :group 'my/ide-layout)

(defcustom my/ide-layout-explorer-width 32
  "Width of the file tree, in columns."
  :type 'integer
  :group 'my/ide-layout)

(defcustom my/ide-layout-vcs-width 48
  "Width of the version-control window, in columns."
  :type 'integer
  :group 'my/ide-layout)

(defcustom my/ide-layout-terminal-height 0.22
  "Height of the terminal window, as a fraction of the frame."
  :type 'number
  :group 'my/ide-layout)

(defcustom my/ide-layout-min-editor-width 60
  "Columns the editor must keep.

A side window that leaves no room to read code is worse than no side
window.  On a frame too narrow for the configured widths, both columns
are scaled down together, and below this they are dropped entirely."
  :type 'integer
  :group 'my/ide-layout)

(defcustom my/ide-layout-terminal-backend nil
  "Terminal to run at the bottom, or nil to pick the best available."
  :type '(choice (const :tag "Automatic" nil)
                 (const vterm) (const eshell) (const shell))
  :group 'my/ide-layout)

(defcustom my/ide-layout-auto t
  "Whether visiting a file in a repository puts the layout up by itself."
  :type 'boolean
  :group 'my/ide-layout)

(defcustom my/ide-layout-refresh-delay 1.5
  "Idle seconds to wait after a save before refreshing the VCS window."
  :type 'number
  :group 'my/ide-layout)

(defcustom my/ide-layout-poll-interval 30
  "Seconds between upkeep passes while the layout is on.

Each pass refreshes the version-control window and puts back any part of
the layout that has been closed.  It only runs while Emacs is idle and
only on frames where the layout is on, so it costs nothing when you are
typing or when the layout is off."
  :type 'number
  :group 'my/ide-layout)

(defcustom my/ide-layout-tune-treemacs t
  "Whether to turn off the treemacs features that are ruinous on CitC.

Opening the tree on a google3 workspace takes 31 seconds with this off
and 73 milliseconds with it on, measured on ajb_signal_framework.  Three
settings account for all of it:

  `treemacs-collapse-dirs' -- spawns a Python helper for every directory
    expanded.  Follow mode expands the ten levels down to the file you
    are editing, over a FUSE mount, so the helper runs ten times.  This
    one setting is 31s -> 600ms on its own.
  `treemacs-git-mode' -- runs git status against a workspace that is jj
    and CitC, so it costs processes and reports nothing.
  `treemacs-filewatch-mode' -- inotify watches over a FUSE mount of the
    whole depot; 600ms -> 73ms once it is off.

Follow mode stays on: revealing the file you are editing is the point of
the tree, and it costs about 70ms.  These are global treemacs settings,
so they stay off for the rest of the session once the tree has opened on
a jj workspace."
  :type 'boolean
  :group 'my/ide-layout)

;; jj lives in `setup-jj'; treemacs, vterm and magit are lazy.  Everything is
;; guarded with `fboundp' at run time; these only keep the compiler quiet.
(defvar vterm-kill-buffer-on-exit)
(defvar treemacs-position)
(defvar treemacs-width)
(defvar treemacs-collapse-dirs)
(declare-function jj--root                "setup-jj" (&optional dir))
(declare-function jj-status-buffer        "setup-jj" (&optional root))
(declare-function jj-status-refresh       "setup-jj" ())
(declare-function vterm                   "vterm" (&optional arg))
(declare-function eshell-mode             "esh-mode" ())
(declare-function magit-status-setup-buffer "magit-status" (&optional directory))
(declare-function magit-refresh           "magit-mode" ())
(declare-function treemacs                "treemacs" ())
(declare-function treemacs-select-window  "treemacs" (&optional arg))
(declare-function treemacs-current-visibility "treemacs-scope" ())
(declare-function treemacs-git-mode       "treemacs-async" (&optional arg))
(declare-function treemacs-filewatch-mode "treemacs-filewatch-mode" (&optional arg))
(declare-function treemacs--maybe-load-workspaces "treemacs-persistence" ())
(declare-function treemacs--find-project-for-path "treemacs-workspaces" (path))
(declare-function treemacs-do-add-project-to-workspace "treemacs-workspaces" (path name))


;;; Repository detection -------------------------------------------------------

(defvar-local my/ide-layout--repo-cache 'unset
  "Cached (ROOT . KIND) for this buffer, nil if none.  `unset' means unknown.")

(defun my/ide-layout--repo ()
  "Return (ROOT . KIND) for this buffer, KIND being `jj' or `git'.

One `locate-dominating-file' walk looking for either marker, cached per
buffer: this runs from `find-file-hook' and `after-save-hook', so it has
to cost nothing.  Remote files are never probed -- walking a TRAMP path
is a round trip per directory."
  (if (not (eq my/ide-layout--repo-cache 'unset))
      my/ide-layout--repo-cache
    (setq my/ide-layout--repo-cache
          (and (stringp default-directory)
               (not (file-remote-p default-directory))
               (let ((hit (locate-dominating-file
                           default-directory
                           (lambda (dir)
                             (or (file-exists-p (expand-file-name ".jj" dir))
                                 (file-exists-p (expand-file-name ".git" dir)))))))
                 (when hit
                   (let ((root (directory-file-name (file-truename hit))))
                     (cons root
                           (if (file-exists-p (expand-file-name ".jj" hit))
                               'jj
                             'git)))))))))

(defun my/ide-layout--root ()
  "The repository root for this buffer, or nil."
  (car (my/ide-layout--repo)))

(defun my/ide-layout--name (root)
  "The short repository name for ROOT."
  (file-name-nondirectory (directory-file-name root)))

;; Must match the name `jj-status' gives its buffer, or the two fight over the
;; same slot under different names.
(defconst my/ide-layout--jj-re "\\`\\*jj: .+\\*\\'"
  "Matches the per-workspace jj status buffers.")

(defconst my/ide-layout--magit-re "\\`magit: .+\\'"
  "Matches magit status buffers.")

(defconst my/ide-layout--terminal-re "\\`\\*term: .+\\*\\'"
  "Matches the per-repository terminal buffers.")

;; treemacs names its buffer " *Treemacs-Scoped-Buffer-#<frame ...>*" -- note
;; the leading space, exactly what shackle's `^ ?\\*' catch-all matches.  Left
;; alone, shackle drags the tree out of its side window and attaches :autokill.
(defconst my/ide-layout--explorer-re "\\` ?\\*Treemacs"
  "Matches treemacs buffers, whatever scope they belong to.")

(defconst my/ide-layout--dired-explorer-re "\\`\\*files: .+\\*\\'"
  "Matches the dired sidebar used when treemacs is unavailable.")

(defun my/ide-layout--terminal-name (root)
  "Name of the terminal buffer for ROOT."
  (format "*term: %s*" (my/ide-layout--name root)))

(defun my/ide-layout--jj-status-name (root)
  "Name of the jj status buffer for ROOT.  Must match what `jj-status' uses."
  (format "*jj: %s*" (my/ide-layout--name root)))


;;; Sizing ---------------------------------------------------------------------

(defun my/ide-layout--widths ()
  "Return (EXPLORER . VCS) column widths that fit the selected frame.

Scaled down together when the frame is too narrow for both, and nil when
even scaled they would leave less than `my/ide-layout-min-editor-width'
for the code -- an 80-column terminal has no business carrying two side
columns."
  (let* ((frame (frame-width))
         (want  (+ my/ide-layout-explorer-width my/ide-layout-vcs-width))
         (spare (- frame my/ide-layout-min-editor-width)))
    (cond
     ((<= spare 0) nil)
     ((<= want spare) (cons my/ide-layout-explorer-width my/ide-layout-vcs-width))
     (t
      ;; Scale both down, but never below the width at which a file tree or a
      ;; status line stops being readable -- and if even those floors do not
      ;; fit the budget, say so rather than handing back a layout that eats
      ;; the editor.  An 80-column terminal ends up here.
      (let* ((scale (/ (float spare) want))
             (ew (max 18 (floor (* my/ide-layout-explorer-width scale))))
             (vw (max 24 (floor (* my/ide-layout-vcs-width scale)))))
        (and (<= (+ ew vw) spare) (cons ew vw)))))))


;;; display-buffer rules -------------------------------------------------------

(defvar my/ide-layout--rules nil
  "The `display-buffer-alist' entries this module installed, by identity.")

(defun my/ide-layout-install-rules ()
  "Put this module's entries at the head of `display-buffer-alist'.

Head, not tail: `shackle-mode' conses its catch-all onto the front when
it starts, and its `^ ?\\*' rule carries :autokill and :autoclose, which
would kill these buffers out from under the layout.  Re-run after
changing any of the size options."
  (interactive)
  (dolist (old my/ide-layout--rules)
    (setq display-buffer-alist (delq old display-buffer-alist)))
  (let* ((widths (my/ide-layout--widths))
         (ew (or (car widths) my/ide-layout-explorer-width))
         (vw (or (cdr widths) my/ide-layout-vcs-width))
         (side `((dedicated . t)
                 (window-parameters . ((no-delete-other-windows . t))))))
    (setq my/ide-layout--rules
          (list
           ;; Left column: the tree.  treemacs pins itself to slot 0 on its
           ;; own side, and it is the only thing on the left, so nothing
           ;; collides.
           `(,my/ide-layout--explorer-re
             (display-buffer-in-side-window)
             (side . left) (slot . 0) (window-width . ,ew) ,@side)
           `(,my/ide-layout--dired-explorer-re
             (display-buffer-in-side-window)
             (side . left) (slot . 0) (window-width . ,ew) ,@side)
           ;; Right column: whichever version control this repository uses.
           `(,my/ide-layout--jj-re
             (display-buffer-in-side-window)
             (side . right) (slot . 0) (window-width . ,vw) ,@side)
           `(,my/ide-layout--magit-re
             (display-buffer-in-side-window)
             (side . right) (slot . 0) (window-width . ,vw) ,@side)
           ;; Bottom: the terminal, full width.
           `(,my/ide-layout--terminal-re
             (display-buffer-in-side-window)
             (side . bottom) (slot . 0)
             (window-height . ,my/ide-layout-terminal-height) ,@side)))
    (dolist (rule (reverse my/ide-layout--rules))
      (push rule display-buffer-alist))))

(defun my/ide-layout--pin (window)
  "Stop WINDOW being resized by the rest of the window machinery."
  (when (window-live-p window)
    (set-window-dedicated-p window t)
    (window-preserve-size window t t)
    window))


;;; The tree -------------------------------------------------------------------

(defun my/ide-layout--show-explorer-dired (root)
  "Sidebar of last resort: a dired buffer on ROOT, when treemacs is missing."
  (let ((buf (dired-noselect root)))
    (with-current-buffer buf
      (rename-buffer (format "*files: %s*" (my/ide-layout--name root)) t))
    (my/ide-layout--pin (display-buffer buf))))

(defvar my/ide-layout--treemacs-tuned nil
  "Whether the CitC treemacs settings have already been applied.")

(defun my/ide-layout--tune-treemacs ()
  "Turn off the treemacs features that make a CitC tree unusable.
See `my/ide-layout-tune-treemacs' for the measurements behind each one."
  (when (and my/ide-layout-tune-treemacs (not my/ide-layout--treemacs-tuned))
    (setq my/ide-layout--treemacs-tuned t)
    (setq treemacs-collapse-dirs 0)
    (when (bound-and-true-p treemacs-git-mode)
      (ignore-errors (treemacs-git-mode -1)))
    (when (bound-and-true-p treemacs-filewatch-mode)
      (ignore-errors (treemacs-filewatch-mode -1)))))

(defun my/ide-layout--show-explorer (root)
  "Show the file tree for ROOT on the left."
  (if (not (require 'treemacs nil t))
      (my/ide-layout--show-explorer-dired root)
    (my/ide-layout--tune-treemacs)
    (setq treemacs-position 'left
          treemacs-width    (or (car (my/ide-layout--widths))
                                my/ide-layout-explorer-width))
    (save-selected-window
      (condition-case err
          (progn
            ;; Teach the workspace about this root *before* opening the tree.
            ;; On an empty workspace `treemacs--init' stops to ask for a first
            ;; project, and that prompt would fire in the middle of building
            ;; the layout -- off a timer, with no obvious cause.
            (when (fboundp 'treemacs--maybe-load-workspaces)
              (treemacs--maybe-load-workspaces))
            (when (and (fboundp 'treemacs--find-project-for-path)
                       (fboundp 'treemacs-do-add-project-to-workspace)
                       (not (treemacs--find-project-for-path
                             (file-name-as-directory root))))
              (treemacs-do-add-project-to-workspace
               (file-name-as-directory root) (my/ide-layout--name root)))
            (unless (and (fboundp 'treemacs-current-visibility)
                         (eq 'visible (treemacs-current-visibility)))
              (treemacs-select-window)))
        (error
         ;; Loud enough to explain a missing tree, quiet enough not to take
         ;; the rest of the layout down with it.
         (message "ide-layout: file tree unavailable (%s)"
                  (error-message-string err)))))))


;;; Version control ------------------------------------------------------------

(defun my/ide-layout--vcs-buffer (repo)
  "The version-control buffer for REPO, a (ROOT . KIND) cons, or nil.
Does not create one."
  (pcase (cdr repo)
    ('jj  (get-buffer (my/ide-layout--jj-status-name (car repo))))
    ('git (seq-find (lambda (b)
                      (and (string-match-p my/ide-layout--magit-re (buffer-name b))
                           (equal (file-truename
                                   (buffer-local-value 'default-directory b))
                                  (file-name-as-directory (car repo)))))
                    (buffer-list)))))

(defun my/ide-layout--show-vcs (repo &optional force)
  "Show the version-control window for REPO without selecting it.

Reuses a live buffer unless FORCE.  Rendering a jj status costs about
640ms in two subprocesses, and the layout is re-asserted on a timer --
paying that every pass for a buffer already on screen would be most of a
second of frozen Emacs for no new information.  The debounced save hook
and the upkeep poll are what keep it current."
  (let ((buf (unless force (my/ide-layout--vcs-buffer repo))))
    (unless (buffer-live-p buf)
      (setq buf
            (pcase (cdr repo)
              ('jj (and (fboundp 'jj-status-buffer)
                        (ignore-errors (jj-status-buffer (car repo)))))
              ('git (and (require 'magit nil t)
                         (fboundp 'magit-status-setup-buffer)
                         (ignore-errors
                           (save-window-excursion
                             (magit-status-setup-buffer (car repo)))
                           (my/ide-layout--vcs-buffer repo)))))))
    (when (buffer-live-p buf)
      (my/ide-layout--pin (display-buffer buf)))))

(defun my/ide-layout--refresh-buffer (buf)
  "Re-render the version-control buffer BUF in place."
  (with-current-buffer buf
    (ignore-errors
      (cond ((and (derived-mode-p 'jj-status-mode) (fboundp 'jj-status-refresh))
             (jj-status-refresh))
            ((and (derived-mode-p 'magit-status-mode) (fboundp 'magit-refresh))
             (magit-refresh))))))


;;; Terminal -------------------------------------------------------------------

(defun my/ide-layout--terminal-backend ()
  "The terminal to use: the configured one, or the best available."
  (or my/ide-layout-terminal-backend
      (if (and (require 'vterm nil t) (fboundp 'vterm)) 'vterm 'eshell)))

(defun my/ide-layout--show-terminal (root)
  "Show a terminal rooted at ROOT, reusing the existing one if there is one."
  (let* ((name (my/ide-layout--terminal-name root))
         (buf  (get-buffer name)))
    (if (buffer-live-p buf)
        (my/ide-layout--pin (display-buffer buf))
      (let ((default-directory (file-name-as-directory root)))
        ;; Each of these displays the buffer itself, and the rule above sends
        ;; it to the bottom slot; `save-selected-window' in the caller puts
        ;; point back in the editor.
        (pcase (my/ide-layout--terminal-backend)
          ('vterm  (let ((vterm-kill-buffer-on-exit nil))
                     (vterm name)))
          ('shell  (shell (get-buffer-create name)))
          (_       (require 'esh-mode)
                   (with-current-buffer (get-buffer-create name)
                     (eshell-mode)
                     (display-buffer (current-buffer)))))
        (my/ide-layout--pin (get-buffer-window name))))))


;;; Putting it up and taking it down -------------------------------------------

(defun my/ide-layout--side-window (re)
  "The side window showing a buffer matching RE on this frame, or nil."
  (seq-find (lambda (win)
              (and (window-parameter win 'window-side)
                   (string-match-p re (buffer-name (window-buffer win)))))
            (window-list nil 'no-minibuf)))

(defun my/ide-layout--vcs-window ()
  "The version-control side window on this frame, or nil."
  (or (my/ide-layout--side-window my/ide-layout--jj-re)
      (my/ide-layout--side-window my/ide-layout--magit-re)))

(defun my/ide-layout-on-p ()
  "Whether the layout is switched on for the selected frame."
  (and (frame-parameter nil 'my/ide-layout-on) t))

(defvar my/ide-layout--quiet nil
  "Bound while the upkeep poll works, to keep it from repeating messages.")

;;;###autoload
(defun my/ide-layout-enable (&optional repo)
  "Put the IDE layout up for REPO, or the repository around point.

Leaves point where it was: every component displays itself, and the whole
thing runs inside `save-selected-window'."
  (interactive)
  (let ((repo (or repo (my/ide-layout--repo))))
    (unless repo
      (user-error "Not inside a jj or git repository: %s" default-directory))
    (my/ide-layout-install-rules)
    (set-frame-parameter nil 'my/ide-layout-on t)
    (set-frame-parameter nil 'my/ide-layout-refused nil)
    (let ((widths (my/ide-layout--widths))
          (root (car repo)))
      (unless (or widths my/ide-layout--quiet)
        (message (concat "ide-layout: frame is %d columns, too narrow for the "
                         "tree and version control; lower "
                         "`my/ide-layout-min-editor-width' (now %d) to force them")
                 (frame-width) my/ide-layout-min-editor-width))
      (save-selected-window
        (when (and widths (memq 'explorer my/ide-layout-components))
          (my/ide-layout--show-explorer root))
        (when (and widths (memq 'vcs my/ide-layout-components))
          (my/ide-layout--show-vcs repo))
        (when (memq 'terminal my/ide-layout-components)
          (my/ide-layout--show-terminal root))))
    (my/ide-layout--start-poll)
    repo))

;;;###autoload
(defun my/ide-layout-disable ()
  "Take the IDE layout down and keep it down on this frame.

Clears the frame parameter rather than only deleting the windows, so
neither the upkeep poll nor the next file you visit brings it back."
  (interactive)
  (set-frame-parameter nil 'my/ide-layout-on nil)
  (set-frame-parameter nil 'my/ide-layout-refused t)
  (dolist (win (window-list nil 'no-minibuf))
    (when (window-live-p win)
      (let ((name (buffer-name (window-buffer win))))
        (when (and (window-parameter win 'window-side)
                   (seq-some (lambda (re) (string-match-p re name))
                             (list my/ide-layout--jj-re
                                   my/ide-layout--magit-re
                                   my/ide-layout--terminal-re
                                   my/ide-layout--dired-explorer-re)))
          (set-window-parameter win 'no-delete-other-windows nil)
          (ignore-errors (delete-window win))))))
  (when (and (fboundp 'treemacs-current-visibility)
             (eq 'visible (treemacs-current-visibility)))
    (ignore-errors (treemacs)))
  (my/ide-layout--stop-poll))

;;;###autoload
(defun my/ide-layout ()
  "Turn the IDE layout on, or off if it is already on."
  (interactive)
  (if (my/ide-layout-on-p)
      (my/ide-layout-disable)
    (my/ide-layout-enable)))

;;;###autoload
(defalias 'my/ide-layout-toggle #'my/ide-layout)

;;;###autoload
(defun my/ide-layout-rebuild ()
  "Tear the layout down, re-render it and put it straight back up.
Use after changing any of the `my/ide-layout-' options."
  (interactive)
  (let ((repo (my/ide-layout--repo)))
    (my/ide-layout-disable)
    (my/ide-layout-enable repo)
    (when (memq 'vcs my/ide-layout-components)
      (save-selected-window (my/ide-layout--show-vcs repo :force)))))

;;;###autoload
(defun my/ide-layout-select-terminal ()
  "Jump to the terminal for this repository, starting it if needed."
  (interactive)
  (let ((root (or (my/ide-layout--root)
                  (user-error "Not inside a jj or git repository"))))
    (my/ide-layout-install-rules)
    (my/ide-layout--show-terminal root)
    (let ((win (get-buffer-window (my/ide-layout--terminal-name root))))
      (when (window-live-p win) (select-window win)))))

;;;###autoload
(defun my/ide-layout-select-vcs ()
  "Jump to the version-control window, opening it if needed."
  (interactive)
  (let ((repo (or (my/ide-layout--repo)
                  (user-error "Not inside a jj or git repository"))))
    (my/ide-layout-install-rules)
    (my/ide-layout--show-vcs repo)
    (let ((win (my/ide-layout--vcs-window)))
      (when (window-live-p win) (select-window win)))))


;;; Refresh and upkeep ---------------------------------------------------------

;;;###autoload
(defun my/ide-layout-refresh-vcs ()
  "Re-render every version-control buffer a window is actually showing."
  (interactive)
  (dolist (win (window-list-1 nil 'no-minibuf t))
    (let ((buf (window-buffer win)))
      (when (buffer-live-p buf)
        (my/ide-layout--refresh-buffer buf)))))

(defvar my/ide-layout--refresh-timer nil
  "Pending one-shot timer that refreshes the version-control window.")

(defun my/ide-layout--schedule-refresh ()
  "Debounce a refresh onto an idle timer after a save.

`jj-status-refresh' shells out twice, about half a second together.  Run
straight from `after-save-hook' that is a stall you feel on every C-x
C-s, and saving several files in a row would pay it once each."
  (when (my/ide-layout--repo)
    (when (timerp my/ide-layout--refresh-timer)
      (cancel-timer my/ide-layout--refresh-timer))
    (setq my/ide-layout--refresh-timer
          (run-with-idle-timer my/ide-layout-refresh-delay nil
                               #'my/ide-layout-refresh-vcs))))

(defvar my/ide-layout--poll-timer nil
  "Repeating timer that keeps the layout up to date and in one piece.")

(defun my/ide-layout--poll ()
  "Upkeep pass: refresh version control, put back anything that was closed.

Only acts while Emacs is idle, so it never competes with typing, and only
on frames where the layout is switched on."
  (when (current-idle-time)
    (let ((my/ide-layout--quiet t))
      (dolist (frame (frame-list))
        (when (and (frame-live-p frame)
                   (frame-parameter frame 'my/ide-layout-on))
          (with-selected-frame frame
            ;; Any window on the frame will do -- the terminal and the tree
            ;; sit in the repository too, and the editor is usually selected.
            (let ((repo (seq-some (lambda (win)
                                    (with-current-buffer (window-buffer win)
                                      (my/ide-layout--repo)))
                                  (window-list frame 'no-minibuf))))
              (when repo
                (let ((win (my/ide-layout--vcs-window)))
                  (if win
                      (my/ide-layout--refresh-buffer (window-buffer win))
                    ;; A piece went missing -- permanent means we put it back.
                    (ignore-errors (my/ide-layout-enable repo))))))))))))

(defun my/ide-layout--start-poll ()
  "Start the upkeep timer if it is not already running."
  (unless (timerp my/ide-layout--poll-timer)
    (setq my/ide-layout--poll-timer
          (run-with-timer my/ide-layout-poll-interval
                          my/ide-layout-poll-interval
                          #'my/ide-layout--poll))))

(defun my/ide-layout--stop-poll ()
  "Stop the upkeep timer once no frame wants the layout."
  (unless (seq-some (lambda (f) (frame-parameter f 'my/ide-layout-on)) (frame-list))
    (when (timerp my/ide-layout--poll-timer)
      (cancel-timer my/ide-layout--poll-timer))
    (setq my/ide-layout--poll-timer nil)))


;;; Arming ---------------------------------------------------------------------

(defvar my/ide-layout--arm-timer nil
  "Pending one-shot timer that puts the layout up.")

(defun my/ide-layout--arm ()
  "Put the layout up if this frame wants it.  Runs off an idle timer."
  (setq my/ide-layout--arm-timer nil)
  (let ((repo (my/ide-layout--repo)))
    (when (and repo
               (not (frame-parameter nil 'my/ide-layout-on))
               (not (frame-parameter nil 'my/ide-layout-refused))
               (not (window-minibuffer-p)))
      (ignore-errors (my/ide-layout-enable repo)))))

(defun my/ide-layout--maybe-arm ()
  "Schedule `my/ide-layout--arm' from `find-file-hook'.

Deferred rather than immediate: `find-file-hook' also runs inside
`save-window-excursion' in other packages, and building a layout there
would be undone a moment later or, worse, left half-built."
  (when (and my/ide-layout-auto
             (not noninteractive)
             (not (timerp my/ide-layout--arm-timer))
             (my/ide-layout--repo))
    (setq my/ide-layout--arm-timer
          (run-with-idle-timer 0.4 nil #'my/ide-layout--arm))))

(add-hook 'find-file-hook  #'my/ide-layout--maybe-arm)
(add-hook 'after-save-hook #'my/ide-layout--schedule-refresh)

;; Install the rules now so that opening a status buffer or a repository
;; terminal by hand already goes to the right slot, layout or no layout.
(my/ide-layout-install-rules)


;;; Discovery ------------------------------------------------------------------

;; Reachable by name, the way the rest of the jj commands are.
(with-eval-after-load 'setup-jj
  (when (boundp 'jj-command-table)
    (dolist (entry '(("layout: turn the IDE layout on or off" . my/ide-layout)
                     ("layout: rebuild after resizing"        . my/ide-layout-rebuild)
                     ("layout: go to the terminal"            . my/ide-layout-select-terminal)
                     ("layout: go to version control"         . my/ide-layout-select-vcs)))
      (unless (rassq (cdr entry) jj-command-table)
        (setq jj-command-table (append jj-command-table (list entry)))))))

(with-eval-after-load 'transient
  (when (fboundp 'transient-append-suffix)
    (ignore-errors
      (transient-append-suffix 'jj-menu '(0 -1)
        ["Layout"
         ("SPC" "IDE layout on/off" my/ide-layout)
         ("!"   "go to terminal"    my/ide-layout-select-terminal)
         ("="   "rebuild layout"    my/ide-layout-rebuild)]))))

(provide 'setup-ide-layout)
;;; setup-ide-layout.el ends here
