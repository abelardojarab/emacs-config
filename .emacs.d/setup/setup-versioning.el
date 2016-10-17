;;; setup-versioning.el ---

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

;; Follow symbolic links
(setq vc-follow-symlinks t)

;; Speed up find file
(remove-hook 'find-file-hooks 'vc-find-file-hook)

;; Designsync versioning control
(use-package vc-sync
  :config (progn
            (defun dired-sync-symlink-filter ()
              (save-excursion
                ;; Goto the beginning of the buffer
                (goto-char (point-min))
                ;; For each matching symbolic link with sync_cache or sync/mirrors in the path name...
                (while (re-search-forward "\\(-> .*/sync\\(_cache\\|/mirrors\\)/.*$\\)" nil t)
                  ;; Create an overlay that masks out everything between the -> and the end of line
                  (let ((o (make-overlay (match-beginning 1) (progn (end-of-line) (point)))))
                    (overlay-put o 'invisible t)
                    (overlay-put o 'evaporate t)))))
            (add-hook 'dired-after-readin-hook 'dired-sync-symlink-filter)))

;; psvn
(use-package psvn
  :config (progn
            (setq svn-status-hide-unmodified t)
            (setq svn-status-hide-unknown t)
            (setq svn-status-svn-file-coding-system 'utf-8)))

;; magit
(use-package magit
  :commands (magit-init
             magit-status
             magit-diff
             magit-log
             magit-commit
             magit-blame
             magit-mode
             magit-blame-mode
             magit-quit-session
             magit-status-internal
             projectile-vc
             git-visit-diffs
             git-visit-diffs-prev
             git-visit-diffs-next)
  :bind (:map ctl-x-map
              ("v" . magit-status)
              :map magit-mode-map
              (("C-c C-a" . magit-just-amend)
               ("c" . magit-maybe-commit)
               ("q" . magit-quit-session)))
  :load-path (lambda () (expand-file-name "magit/lisp" user-emacs-directory))
  :init (progn
          (eval-after-load 'info
            '(progn (info-initialize)
                    (add-to-list 'Info-directory-list (expand-file-name "magit/Documentation" user-emacs-directory))))

          ;; https://github.com/syl20bnr/spacemacs/issues/7225
          (setq with-editor-file-name-history-exclude 1)

          ;; Enable line highlight
          (add-hook 'magit-mode-hook #'hl-line-mode)

          ;; we no longer need vc-git
          (delete 'Git vc-handled-backends)

          ;; make magit status go full-screen but remember previous window
          ;; settings
          ;; from: http://whattheemacsd.com/setup-magit.el-01.html
          (defadvice magit-status (around magit-fullscreen activate)
            (window-configuration-to-register :magit-fullscreen)
            ad-do-it
            (delete-other-windows))

          ;; Make `magit-log' run alone in the frame, and then restore the old window
          ;; configuration when you quit out of magit.
          ;; from: https://github.com/philippe-grenet/exordium/blob/master/modules/init-git.el
          (defadvice magit-log (around magit-fullscreen activate)
            (window-configuration-to-register :magit-fullscreen)
            ad-do-it
            (delete-other-windows))

          (defun magit-quit-session ()
            "Restores the previous window configuration and kills the magit buffer"
            (interactive)
            (kill-buffer)
            (jump-to-register :magit-fullscreen))

          ;; Don't show "MRev" in the modeline
          (when (bound-and-true-p magit-auto-revert-mode)
            (diminish 'magit-auto-revert-mode))

          ;; Turn off the horrible warning about magit auto-revert of saved buffers
          (setq magit-last-seen-setup-instructions "1.4.0")

          ;; Close popup when commiting - this stops the commit window
          ;; hanging around
          ;; From: http://git.io/rPBE0Q
          (defadvice git-commit-commit (after delete-window activate)
            (delete-window))

          (defadvice git-commit-abort (after delete-window activate)
            (delete-window))

          ;; these two force a new line to be inserted into a commit window,
          ;; which stops the invalid style showing up.
          ;; From: http://git.io/rPBE0Q
          (defun magit-commit-mode-init ()
            (when (looking-at "\n")
              (open-line 1)))

          (add-hook 'git-commit-mode-hook 'magit-commit-mode-init))
  :config (progn

            ;; don't put "origin-" in front of new branch names by default
            (setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
                  ;; open magit status in same window as current buffer
                  magit-status-buffer-switch-function 'switch-to-buffer
                  ;; pop the process buffer if we're taking a while to complete
                  magit-process-popup-time 10
                  ;; ask me to save buffers
                  magit-save-some-buffers t
                  ;; ask me if I want a tracking upstream
                  magit-set-upstream-on-push 'askifnotset
                  ;; highlight word/letter changes in hunk diffs
                  magit-diff-refine-hunk t
                  ;; use ido to look for branches
                  magit-completing-read-function 'magit-ido-completing-read
                  ;; ask me if I want to include a revision when rewriting
                  magit-rewrite-inclusive 'ask
                  ;; this is too expensive to have on by default
                  magit-backup-mode nil
                  ;; don't revert automatically,
                  magit-auto-revert-mode nil
                  magit-refresh-file-buffer-hook nil ;; obsolete
                  magit-turn-on-auto-revert-mode nil ;; obsolete
                  magit-revert-buffers 'silent ;; obsolete
                  ;; see https://github.com/magit/magit/pull/2091
                  magit-keep-region-overlay t
                  ;; attempt to disable magit-auto-revert-immediately
                  magit-auto-revert-immediately (null (and (boundp 'auto-revert-use-notify)
                                                           auto-revert-use-notify))
                  git-commit-fill-column 120
                  git-commit-summary-max-length 80
                  auto-revert-verbose nil)

            ;; Face setup
            (set-face-foreground 'magit-hash (face-foreground 'font-lock-type-face))

            ;; defaults for popups
            (setq magit-branch-popup-show-variables nil)
            (defconst magit-pull-request-remote "upstream"
              "Where to find pull requests.")
            ;; From https://github.com/tarsius/magit-rockstar/
            ;; Changed "origin" to `magit-pull-request-remote', remove log action,
            ;; fetch it to <remote>/pull/<num> instead of pr-<num>.
            (defun magit-branch-pull-request (number &optional branch checkout)
              "Create a new branch from a Github pull request.
Read \"NR[:BRANCH-NAME] from the user. If BRANCH-NAME is not
provided use \"pr-NR\". Assume all pull requests can be found on
`magit-pull-request-remote'. With a prefix argument checkout
branch."
              (interactive
               (let ((input (magit-read-string "Branch pull request (NR[:BRANCH-NAME])")))
                 (if (string-match "\\([1-9][0-9]*\\)\\(?::\\(.+\\)\\)?" input)
                     (list (match-string 1 input)
                           (match-string 2 input)
                           current-prefix-arg)
                   (user-error "Invalid input"))))
              (unless branch
                (setq branch number))
              (magit-call-git "fetch" magit-pull-request-remote
                              (format "pull/%s/head:refs/remotes/%s/pull/%s"
                                      number magit-pull-request-remote branch))
              (when checkout
                (magit-run-git "checkout" branch)))
            (magit-define-popup-action 'magit-fetch-popup
              ?p "Fetch pull request" 'magit-branch-pull-request)

            ;; Show worktree section if there are worktrees, avoid overhead if
            ;; there aren't.
            (defvar-local np/magit-want-worktrees t)
            (put 'np/magit-want-worktrees 'permanent-local t)
            (defun np/magit-maybe-add-worktrees ()
              (if (and np/magit-want-worktrees
                       (not (memq #'magit-insert-worktrees magit-status-sections-hook))
                       (> (length (magit-list-worktrees)) 1))
                  (magit-add-section-hook
                   'magit-status-sections-hook #'magit-insert-worktrees
                   'magit-insert-status-headers 'append 'local)
                (setq-local np/magit-want-worktrees nil)))
            (add-hook 'magit-status-mode-hook #'np/magit-maybe-add-worktrees)

            ;; Show submodule section if there are submodules, avoid overhead if
            ;; there aren't.
            (defvar-local np/magit-want-submodules t)
            (put 'np/magit-want-submodules 'permanent-local t)
            (defun np/magit-maybe-add-submodules ()
              (if (and np/magit-want-submodules
                       (not (memq #'magit-insert-submodules magit-status-sections-hook))
                       (magit-get-submodules))
                  (dolist (inserter '(magit-insert-modules-unpulled-from-upstream
                                      magit-insert-modules-unpulled-from-pushremote
                                      magit-insert-modules-unpushed-to-upstream
                                      magit-insert-modules-unpushed-to-pushremote
                                      magit-insert-submodules))
                    (magit-add-section-hook
                     'magit-status-sections-hook inserter
                     'magit-insert-unpulled-from-upstream nil 'local))
                (setq-local np/magit-want-submodules nil)))
            (add-hook 'magit-status-mode-hook #'np/magit-maybe-add-submodules)

            ;; restore previously hidden windows
            (defadvice magit-quit-window (around magit-restore-screen activate)
              (let ((current-mode major-mode))
                ad-do-it
                ;; we only want to jump to register when the last seen buffer
                ;; was a magit-status buffer.
                (when (eq 'magit-status-mode current-mode)
                  (jump-to-register :magit-fullscreen))))

            (defun magit-maybe-commit (&optional show-options)
              "Runs magit-commit unless prefix is passed"
              (interactive "P")
              (if show-options
                  (magit-key-mode-popup-committing)
                (magit-commit)))

            ;; This extension finds all the changes ("hunks") between a working copy of a
            ;; git repository and a reference (branch name or hashref) and presents them
            ;; as a series of narrowed buffers.
            ;;
            ;; Interactive functions:
            ;;
            ;; git-visit-diffs (ref) : prompts for a ref, to be passed to `git diff`;
            ;; collects the hunks and view the first hunk, if any. Operates from the
            ;; current directory.
            ;; git-visit-diffs-next () : view the next hunk, if any.
            ;; git-visit-diffs-prev () : view the previous hunk, if any.

            ;; Global variables and types

            (defvar *git-visit-current-hunk-list*)
            (defvar *git-visit-previous-hunk-list*)
            (define-error 'git-error "Git error")

            ;; Utility functions
            (defun git-visit-visit-modified-region (filename line count)
              "Visit a hunk in a narrowed buffer"
              (find-file filename)
              (widen)
              (goto-line line)
              (beginning-of-line)
              (let ((beginning-position (line-beginning-position)))
                (next-line count)
                (narrow-to-region beginning-position (line-beginning-position))
                (beginning-of-buffer)))


            (defun git-visit-get-hunk-list (dir ref)
              "Return a list of changes between the current working copy and a git ref"
              ;; one triplet per change, like this: ((file1 13 8) (file1 19 63) (file2 24 12))
              (save-excursion
                (let (hunk-list file)
                  (with-temp-buffer
                    ;; (with-current-buffer (get-buffer-create "*git-visit-output*") (erase-buffer)
                    (if (zerop (call-process "git" nil (current-buffer) t "diff" ref))
                        (progn
                          (beginning-of-buffer)
                          (while (re-search-forward "\\+\\+\\+ \\(.*\\)\\|@@ -[0-9]+,[0-9]+ \\+\\([0-9]+\\),\\([0-9]+\\) @@" nil t)
                            (if (match-string 1)
                                (setq file
                                      (and
                                       (not (string-equal (match-string 1) "/dev/null"))
                                       (concat dir (substring (match-string 1) 2))))
                              (if file
                                  (setq hunk-list
                                        (cons (list file
                                                    (string-to-number (match-string 2))
                                                    (string-to-number (match-string 3)) )
                                              hunk-list))) )))
                      (signal 'git-error (buffer-substring-no-properties (buffer-end -1) (buffer-end 1)))))
                  hunk-list)))

            ;; Interactive functions
            (defun git-visit-diffs-next ()
              "Show next diff in narrowed buffer."
              (interactive)
              (if *git-visit-current-hunk-list*
                  (let ((next-hunk (car *git-visit-current-hunk-list*)))
                    (setq *git-visit-previous-hunk-list* (cons next-hunk *git-visit-previous-hunk-list*))
                    (setq *git-visit-current-hunk-list* (cdr *git-visit-current-hunk-list*))
                    (apply 'git-visit-visit-modified-region next-hunk))
                (message "at end of hunk list")))

            (defun git-visit-diffs-prev ()
              "Show previous diff in narrowed buffer."
              (interactive)
              (if (and *git-visit-previous-hunk-list* (cdr *git-visit-previous-hunk-list*))
                  (let ((next-hunk (cadr *git-visit-previous-hunk-list*)))
                    (setq *git-visit-current-hunk-list* (cons (car *git-visit-previous-hunk-list*) *git-visit-current-hunk-list*))
                    (setq *git-visit-previous-hunk-list* (cdr *git-visit-previous-hunk-list*))
                    (apply 'git-visit-visit-modified-region next-hunk))
                (message "at beginning of hunk list")))

            (defun git-visit-diffs (ref)
              "Finds the diffs between REF and working copy. Shows the first diff in a narrowed buffer."
              (interactive "sref: ")
              (let ((dir (ignore-errors
                           (file-name-as-directory (car (process-lines "git" "rev-parse" "--show-toplevel"))))))
                (if (not dir)
                    (error (message "%s" "cannot locate repository root"))
                  (condition-case error
                      (progn
                        (setq *git-visit-current-hunk-list* (git-visit-get-hunk-list dir ref))
                        (setq *git-visit-previous-hunk-list* nil)
                        ;; (print *git-visit-current-hunk-list* (get-buffer "*scratch*"))
                        (git-visit-diffs-next))
                    (git-error (message (cadr error)))))))))

;; magit integration with git flow
(use-package magit-gitflow
  :load-path (lambda () (expand-file-name "magit-gitflow/" user-emacs-directory))
  :commands (turn-on-magit-gitflow)
  :defer t
  :init (add-hook 'magit-mode-hook #'turn-on-magit-gitflow 'append))

;; diff-hl
(use-package diff-hl
  :defer t
  :commands (global-diff-hl-mode
             diff-hl-mode
             diff-hl-next-hunk
             diff-hl-previous-hunk
             diff-hl-mark-hunk
             diff-hl-diff-goto-hunk
             diff-hl-revert-hunk)
  :load-path (lambda () (expand-file-name "diff-hl/" user-emacs-directory))
  :config (progn
            (setq diff-hl-draw-borders t)
            (defadvice svn-sttus-update-modeline (after svn-update-diff-hl activate)
              (diff-hl-update))
            (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

            ;; Enable diff-hl
            (global-diff-hl-mode)))

;; git-timemachine
(use-package git-timemachine
  :commands (git-timemachine-start
             git-timemachine-toggle
             git-timemachine-switch-branch)
  :load-path (lambda () (expand-file-name "git-timemachine/" user-emacs-directory))
  :config (progn
            (defun my-git-timemachine-show-selected-revision ()
              "Show last (current) revision of file."
              (interactive)
              (let (collection)
                (setq collection
                      (mapcar (lambda (rev)
                                ;; re-shape list for the ivy-read
                                (cons (concat (substring (nth 0 rev) 0 7) "|" (nth 5 rev) "|" (nth 6 rev)) rev))
                              (git-timemachine--revisions)))
                (ivy-read "commits:"
                          collection
                          :action (lambda (rev)
                                    (git-timemachine-show-revision rev)))))

            (defun git-timemachine-start ()
              "Open git snapshot with the selected version.  Based on ivy-mode."
              (interactive)
              (unless (featurep 'git-timemachine)
                (require 'git-timemachine))
              (git-timemachine--start #'my-git-timemachine-show-selected-revision))))

;; Show blame for current line
(use-package git-messenger
  :load-path (lambda () (expand-file-name "git-messenger/" user-emacs-directory)))

(use-package git-gutter-plus
  :defer t
  :if (display-graphic-p)
  :diminish git-gutter+-mode
  :load-path (lambda () (expand-file-name "git-gutter-plus/" user-emacs-directory)))

(use-package git-gutter-fringe+
  :if (display-graphic-p)
  :commands global-git-gutter+-mode
  :load-path (lambda () (expand-file-name "git-gutter-fringe-plus/" user-emacs-directory))
  :config (progn
            (global-git-gutter+-mode t)
            (set-face-foreground 'git-gutter-fr+-modified "LightSeaGreen")
            (set-face-foreground 'git-gutter-fr+-added    "SeaGreen")
            (set-face-foreground 'git-gutter-fr+-deleted  "red")

            ;; Please adjust fringe width if your own sign is too big.
            (setq-default left-fringe-width 20)

            (fringe-helper-define 'git-gutter-fr+-added nil
              ".XXXXXX."
              "XXxxxxXX"
              "XX....XX"
              "XX....XX"
              "XXXXXXXX"
              "XXXXXXXX"
              "XX....XX"
              "XX....XX")

            (fringe-helper-define 'git-gutter-fr+-deleted nil
              "XXXXXX.."
              "XXXXXXX."
              "XX...xXX"
              "XX....XX"
              "XX....XX"
              "XX...xXX"
              "XXXXXXX."
              "XXXXXX..")

            (fringe-helper-define 'git-gutter-fr+-modified nil
              "XXXXXXXX"
              "XXXXXXXX"
              "Xx.XX.xX"
              "Xx.XX.xX"
              "Xx.XX.xX"
              "Xx.XX.xX"
              "Xx.XX.xX"
              "Xx.XX.xX")

            ;; Fringe fix in Windows
            (unless (string-equal system-type "windows-nt")
              (defadvice git-gutter+-process-diff (before git-gutter+-process-diff-advice activate)
                (ad-set-arg 0 (file-truename (ad-get-arg 0)))))))

;; ibuffer versioning-based groups
(use-package ibuffer-vc
  :load-path (lambda () (expand-file-name "ibuffer-vc/" user-emacs-directory)))

(provide 'setup-versioning)
;;; setup-versioning.el ends here
