;;; setup-versioning.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2017  Abelardo Jara-Berrocal

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

;; Follow symbolic links
(setq vc-follow-symlinks t)

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
  :defer t
  :config (progn
            (setq svn-status-hide-unmodified t)
            (setq svn-status-hide-unknown t)
            (setq svn-status-svn-file-coding-system 'utf-8)))

;; git-modes
(use-package git-modes
  :load-path (lambda () (expand-file-name "git-modes/" user-emacs-directory)))

;; magit
(use-package magit
  :commands (magit-init
             magit-status
             magit-diff
             magit-log
             magit-log-mode
             magit-commit
             magit-blame
             magit-mode
             magit-blame-mode
             magit-quit-session
             magit-status-internal
             magit-checkout
             magit-fetch
             magit-fetch-popup
             magit-branch
             magit-branch-popup
             magit-push
             magit-push-popup
             magit-pull
             magit-pull-popup
             magit-stage-file
             magit-unstage-file
             magit-display-buffer
             magit-git-wash
             magit-insert-section
             projectile-vc
             git-commit-setup-check-buffer
             my/git-visit-diffs
             my/git-visit-diffs-prev
             my/git-visit-diffs-next)
  :bind (:map ctl-x-map
              ("v" . magit-status)
              :map magit-mode-map
              (("C-c C-a" . magit-just-amend)
               ("c" . magit-maybe-commit)
               ("q" . magit-quit-session)))
  :if (executable-find "git")
  :load-path (lambda () (expand-file-name "magit/lisp" user-emacs-directory))
  :init (progn
          (setenv "GIT_PAGER" "")

          (eval-after-load 'info
            '(progn (info-initialize)
                    (add-to-list 'Info-directory-list (expand-file-name "magit/Documentation" user-emacs-directory))))

          ;; https://github.com/syl20bnr/spacemacs/issues/7225
          (setq with-editor-file-name-history-exclude '("1"))

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
                  magit-completing-read-function 'ivy-completing-read
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
                  magit-refs-show-commit-count 'all
                  git-commit-fill-column 120
                  git-commit-summary-max-length 80
                  auto-revert-verbose nil)

            ;; Customize lighters
            (delight
             '((magit-diff-mode "Magit Diff")
               (magit-log-mode "Magit Log")
               (magit-popup-mode "Magit Popup")
               (magit-status-mode "Magit Status")))

            ;; Face setup
            (set-face-foreground 'magit-hash (face-foreground 'font-lock-type-face))

            ;; defaults for popups
            (setq magit-branch-popup-show-variables nil)
            (defconst magit-pull-request-remote "upstream"
              "Where to find pull requests.")

            ;; From https://github.com/tarsius/magit-rockstar
            ;; Show worktree section if there are worktrees, avoid overhead if
            ;; there aren't.
            (defvar-local my/magit-want-worktrees t)
            (put 'my/magit-want-worktrees 'permanent-local t)
            (defun my/magit-maybe-add-worktrees ()
              (if (and my/magit-want-worktrees
                       (not (memq #'magit-insert-worktrees magit-status-sections-hook))
                       (> (length (magit-list-worktrees)) 1))
                  (magit-add-section-hook
                   'magit-status-sections-hook #'magit-insert-worktrees
                   'magit-insert-status-headers 'append 'local)
                (setq-local my/magit-want-worktrees nil)))
            (add-hook 'magit-status-mode-hook #'my/magit-maybe-add-worktrees)

            ;; Show submodule section if there are submodules, avoid overhead if
            ;; there aren't.
            (defvar-local my/magit-want-submodules t)
            (put 'my/magit-want-submodules 'permanent-local t)
            (defun my/magit-maybe-add-submodules ()
              (if (and my/magit-want-submodules
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
                (setq-local my/magit-want-submodules nil)))
            (add-hook 'magit-status-mode-hook #'my/magit-maybe-add-submodules)

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

            ;; https://ekaschalk.github.io/post/pretty-magit
            (defmacro pretty-magit (WORD ICON PROPS &optional NO-PROMPT?)
              "Replace sanitized WORD with ICON, PROPS and by default add to prompts."
              `(prog1
                   (add-to-list 'pretty-magit-alist
                                (list (rx bow (group ,WORD (eval (if ,NO-PROMPT? "" ":"))))
                                      ,ICON ',PROPS))
                 (unless ,NO-PROMPT?
                   (add-to-list 'pretty-magit-prompt (concat ,WORD ": ")))))

            (setq pretty-magit-alist nil)
            (setq pretty-magit-prompt nil)
            (pretty-magit "Feature" ? (:foreground "slate gray" :height 1.1))
            (pretty-magit "Add"     ? (:foreground "#375E97" :height 1.1))
            (pretty-magit "Fix"     ? (:foreground "#FB6542" :height 1.1))
            (pretty-magit "Clean"   ? (:foreground "#FFBB00" :height 1.1))
            (pretty-magit "Docs"    ? (:foreground "#3F681C" :height 1.1))
            (pretty-magit "master"  ? (:box nil :height 1.1) t)
            (pretty-magit "origin"  ? (:box nil :height 1.1) t)

            (defun my/add-magit-faces ()
              "Add face properties and compose symbols for buffer from pretty-magit."
              (interactive)
              (with-silent-modifications
                (--each pretty-magit-alist
                  (-let (((rgx icon props) it))
                    (save-excursion
                      (goto-char (point-min))
                      (while (search-forward-regexp rgx nil t)
                        (compose-region
                         (match-beginning 1) (match-end 1) icon)
                        (when props
                          (add-face-text-property
                           (match-beginning 1) (match-end 1) props))))))))

            (advice-add 'magit-status :after 'my/add-magit-faces)
            (advice-add 'magit-refresh-buffer :after 'my/add-magit-faces)

            (setq my/use-magit-commit-prompt-p nil)
            (defun my/use-magit-commit-prompt (&rest args)
              (setq my/use-magit-commit-prompt-p t))

            (defun my/magit-commit-prompt ()
              "Magit prompt and insert commit header with faces."
              (interactive)
              (when my/use-magit-commit-prompt-p
                (setq my/use-magit-commit-prompt-p nil)
                (insert (ivy-read "Commit Type " pretty-magit-prompt
                                  :require-match t :sort t :preselect "Add: "))
                (my/add-magit-faces)))

            (remove-hook 'git-commit-setup-hook 'with-editor-usage-message)
            (add-hook 'git-commit-setup-hook 'my/magit-commit-prompt)
            (advice-add 'magit-commit :after 'my/use-magit-commit-prompt)

            ;; This extension finds all the changes ("hunks") between a working copy of a
            ;; git repository and a reference (branch name or hashref) and presents them
            ;; as a series of narrowed buffers.
            ;;
            ;; Interactive functions:
            ;;
            ;; my/git-visit-diffs (ref) : prompts for a ref, to be passed to `git diff`;
            ;; collects the hunks and view the first hunk, if any. Operates from the
            ;; current directory.
            ;; my/git-visit-diffs-next () : view the next hunk, if any.
            ;; my/git-visit-diffs-prev () : view the previous hunk, if any.

            ;; Global variables and types
            (defvar *git-visit-current-hunk-list*)
            (defvar *git-visit-previous-hunk-list*)
            (define-error 'git-error "Git error")

            ;; Utility functions
            (defun my/git-visit-visit-modified-region (filename line count)
              "Visit a hunk in a narrowed buffer"
              (find-file filename)
              (widen)
              (goto-line line)
              (beginning-of-line)
              (let ((beginning-position (line-beginning-position)))
                (next-line count)
                (narrow-to-region beginning-position (line-beginning-position))
                (beginning-of-buffer)))

            (defun my/git-visit-get-hunk-list (dir ref)
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
            (defun my/git-visit-diffs-next ()
              "Show next diff in narrowed buffer."
              (interactive)
              (if *git-visit-current-hunk-list*
                  (let ((next-hunk (car *git-visit-current-hunk-list*)))
                    (setq *git-visit-previous-hunk-list* (cons next-hunk *git-visit-previous-hunk-list*))
                    (setq *git-visit-current-hunk-list* (cdr *git-visit-current-hunk-list*))
                    (apply 'my/git-visit-visit-modified-region next-hunk))
                (message "At end of hunk list")))

            (defun my/git-visit-diffs-prev ()
              "Show previous diff in narrowed buffer."
              (interactive)
              (if (and *git-visit-previous-hunk-list* (cdr *git-visit-previous-hunk-list*))
                  (let ((next-hunk (cadr *git-visit-previous-hunk-list*)))
                    (setq *git-visit-current-hunk-list* (cons (car *git-visit-previous-hunk-list*) *git-visit-current-hunk-list*))
                    (setq *git-visit-previous-hunk-list* (cdr *git-visit-previous-hunk-list*))
                    (apply 'my/git-visit-visit-modified-region next-hunk))
                (message "At beginning of hunk list")))

            (defun my/git-visit-diffs (ref)
              "Finds the diffs between REF and working copy. Shows the first diff in a narrowed buffer."
              (interactive "sref: ")
              (let ((dir (ignore-errors
                           (file-name-as-directory (car (process-lines "git" "rev-parse" "--show-toplevel"))))))
                (if (not dir)
                    (error (message "%s" "Cannot locate repository root"))
                  (condition-case error
                      (progn
                        (setq *git-visit-current-hunk-list* (my/git-visit-get-hunk-list dir ref))
                        (setq *git-visit-previous-hunk-list* nil)
                        (my/git-visit-diffs-next))
                    (git-error (message (cadr error)))))))))

;; magit integration with git flow
(use-package magit-gitflow
  :defer t
  :commands (turn-on-magit-gitflow)
  :if (executable-find "git")
  :after magit
  :load-path (lambda () (expand-file-name "magit-gitflow/" user-emacs-directory))
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

;; git modeline and git utilities
(use-package git-emacs
  :if (executable-find "git")
  :load-path (lambda () (expand-file-name "git-emacs/" user-emacs-directory))
  :config (use-package git-modeline))

;; git-timemachine
(use-package git-timemachine
  :defer t
  :commands (my/git-timemachine-start
             git-timemachine-toggle
             git-timemachine-switch-branch)
  :load-path (lambda () (expand-file-name "git-timemachine/" user-emacs-directory))
  :if (executable-find "git")
  :after magit
  :config (progn
            (defun my/git-timemachine-show-selected-revision ()
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

            (defun my/git-timemachine-start ()
              "Open git snapshot with the selected version. Based on ivy-mode."
              (interactive)
              (git-timemachine--start #'my/git-timemachine-show-selected-revision))))

;; Show blame for current line
(use-package git-messenger
  :if (executable-find "git")
  :load-path (lambda () (expand-file-name "git-messenger/" user-emacs-directory)))

;; Show git state for individual lines on the margin
(use-package git-gutter+
  :if (executable-find "git")
  :diminish git-gutter+-mode
  :after magit
  :load-path (lambda () (expand-file-name "git-gutter-plus/" user-emacs-directory))
  :config (progn
            ;; auto-save-buffer calls write-file which doesn't naturally call the git-gutter refresh fn
            (defadvice write-file (after write-file-git-gutter-mode activate) (git-gutter+-refresh))
            (defadvice refresh-file (after write-file-git-gutter-mode activate) (git-gutter+-refresh))

            ;; http://stackoverflow.com/questions/23344540/emacs-update-git-gutter-annotations-when-staging-or-unstaging-changes-in-magit
            (defvar my/magit-after-stage-hooks nil
              "Hooks to be run after staging one item in magit.")

            (defvar my/magit-after-unstage-hooks nil
              "Hooks to be run after unstaging one item in magit.")

            (defun my/refresh-visible-git-gutter-buffers ()
              "Refresh git-gutter-mode on all visible git-gutter-mode buffers."
              (dolist (buff (buffer-list))
                (with-current-buffer buff
                  (when (and git-gutter+-mode (get-buffer-window buff))
                    (git-gutter+-mode t)))))

            (add-hook 'my/magit-after-unstage-hooks
                      'my/refresh-visible-git-gutter-buffers)
            (add-hook 'my/magit-after-stage-hooks
                      'my/refresh-visible-git-gutter-buffers)

            (global-git-gutter+-mode t)))

;; Use fringe instead of margin for git-gutter-plus
(use-package git-gutter-fringe+
  :if (display-graphic-p)
  :after git-gutter+
  :load-path (lambda () (expand-file-name "git-gutter-fringe-plus/" user-emacs-directory))
  :config (progn
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

;; highlight regions according to age
(use-package smeargle
  :if (executable-find "git")
  :commands (smeargle smeargle-commits smeargle-clear)
  :load-path (lambda () (expand-file-name "smeargle/" user-emacs-directory)))

(provide 'setup-versioning)
;;; setup-versioning.el ends here
