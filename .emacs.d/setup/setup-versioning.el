;;; setup-versioning.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Abelardo Jara-Berrocal

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

;; Follow symbolic links
(use-package vc
  :demand t
  :config (progn
            (setq vc-follow-symlinks t)

            ;; Ignore errors in vc-exec-after
            (defadvice vc-exec-after (around bar activate)
              (ignore-errors add-do-it))))

;; Display commit number that is associated with current line of code
(use-package vc-msg
  :defer t
  :after (vc popup)
  :commands vc-msg-show
  :config (progn
            (defun vc-msg-hook-setup (vcs-type commit-info)
              ;; copy commit id to clipboard
              (message (format "%s\n%s\n%s\n%s"
                               (plist-get commit-info :id)
                               (plist-get commit-info :author)
                               (plist-get commit-info :author-time)
                               (plist-get commit-info :author-summary))))
            (add-hook 'vc-msg-hook #'vc-msg-hook-setup)))

;; Annotate lines with author history
(use-package vc-annotate
  :defer t
  :after vc
  :commands vc-annotate
  :config (defun vc-annotate-get-time-set-line-props ()
            (let ((bol (point))
                  (date (vc-call-backend vc-annotate-backend 'annotate-time))
                  (inhibit-read-only t))
              (assert (>= (point) bol))
              (put-text-property bol (point) 'invisible 'vc-annotate-annotation)
              (when (string-equal "Git" vc-annotate-backend)
                (save-excursion
                  (goto-char bol)
                  (search-forward "(")
                  (let ((p1 (point)))
                    (re-search-forward " [0-9]")
                    (remove-text-properties p1 (1- (point)) '(invisible nil))
                    )))
              date)))

;; psvn
(use-package psvn
  :defer t
  :after vc
  :config (setq svn-status-hide-unmodified        t
                svn-status-hide-unknown           t
                svn-status-svn-file-coding-system 'utf-8))

;; git-modes, not available in melpa
(use-package git-modes
  :demand t
  :after vc
  :if (executable-find "git")
  :load-path (lambda () (expand-file-name "git-modes/" user-emacs-directory)))

;; Get missing magit-get-submodules
(use-package magit-git
  :commands magit-get-submodules
  :config (defun magit-get-submodules ()
            (--mapcat (and (string-match "^160000 [0-9a-z]\\{40\\} 0\t\\(.+\\)$" it)
                           (list (match-string 1 it)))
                      (magit-git-items "ls-files" "-z" "--stage"))))

;; magit
(use-package magit
  :defer t
  :commands (projectile-vc
             magit-init
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
             magit-after-save-refresh-status
             magit-after-revert-hook
             magit-not-reverted-hook
             git-commit-setup-check-buffer)
  :defines (magit-ediff-dwim-show-on-hunks)
  :bind (:map ctl-x-map
              ("m"        . magit-status)
              :map magit-mode-map
              (("C-c C-a" . magit-just-amend)
               ("c"       . magit-maybe-commit)
               ("q"       . magit-quit-session)))
  :if (executable-find "git")
  :init (progn
          (setenv "GIT_PAGER" "")
          (setq with-editor-file-name-history-exclude '("1"))

          ;; Enable line highlight
          (add-hook 'magit-mode-hook #'hl-line-mode)

          ;; Refresh status
          (add-hook 'after-save-hook 'magit-after-save-refresh-status t)

          ;; we no longer need vc-git
          (delete 'Git vc-handled-backends)

          ;; make magit status go full-screen but remember previous window
          (defadvice magit-status (around magit-fullscreen activate)
            (window-configuration-to-register :magit-fullscreen)
            ad-do-it
            (delete-other-windows))

          ;; Make `magit-log' run alone in the frame, and then restore the old window
          ;; configuration when you quit out of magit.
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

          ;; Close popup when commiting
          (defadvice git-commit-commit (after delete-window activate)
            (delete-window))

          (defadvice git-commit-abort (after delete-window activate)
            (delete-window))

          ;; these two force a new line to be inserted into a commit window,
          ;; which stops the invalid style showing up.
          (defun magit-commit-mode-init ()
            (when (looking-at "\n")
              (open-line 1)))

          (add-hook 'git-commit-mode-hook #'magit-commit-mode-init))
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

            ;; Show gravatars
            (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

            ;; Always show recent/unpushed/unpulled commits
            (setq magit-section-initial-visibility-alist '((unpushed . show)
                                                           (unpulled . show)))

            ;; Face setup
            (set-face-foreground 'magit-hash (face-foreground 'font-lock-type-face))

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

            ;; Prettify magit interface
            (when (display-graphic-p)
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
              (pretty-magit "Start new feature"   ?⮑ (:foreground "slate gray" :height 1.1))
              (pretty-magit "Continue feature"    ?⭇ (:foreground "#375E97" :height 1.1))
              (pretty-magit "Fix bug"             ?⚒ (:foreground "#FB6542" :height 1.1))
              (pretty-magit "Refactor"            ?♽ (:foreground "#FFBB00" :height 1.1))
              (pretty-magit "Docs"                ?🕮 (:foreground "#3F681C" :height 1.1))
              (pretty-magit "Tag"                 ? (:foreground "#3F681C" :height 1.1))
              (pretty-magit "master"              ?⯎ (:box nil :height 1.0) t)
              (pretty-magit "origin"              ?⭗ (:box nil :height 1.0) t)

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
              (add-hook 'git-commit-setup-hook #'my/magit-commit-prompt)
              (advice-add 'magit-commit :after 'my/use-magit-commit-prompt))

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
                  hunk-list)))))

;; magit integration with git flow
(use-package magit-gitflow
  :defer t
  :commands (turn-on-magit-gitflow)
  :if (executable-find "git")
  :after magit
  :init (add-hook 'magit-mode-hook #'turn-on-magit-gitflow 'append))

;; flydiff
(use-package diff-hl-flydiff
  :commands diff-hl-flydiff-mode)

;; diff-hl
(use-package diff-hl
  :after (diff-hl-flydiff magit)
  :commands (global-diff-hl-mode
             diff-hl-mode
             diff-hl-next-hunk
             diff-hl-previous-hunk
             diff-hl-mark-hunk
             diff-hl-diff-goto-hunk
             diff-hl-revert-hunk
             diff-hl-flydiff-mode
             diff-hl-magit-post-refresh
             diff-hl-dired-mode)
  :hook (((prog-mode conf-mode vc-dir-mode ledger-mode) . turn-on-diff-hl-mode)
         (magit-post-refresh                            . diff-hl-magit-post-refresh))
  :config (progn
            (use-package diff-hl-dired
              :hook (dired-mode . diff-hl-dired-mode))

            (setq diff-hl-draw-borders t)
            (defadvice svn-sttus-update-modeline (after svn-update-diff-hl activate)
              (diff-hl-update))

            ;; highlight in unsaved buffers as well
            (diff-hl-flydiff-mode 1)))

;; git modeline and git utilities, not in melpa
(use-package git-emacs
  :if (executable-find "git")
  :load-path (lambda () (expand-file-name "git-emacs/" user-emacs-directory))
  :config (use-package git-modeline))

;; gist
(use-package gist
  :defer t
  :if (executable-find "git")
  :diminish gist-list
  :commands (gist-list gist-region-or-buffer))

;; git-timemachine
(use-package git-timemachine
  :disabled t
  :defer t
  :commands (my/git-timemachine-start
             git-timemachine-toggle
             git-timemachine-switch-branch)
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

;; Show blame for current lines
(use-package git-messenger
  :if (executable-find "git")
  :commands (git-messenger:popup-message)
  :custom (git-messenger:show-detail t)
  :config (define-key git-messenger-map (kbd "RET") 'git-messenger:popup-close))

;; Show git state for individual lines on the margin
(use-package git-gutter
  :if (executable-find "git")
  :diminish git-gutter-mode
  :init (global-git-gutter-mode t)
  :commands global-git-gutter-mode
  :hook (magit-post-refresh . git-gutter:update-all-windows)
  :config (progn

            (add-hook 'git-gutter:update-hooks 'magit-after-revert-hook)
            (add-hook 'git-gutter:update-hooks 'magit-not-reverted-hook)

            (defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                                                  :hint nil)
              "
Git gutter:
  _j_: next hunk        _s_tage hunk     _q_uit
  _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _p_opup hunk
  _h_: first hunk
  _l_: last hunk        set start _R_evision
"
              ("j" git-gutter:next-hunk)
              ("k" git-gutter:previous-hunk)
              ("h" (progn (goto-char (point-min))
                          (git-gutter:next-hunk 1)))
              ("l" (progn (goto-char (point-min))
                          (git-gutter:previous-hunk 1)))
              ("s" git-gutter:stage-hunk)
              ("r" git-gutter:revert-hunk)
              ("p" git-gutter:popup-hunk)
              ("R" git-gutter:set-start-revision)
              ("q" nil :color blue)
              ("Q" (progn (git-gutter-mode -1)
                          ;; git-gutter-fringe doesn't seem to
                          ;; clear the markup right away
                          (sit-for 0.1)
                          (git-gutter:clear))
               :color blue))

            ;; Use fringe instead of margin for git-gutter-plus
            (use-package git-gutter-fringe
              :demand t
              :if (and (executable-find "git")
                       (display-graphic-p))
              :after git-gutter
              :config (progn

                        ;; Please adjust fringe width if your own sign is too big.
                        (setq-default left-fringe-width  20)
                        (setq-default right-fringe-width 20)
                        (setq git-gutter-fr:side 'left-fringe)
                        (set-face-foreground 'git-gutter-fr:modified "yellow")
                        (set-face-foreground 'git-gutter-fr:added    "blue")
                        (set-face-foreground 'git-gutter-fr:deleted  "white")

                        (fringe-helper-define 'git-gutter-fr:added nil
                          ".XXXXXX."
                          "XXxxxxXX"
                          "XX....XX"
                          "XX....XX"
                          "XXXXXXXX"
                          "XXXXXXXX"
                          "XX....XX"
                          "XX....XX")

                        (fringe-helper-define 'git-gutter-fr:deleted nil
                          "XXXXXX.."
                          "XXXXXXX."
                          "XX...xXX"
                          "XX....XX"
                          "XX....XX"
                          "XX...xXX"
                          "XXXXXXX."
                          "XXXXXX..")

                        (fringe-helper-define 'git-gutter-fr:modified nil
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
                          (defadvice git-gutter-process-diff (before git-gutter-process-diff-advice activate)
                            (ad-set-arg 0 (file-truename (ad-get-arg 0)))))

                        ;; Start git gutter mode
                        (global-git-gutter-mode t)))))

;; Highlight regions according to age
(use-package smeargle
  :defer t
  :if (executable-find "git")
  :commands (smeargle smeargle-commits smeargle-clear))

;; Smerge, resolve merge conflicts
(use-package smerge-mode
  :defer t
  :if (executable-find "git")
  :commands (hydra-smerge/body smerge-mode)
  :init (progn
          (defun my/enable-smerge-maybe ()
            "Auto-enable `smerge-mode' when merge conflict is detected."
            (save-excursion
              (goto-char (point-min))
              (when (re-search-forward "^<<<<<<< " nil :noerror)
                (smerge-mode 1))))
          (add-hook 'find-file-hook #'my/enable-smerge-maybe :append))
  :config (progn
            (ignore-errors
              (>=e "26.0"
                   nil
                   (defalias 'smerge-keep-upper 'smerge-keep-mine)
                   (defalias 'smerge-keep-lower 'smerge-keep-other)
                   (defalias 'smerge-diff-base-upper 'smerge-diff-base-mine)
                   (defalias 'smerge-diff-upper-lower 'smerge-diff-mine-other)
                   (defalias 'smerge-diff-base-lower 'smerge-diff-base-other)))

            (defhydra hydra-smerge (:color pink
                                           :hint nil
                                           :pre (smerge-mode 1)
                                           ;; Disable `smerge-mode' when quitting hydra if
                                           ;; no merge conflicts remain.
                                           :post (smerge-auto-leave))
              "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
              ("n" smerge-next)
              ("p" smerge-prev)
              ("b" smerge-keep-base)
              ("u" smerge-keep-upper)
              ("l" smerge-keep-lower)
              ("a" smerge-keep-all)
              ("RET" smerge-keep-current)
              ("\C-m" smerge-keep-current)
              ("<" smerge-diff-base-upper)
              ("=" smerge-diff-upper-lower)
              (">" smerge-diff-base-lower)
              ("R" smerge-refine)
              ("E" smerge-ediff)
              ("C" smerge-combine-with-next)
              ("r" smerge-resolve)
              ("k" smerge-kill-current)
              ("q" nil "cancel" :color blue))))

(provide 'setup-versioning)
;;; setup-versioning.el ends here
