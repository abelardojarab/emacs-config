;; Do not ask to confirm all
(set-default 'magit-stage-all-confirm nil)
(set-default 'magit-unstage-all-confirm nil)

;; Load git configurations
(add-hook 'magit-mode-hook 'magit-load-config-extensions)

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
(add-hook 'git-commit-mode-hook 'magit-commit-mode-init)
(add-hook 'git-commit-mode-hook 'goto-address-mode)

(defun magit-save-and-exit-commit-mode ()
  (interactive)
  (save-buffer)
  (server-edit)
  (delete-window))

(defun magit-exit-commit-mode ()
  (interactive)
  (kill-buffer)
  (delete-window))

(eval-after-load "git-commit-mode"
  '(define-key git-commit-mode-map (kbd "C-c C-k") 'magit-exit-commit-mode))

;; C-c C-a to amend without any prompt
(defun magit-just-amend ()
  (interactive)
  (save-window-excursion
    (magit-with-refresh
     (shell-command "git --no-pager commit --amend --reuse-message=HEAD"))))

(eval-after-load "magit"
  '(define-key magit-status-mode-map (kbd "C-c C-a") 'magit-just-amend))

;; C-x C-k to kill file on line
(defun magit-kill-file-on-line ()
  "Show file on current magit line and prompt for deletion."
  (interactive)
  (magit-visit-item)
  (delete-current-buffer-file)
  (magit-refresh))
(define-key magit-status-mode-map (kbd "C-x C-k") 'magit-kill-file-on-line)

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))
(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;; full screen vc-annotate
(defun vc-annotate-quit ()
  "Restores the previous window configuration and kills the vc-annotate buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :vc-annotate-fullscreen))

(eval-after-load "vc-annotate"
  '(progn
     (defadvice vc-annotate (around fullscreen activate)
       (window-configuration-to-register :vc-annotate-fullscreen)
       ad-do-it
       (delete-other-windows))
     (define-key vc-annotate-mode-map (kbd "q") 'vc-annotate-quit)))

;; ignore whitespace
(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))
(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

;; magit settings
(setq
 ;; use ido to look for branches
 magit-completing-read-function 'magit-ido-completing-read
 ;; don't put "origin-" in front of new branch names by default
 magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
 ;; open magit status in same window as current buffer
 magit-status-buffer-switch-function 'switch-to-buffer
 ;; highlight word/letter changes in hunk diffs
 magit-diff-refine-hunk t
 ;; ask me if I want to include a revision when rewriting
 magit-rewrite-inclusive 'ask
 ;; ask me to save buffers
 magit-save-some-buffers t
 ;; pop the process buffer if we're taking a while to complete
 magit-process-popup-time 10
 ;; ask me if I want a tracking upstream
 magit-set-upstream-on-push 'askifnotset)

;; Show blame for current line
(add-to-list 'load-path "~/.emacs.d/git-messenger")
(require 'git-messenger)

(provide 'setup-magit)
