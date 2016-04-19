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
             projectile-vc
             magit-status-internal)
  :bind (:map ctl-x-map
              ("v" . magit-status))
  :load-path (lambda () (expand-file-name "magit/lisp" user-emacs-directory))
  :init (eval-after-load 'info
          '(progn (info-initialize)
                  (add-to-list 'Info-directory-list (expand-file-name "magit/Documentation" user-emacs-directory))))
  :config (progn
            (setq magit-commit-signoff nil
                  magit-auto-revert-mode nil
                  magit-process-popup-time 5
                  magit-save-some-buffers nil
                  magit-set-upstream-on-push t
                  magit-diff-refine-hunk t
                  magit-auto-revert-mode-lighter nil
                  magit-completing-read-function 'magit-ido-completing-read)

            (add-hook 'git-commit-mode-hook (toggle-save-place 0))))

;; diff-hl
(use-package diff-hl
  :load-path (lambda () (expand-file-name "diff-hl/" user-emacs-directory))
  :config (progn
            (setq diff-hl-draw-borders t)
            (defadvice svn-sttus-update-modeline (after svn-update-diff-hl activate)
              (diff-hl-update))
            (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

;; git-timemachine
(use-package git-timemachine
  :commands (git-timemachine git-timemachine-mode)
  :load-path (lambda () (expand-file-name "git-timemachine/" user-emacs-directory)))

;; Show blame for current line
(use-package git-messenger
  :load-path (lambda () (expand-file-name "git-messenger/" user-emacs-directory)))

(use-package git-gutter-plus
  :defer t
  :if window-system
  :diminish git-gutter+-mode
  :load-path (lambda () (expand-file-name "git-gutter-plus/" user-emacs-directory)))

(use-package git-gutter-fringe+
  :if window-system
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
