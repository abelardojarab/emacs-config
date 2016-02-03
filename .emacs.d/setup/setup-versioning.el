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

;; Designsync versioning control
(require 'vc-sync)
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
(add-hook 'dired-after-readin-hook 'dired-sync-symlink-filter)

;; psvn
(add-to-list 'load-path "~/.emacs.d/diff-hl")
(require 'psvn)
(require 'diff-hl)
(defadvice svn-status-update-modeline (after svn-update-diff-hl activate)
  (diff-hl-update))

;; git
(add-to-list 'load-path "~/.emacs.d/git-timemachine")
(add-to-list 'load-path "~/.emacs.d/with-editor")
(add-to-list 'load-path "~/.emacs.d/magit/lisp")
(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list "~/.emacs.d/magit/Documentation")))
(require 'with-editor)
(require 'magit)
(require 'git-timemachine)

;; Show blame for current line
(add-to-list 'load-path "~/.emacs.d/git-messenger")
(require 'git-messenger)

(when window-system
  (add-to-list 'load-path "~/.emacs.d/git-gutter-plus")
  (add-to-list 'load-path "~/.emacs.d/git-gutter-fringe-plus")
  (require 'git-gutter-fringe+)
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
      (ad-set-arg 0 (file-truename (ad-get-arg 0))))))

;; Speed up find file
(remove-hook 'find-file-hooks 'vc-find-file-hook)

;; ibuffer versioning-based groups
(add-to-list 'load-path "~/.emacs.d/ibuffer-vc")
(require 'ibuffer-vc)

(provide 'setup-versioning)
;;; setup-versioning.el ends here
