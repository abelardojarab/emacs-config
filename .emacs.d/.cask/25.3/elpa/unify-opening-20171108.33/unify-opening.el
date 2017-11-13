;;; unify-opening.el --- Unify the mechanism to open files

;; Copyright (C) 2015 Damien Cassou

;; Author: Damien Cassou <damien.cassou@gmail.com>
;; Url: https://github.com/DamienCassou/unify-opening
;; Package-Version: 20171108.33
;; GIT: https://github.com/DamienCassou/unify-opening
;; Version: 2.0.0
;; Package-Requires: ((emacs "24.4"))
;; Created: 16 Jan 2015

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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
;; Make everything use the same mechanism to open files.  Currently, `dired` has
;; its mechanism, `org-mode` uses something different (the `org-file-apps`
;; variable), and `mu4e` something else (a simple prompt).  This package makes
;; sure that each package uses the mechanism of `dired`.  I advise you to
;; install the [`runner`](https://github.com/thamer/runner) package to improve
;; the `dired` mechanism.

;;; Code:

;; Avoid warnings about undefined variables and functions
(eval-when-compile
  (defvar org-file-apps)
  (declare-function dired-do-async-shell-command "dired-aux")
  (declare-function dired-guess-default "dired-x")
  (declare-function dired-guess-shell-command "dired-x")
  (declare-function helm "ext:helm")
  (declare-function mm-handle-filename "mm-decode")
  (declare-function mm-interactively-view-part "mm-decode")
  (declare-function mm-save-part-to-file "mm-decode")
  (declare-function counsel-locate-action-extern "counsel"))

(defun unify-opening-find-cmd (file)
  "Return a string representing the best command to open FILE.
This method uses `dired-guess-shell-command'.  The runner package, which I
  recommend, will modify the behavior of `dired-guess-shell-command' to
  work better."
  (require 'dired-x)
  (dired-guess-shell-command (format  "Open %s " file) (list file)))

(defun unify-opening-open (file &optional cmd)
  "Open FILE with CMD if provided, ask for best CMD if not.
Asking for best CMD to use to open FILE is done through
`unify-opening-find-cmd'."
  (let ((cmd (or cmd (unify-opening-find-cmd file))))
    (require 'dired-aux)
    (dired-do-async-shell-command cmd 0 (list file))))

(defun unify-opening-mm-interactively-view-part (handle)
  "Une unify-opening to display HANDLE.
Designed to replace `mm-interactively-view-part'."
  (let ((tmpfile (make-temp-file
                  "emacs-mm-part-"
                  nil
                  (mm-handle-filename handle))))
    (mm-save-part-to-file handle tmpfile)
    (unify-opening-open tmpfile)))

(with-eval-after-load "mm-decode"
  (advice-add #'mm-interactively-view-part :override #'unify-opening-mm-interactively-view-part))

(defun unify-opening--org-default-opener (file link)
  "Open FILE with unify-opening.  Ignore LINK."
  (unify-opening-open file))

(with-eval-after-load "org"
  (add-to-list 'org-file-apps '(t . unify-opening--org-default-opener)))

;;; When listing files from Helm, make sure the "Open file externally" action
;;; uses `unify-opening'.
(with-eval-after-load "helm-external"
  (defun unify-opening-helm-get-default-program-for-file (filename)
    "Use `unify-opening-find-cmd' to select which command to open files with.
This method will be triggered when typing\\<helm-find-files-map> \\[helm-ff-run-open-file-externally] during execution of `helm-find-files' (\\<global-map>\\[helm-find-files])."
    (unify-opening-find-cmd filename))
  (advice-add
   'helm-get-default-program-for-file
   :override
   'unify-opening-helm-get-default-program-for-file))

(defun unify-opening-guess-shell-command (files)
  "Ask user which command to use to open FILES.

Guess a list of suited commands to open FILES, then present the list to the
user so s·he can choose."
  (let ((commands (dired-guess-default files)))
    (when (consp commands)
      (completing-read "command: " commands nil))))

(defun unify-opening-dired-guess-shell-command (original-fun prompt files)
  "Advice ORIGINAL-FUN to ask user with PROMPT for a shell command, guessing a default from FILES."
  (or (unify-opening-guess-shell-command files)
      (funcall original-fun prompt files)))

(with-eval-after-load "dired-x"
  (advice-add #'dired-guess-shell-command :around #'unify-opening-dired-guess-shell-command))

(with-eval-after-load "counsel"
  (advice-add #'counsel-locate-action-extern :override #'unify-opening-open))

(provide 'unify-opening)

;;; unify-opening.el ends here
