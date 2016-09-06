;;; sudo-edit.el --- Open files as another user       -*- lexical-binding: t -*-

;; Copyright (C) 2014 Nathaniel Flath <flat0103@gmail.com>

;; Author: Nathaniel Flath <flat0103@gmail.com>
;; URL: https://github.com/nflath/sudo-edit
;; Keywords: convenience
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package allows to open files as another user, by default "root":
;;
;;     `sudo-edit'
;;
;; Suggested keybinding:
;;
;;     (global-set-key (kbd "C-c C-r") 'sudo-edit)
;;
;; Installation:
;;
;; To use this mode, put the following in your init.el:
;;
;;     (require 'sudo-edit)

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x nil 'no-error))

(require 'tramp)

;; Compatibility for Emacs 24.3 and earlier
(eval-and-compile
  (unless (fboundp 'string-prefix-p)
    (defun string-prefix-p (prefix string &optional ignore-case)
      "Return non-nil if PREFIX is a prefix of STRING.
If IGNORE-CASE is non-nil, the comparison is done without paying attention
to case differences."
      (let ((prefix-length (length prefix)))
        (if (> prefix-length (length string)) nil
          (eq t (compare-strings prefix 0 prefix-length string
                                 0 prefix-length ignore-case))))))

  (unless (fboundp 'string-suffix-p)
    (defun string-suffix-p (suffix string  &optional ignore-case)
      "Return non-nil if SUFFIX is a suffix of STRING.
If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
      (let ((start-pos (- (length string) (length suffix))))
        (and (>= start-pos 0)
             (eq t (compare-strings suffix nil nil
                                    string start-pos nil ignore-case))))))

  (unless (featurep 'subr-x)
    (defsubst string-remove-prefix (prefix string)
      "Remove PREFIX from STRING if present."
      (if (string-prefix-p prefix string)
          (substring string (length prefix))
        string))

    (defsubst string-remove-suffix (suffix string)
      "Remove SUFFIX from STRING if present."
      (if (string-suffix-p suffix string)
          (substring string 0 (- (length string) (length suffix)))
        string))

    (defsubst string-blank-p (string)
      "Check whether STRING is either empty or only whitespace."
      (string-match-p "\\`[ \t\n\r]*\\'" string))))

(defgroup sudo-edit nil
  "Edit files as another user."
  :prefix "sudo-edit-"
  :group 'convenience)

(defcustom sudo-edit-user "root"
  "Default user to edit a file with `sudo-edit'."
  :type 'string
  :group 'sudo-edit)

(defvar sudo-edit-user-history nil)

(defun sudo-edit-filename (filename user)
  "Return a tramp edit name for a FILENAME as USER."
  (if (file-remote-p filename)
      (let* ((vec (tramp-dissect-file-name filename))
             (hop (tramp-make-tramp-file-name
                   (tramp-file-name-method vec)
                   (tramp-file-name-user vec)
                   (tramp-file-name-host vec)
                   ""
                   (tramp-file-name-hop vec))))
        (setq hop (string-remove-prefix tramp-prefix-format hop))
        (setq hop (string-remove-suffix tramp-postfix-host-format hop))
        (setq hop (concat hop tramp-postfix-hop-format))
        (tramp-make-tramp-file-name "sudo" user (tramp-file-name-host vec) (tramp-file-name-localname vec) hop))
    (tramp-make-tramp-file-name "sudo" user "localhost" (expand-file-name filename))))

;;;###autoload
(defun sudo-edit (&optional arg)
  "Edit currently visited file as another user, by default `sudo-edit-user'.

With a prefix ARG prompt for a file to visit.  Will also prompt
for a file to visit if current buffer is not visiting a file."
  (interactive "P")
  (let ((user (if arg
                  (completing-read "User: " (system-users) nil nil nil 'sudo-edit-user-history sudo-edit-user)
                sudo-edit-user))
        (filename (or buffer-file-name
                      (and (derived-mode-p 'dired-mode) default-directory))))
    (cl-assert (not (string-blank-p user)) nil "User must not be a empty string")
    (if (or arg (not filename))
        (find-file (sudo-edit-filename (read-file-name (format "Find file (as \"%s\"): " user)) user))
      (let ((position (point)))
        (find-alternate-file (sudo-edit-filename filename user))
        (goto-char position)))))

(define-obsolete-function-alias 'sudo-edit-current-file 'sudo-edit)

(provide 'sudo-edit)
;;; sudo-edit.el ends here
