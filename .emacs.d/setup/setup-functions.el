;;; setup-functions.el ---                   -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018, 2022  Abelardo Jara-Berrocal

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
(defvar after-find-file-from-revert-buffer nil)

(defun -rpartial (fn &rest args)
    "Takes a function FN and fewer than the normal arguments to FN,
and returns a fn that takes a variable number of additional ARGS.
When called, the returned function calls FN with the additional
args first and then ARGS."
      (lambda (&rest args-before) (apply fn (append args-before args))))

(defun custom-add-choice (variable choice)
    "Add CHOICE to the custom type of VARIABLE.
If a choice with the same tag already exists, no action is taken."
    (let ((choices (get variable 'custom-type)))
      (unless (eq (car choices) 'choice)
        (error "Not a choice type: %s" choices))
      (unless (seq-find (lambda (elem)
                          (equal (caddr (member :tag elem))
                                 (caddr (member :tag choice))))
                        (cdr choices))
        ;; Put the new choice at the end.
        (put variable 'custom-type
             (append choices (list choice))))))

;; Missing variables
(defvar scheme-imenu-generic-expression "")

(defvar cursor-sensor-inhibit nil)

(defvar debian-aspell-only-dictionary-alist nil)

(defvar my/switch-buffer-ignore-dired t)

;; Set this constant to the number of times `update-progress-bar' is called during init
(defconst my/loading-step-count 22)
(defconst my/loading-step-size
  (/ (window-total-size nil 'width) my/loading-step-count))
(defconst my/loading-char ?*)
(defvar my/loading-string "")
(defvar my/start-time (current-time))
(defvar my/progress-bar nil)

;; Required functions
(use-package setup-functions-required
  :defer t
  :commands (cl--set-getf
             font-lock-flush
             special-form-p
             define-error
             internet-up-p
             my/tabs-setup))

;; Deferred functions
(use-package setup-functions-deferred
  :defer t
  :commands (goto-eol
             goto-bol
             line-beginning-position
             line-end-position
             regexp-match-p
             insert-date-string
             toggle-line-spacing
             refresh-file
             put-file-name-on-clipboard
             remove-trailing-spaces
             dos2unix
             unix2dos
             beautify-region
             beautify-buffer
             goto-match-paren-or-up
             goto-match-paren
             indent-block
             unindent-block
             shift-region
             unindent-block-or-line
             rename-file-and-buffer
             delete-buffer-and-file
             number-rectangle
             toggle-selective-display
             my/next-user-buffer
             my/previous-user-buffer
             rotate-windows
             update-progress-bar
             my/directory-tree
             my/read-file-lines
             my/read-file-as-string
             my/parent-directory
             insert-timestamp
             insert-datestamp-us
             insert-datestamp-us-full-year
             insert-datestamp-us-full-year-and-dashes
             org-time-stamp-with-seconds-now))

;; Needed later by CEDET
(use-package eieio-core)

;; String manipulation functions
(use-package subr-x
  :defer t
  :commands (string-trim-left
             string-trim-right
             string-trim-right
         string-trim
         string-truncate
             string-suffix-p)
  :config (progn
        (put 'if-let   'byte-obsolete-info nil)
        (put 'when-let 'byte-obsolete-info nil)

            (unless (fboundp 'string-suffix-p)
              (defun string-suffix-p (suffix string  &optional ignore-case)
                "Return non-nil if SUFFIX is a suffix of STRING.
If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
                (let ((start-pos (- (length string) (length suffix))))
                  (and (>= start-pos 0)
                       (eq t (compare-strings suffix nil nil
                                              string start-pos nil ignore-case))))))

            ;; Other string functions introduced in Emacs 24.4:
            (unless (fboundp 'string-trim-left)
              (defsubst string-trim-left (string)
                "Remove leading whitespace from STRING."
                (if (string-match "\\`[ \t\n\r]+" string)
                    (replace-match "" t t string)
                  string)))

            (unless (fboundp 'string-trim-right)
              (defsubst string-trim-right (string)
                "Remove trailing whitespace from STRING."
                (if (string-match "[ \t\n\r]+\\'" string)
                    (replace-match "" t t string)
                  string)))

            (unless (fboundp 'string-trim)
              (defsubst string-trim (string)
                "Remove leading and trailing whitespace from STRING."
                (string-trim-left (string-trim-right string))))

            (unless (fboundp 'string-truncate)
              (defun string-truncate (string n)
                "Return STRING minus the last N characters."
                (substring string 0 (max 0(- (length string) n)))))))

(defun my/page-down ()
  (interactive)
  (next-line
   (- (window-text-height)
      next-screen-context-lines)))

(defun my/page-up ()
  (interactive)
  (previous-line
   (- (window-text-height)
      next-screen-context-lines)))

(defun screen-size ()
  (let ((screen-width 0) (screen-height 0))
    (dolist (attrs (display-monitor-attributes-list))
      (let* ((geometry (cdr (assq 'geometry attrs)))
             (right (+ (nth 0 geometry) (nth 2 geometry)))
             (bottom (+ (nth 1 geometry) (nth 3 geometry))))
        (when (> right screen-width) (setq screen-width right))
        (when (> bottom screen-height) (setq screen-height bottom))))
    (list screen-width screen-height)))

(provide 'setup-functions)
;;; setup-utilities.el ends here
