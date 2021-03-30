;;; code-cells.el --- Work with code split into cells and Jupyter notebooks -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Augusto Stoffel

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; Keywords: convenience, outlines
;; URL: https://github.com/astoff/code-cells.el
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; With this package, you can efficiently navigate, edit and execute
;; code split into cells according to certain magic comments.  It also
;; allows to open ipynb notebook files directly in Emacs.  They will
;; be automatically converted to a script for editing, and converted
;; back to notebook format when saving.  An external tool, Jupytext by
;; default, is required for this.
;;
;; Out of the box, there are no keybindings and, in fact, only a small
;; number of editing commands is provided.  Rather, the idea is that
;; you can create your own cell-aware commands from regular ones
;; through the `code-cells-command' function and the `code-cells-do' macro.  See
;; the README for configuration examples.  There is also a
;; `code-cells-mode' minor mode, which, among other things, provides
;; outline support.

;;; Code:

(require 'map)
(require 'pulse)
(require 'subr-x)
(eval-when-compile (require 'rx))

(defgroup code-cells nil
  "Utilities for code split into cells."
  :group 'convenience
  :prefix "code-cells-")

;;* Cell navigation

(defcustom code-cells-boundary-markers
  (list (rx (* space) "%" (group-n 1 (+ "%")))
        (rx (group-n 1 (+ "*")))
        (rx " In[" (* (any space digit)) "]:"))
  "List of regular expressions specifying cell boundaries.
They should match immediately after a comment start at the
beginning of a line.  The length of the first capture determines
the outline level."
  :type '(repeat sexp))

(defun code-cells-boundary-regexp ()
  "Return a regexp matching comment lines that serve as cell boundary."
  (concat (rx line-start (+ (syntax comment-start)))
          "\\(?:"
          (string-join code-cells-boundary-markers "\\|")
          "\\)"))

;;;###autoload
(defun code-cells-forward-cell (&optional arg)
  "Move to the next cell boundary, or end of buffer.
With ARG, repeat this that many times.  If ARG is negative, move
backward."
  (interactive "p")
  (let ((page-delimiter (code-cells-boundary-regexp)))
    (forward-page arg)
    (move-beginning-of-line 1)))

;;;###autoload
(defun code-cells-backward-cell (&optional arg)
  "Move to the previous cell boundary, or beginning of buffer.
With ARG, repeat this that many times.  If ARG is negative, move
forward."
  (interactive "p")
  (code-cells-forward-cell (- (or arg 1))))

;;;###autoload
(defmacro code-cells-do (&rest body)
  "Find current cell bounds and evaluate BODY.
Inside BODY, the variables `start' and `end' are bound to the
limits of the current cell.

If the first element of BODY is the keyword `:use-region' and the
region is active, use its bounds instead.  In this case,
`using-region' is non-nil in BODY."
  `(pcase (if (and ,(eq (car body) :use-region) (use-region-p))
              (list t (region-end) (region-beginning))
            (save-excursion
              (list nil
                    (progn (code-cells-forward-cell) (point))
                    (progn (code-cells-backward-cell) (point)))))
     (`(,using-region ,end ,start)
      ;; Avoid compiler warnings if one of those is unused in body
      (ignore using-region end start)
      ,@body)))

;;;###autoload
(defun code-cells-mark-cell ()
  "Put point at the beginning of this cell, mark at end."
  (interactive)
  (code-cells-do
   (goto-char start)
   (push-mark end nil t)))

;;;###autoload
(defun code-cells-command (fun &rest options)
  "Return an anonymous command that calls FUN on the current cell.

FUN is a function that takes two character positions as argument.
Most interactive commands that act on a region are of this form
and can be used here.

If OPTIONS contains the keyword :use-region, the command will act
on the region instead of the current cell when appropriate.

If OPTIONS contains the keyword :pulse, provide visual feedback
via `pulse-momentary-highlight-region'."
  (eval `(lambda ()
           ,(concat
             "Call `" (symbol-name fun) "' on the current code cell."
             (when (member :use-region options)
               "\nIf region is active, use it instead."))
           (interactive)
           (code-cells-do ,(car (member :use-region options))
                          ,(when (member :pulse options)
                             '(pulse-momentary-highlight-region start end))
                          (funcall ',fun start end)))))

(defun code-cells-speed-key (command)
  "Return a speed key definition, suitable for passing to `define-key'.
The resulting keybinding will only have any effect when the point
is at the beginning of a cell heading, in which case it executes
COMMAND."
  (list 'menu-item nil command
        :filter (lambda (d)
                  (if (and (bolp)
                           (looking-at (code-cells-boundary-regexp)))
                      d))))

;;* Minor mode

(defvar-local code-cells--saved-vars nil
  "A place to save variables before activating `code-cells-mode'.")

(defun code-cells--outline-level ()
  "Compute the outline level, taking code cells into account.
To be used as the value of the variable `outline-level'.

At a cell boundary, returns the cell outline level, as determined by
`code-cells-boundary-markers'.  Otherwise, returns the sum of the
outline level as determined by the major mode and the current cell
level."
  (let* ((at-boundary (looking-at-p (code-cells-boundary-regexp)))
         (mm-level (if at-boundary
                       0
                     (funcall (car code-cells--saved-vars))))
         (cell-level (if (or at-boundary
                             (save-excursion
                               (re-search-backward
                                (code-cells-boundary-regexp) nil t)))
                         (if (match-string 1)
                             (- (match-end 1) (match-beginning 1))
                           1)
                       0)))
    (+ cell-level mm-level)))

(defface code-cells-header-line '((t :extend t :inherit header-line))
  "Face used by `code-cells-mode' to highlight cell boundaries.")

(defun code-cells--font-lock-keywords ()
  "Font lock keywords to highlight cell boundaries."
  `((,(rx (regexp (code-cells-boundary-regexp)) (* any) "\n")
     0 'code-cells-header-line append)))

(defvar outline-heading-end-regexp)

;;;###autoload
(define-minor-mode code-cells-mode
  "Minor mode for cell-oriented code."
  :keymap (make-sparse-keymap)
  (if code-cells-mode
      (progn
        (require 'outline)
        (setq-local code-cells--saved-vars (list outline-level
                                                 outline-regexp
                                                 outline-heading-end-regexp)
                    outline-level 'code-cells--outline-level
                    outline-regexp (rx (or (regexp (code-cells-boundary-regexp))
                                           (regexp outline-regexp)))
                    outline-heading-end-regexp "\n")
        (font-lock-add-keywords nil (code-cells--font-lock-keywords)))
    (setq-local outline-level (nth 0 code-cells--saved-vars)
                outline-regexp (nth 1 code-cells--saved-vars)
                outline-heading-end-regexp (nth 2 code-cells--saved-vars))
    (font-lock-remove-keywords nil (code-cells--font-lock-keywords)))
  (font-lock-flush))

;;* Jupyter notebook conversion

(defcustom code-cells-convert-ipynb-style
  '(("jupytext" "--to" "ipynb")
    ("jupytext" "--to" "auto:percent")
    nil
    code-cells-convert-ipynb-hook)
  "Determines how to convert ipynb files for editing.
The first two entries are lists of strings: the command name and
arguments used, respectively, to convert to and from ipynb
format.

The third entry, if present, specificies the major mode
called after converting from ipynb.  If omitted, the major mode
is determined from the notebook's language.

The fourth entry, also optional, is a hook run after the new
major mode is activated."
  :type '(list sexp sexp sexp sexp))

(defvar code-cells-convert-ipynb-hook '(code-cells-mode)
  "Hook used in the default `code-cells-convert-ipynb-style'.")

(defun code-cells--call-process (buffer command)
  "Pipe BUFFER through COMMAND, with output to the current buffer.
Returns the process exit code.  COMMAND is a list of strings, the
program name followed by arguments."
  (unless (executable-find (car command))
    (error "Can't find %s" (car command)))
  (let ((logfile (make-temp-file "emacs-cells-")))
    (prog1
        (apply #'call-process-region nil nil (car command) nil
               (list buffer logfile) nil
               (cdr command))
      (with-temp-buffer
        (insert-file-contents logfile)
        (when (> (buffer-size) 0)
          (display-warning 'code-cells (buffer-substring-no-properties
                                        (point-min) (point-max))))
        (delete-file logfile)))))

;;;###autoload
(defun code-cells-convert-ipynb ()
  "Convert buffer from ipynb format to a regular script."
  (goto-char (point-min))
  (let* ((nb (json-parse-buffer))
         (pt (point))
         (lang (or (map-nested-elt nb '("metadata" "kernelspec" "language"))
                   (map-nested-elt nb '("metadata" "jupytext" "main_language"))))
         (mode (or (nth 2 code-cells-convert-ipynb-style)
                   (intern (concat lang "-mode"))))
         (exit (code-cells--call-process t (nth 1 code-cells-convert-ipynb-style))))
    (unless (eq 0 exit)
      (delete-region pt (point-max))
      (error "Error converting notebook (exit code %s)" exit))
    (delete-region (point-min) pt)
    (set-buffer-modified-p nil)
    (setq-local write-file-functions '(code-cells-write-ipynb))
    (when (fboundp mode)
      (funcall mode)
      (run-hooks (nth 3 code-cells-convert-ipynb-style)))))

;;;###autoload
(defun code-cells-write-ipynb (&optional file)
  "Convert buffer to ipynb format and write to FILE.
Interactively, asks for the file name.  When called from Lisp,
FILE defaults to the current buffer file name."
  (interactive "F")
  (let* ((file (or file buffer-file-name))
         (temp (generate-new-buffer " *cells--call-process output*"))
         (exit (code-cells--call-process temp (nth 0 code-cells-convert-ipynb-style))))
    (unless (eq 0 exit)
        (error "Error converting notebook (exit code %s)" exit))
    (with-current-buffer temp
      (write-region nil nil file)
      (kill-buffer))
    (when (eq file buffer-file-name)
      (set-buffer-modified-p nil)
      (set-visited-file-modtime))
    'job-done))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ipynb\\'" . code-cells-convert-ipynb))

(provide 'code-cells)
;;; code-cells.el ends here
