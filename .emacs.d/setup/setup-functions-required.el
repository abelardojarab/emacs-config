;;; setup-functions-required.el ---             -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2023  Abelardo Jara-Berrocal

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

(defun dframe-timer-fn ()
  "Called due to the dframe timer.
Evaluates all cached timer functions in sequence."
  (let ((l dframe-client-functions))
    (while (and l (sit-for 0))
      (with-demoted-errors "DFRAME TIMER ERROR: %S"
        (ignore-errors (funcall (car l))))
      (setq l (cdr l)))))

;; Replace expression with the value
(defadvice eval-last-sexp (around replace-sexp (arg) activate)
  "Replace sexp when called with a prefix argument."
  (if arg
      (let ((pos (point)))
        ad-do-it
        (goto-char pos)
        (backward-kill-sexp)
        (forward-sexp))
    ad-do-it))

;; Determine if Internet connection is available
(defun internet-up-p (&optional host)
  (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
                     (if host host "www.google.com"))))

;; Missing cl-lib function
(defun cl--set-getf (plist tag val)
  (let ((p plist))
    (while (and p (not (eq (car p) tag))) (setq p (cdr (cdr p))))
    (if p (progn (setcar (cdr p) val) plist) (list* tag val plist))))

;; Missing function
(when (not (fboundp 'font-lock-flush))
  (defun font-lock-flush ()
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))

;; Missing function
(when (not (fboundp 'display--update-for-mouse-movement))
  (defun display--update-for-mouse-movement (a b)
    nil))

;; Missing function
(when (not (fboundp 'make-variable-frame-local))
  (defun make-variable-frame-local (variable) variable))

;; Missing function
(when (not (fboundp 'special-form-p))
  (defun special-form-p (object)
    "Non-nil if and only if OBJECT is a special form."
    (if (and (symbolp object) (fboundp object))
        (setq object (indirect-function object)))
    (and (subrp object) (eq (cdr (subr-arity object)) 'unevalled))))

;; Missing function
(when (not (fboundp 'define-error))
  (defun define-error (name message &optional parent)
    "Define NAME as a new error signal.
MESSAGE is a string that will be output to the echo area if such an error
is signaled without being caught by a `condition-case'.
PARENT is either a signal or a list of signals from which it inherits.
Defaults to `error'."
    (unless parent (setq parent 'error))
    (let ((conditions
           (if (consp parent)
               (apply #'nconc
                      (mapcar (lambda (parent)
                                (cons parent
                                      (or (get parent 'error-conditions)
                                          (error "Unknown signal `%s'" parent))))
                              parent))
             (cons parent (get parent 'error-conditions)))))
      (put name 'error-conditions
           (delete-dups (copy-sequence (cons name conditions))))
      (when message (put name 'error-message message)))))

;; Utility function
(defun my/tabs-setup (tabs length)
  (setq-default indent-tabs-mode tabs)
  (setq-default tab-width length)
  (setq-default tab-stop-list (number-sequence length 100 length)))

;; Missing ensure-list
(unless (fboundp 'ensure-list)
  (defun ensure-list (object)
    "Return OBJECT as a list.
If OBJECT is already a list, return OBJECT itself.  If it's
not a list, return a one-element list containing OBJECT."
    (declare (side-effect-free error-free))
    (if (listp object)
        object
      (list object))))

(provide 'setup-functions-required)
;;; setup-functions-required.el ends here
