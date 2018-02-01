;;; setup-functions-required.el ---             -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Abelardo Jara-Berrocal

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

(provide 'setup-functions-required)
;;; setup-functions-required.el ends here
