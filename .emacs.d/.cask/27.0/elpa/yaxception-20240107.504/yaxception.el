;;; yaxception.el --- Provide framework about exception like Java for Elisp

;; Copyright (C) 2013  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: exception error signal
;; URL: https://github.com/aki2o/yaxception
;; Package-Version: 20240107.504
;; Package-Revision: 5941de88b197
;; Package-Requires: ((emacs "28") (dash "2.19.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This extension provides framework about exception for elisp.
;; - try/catch/finally coding style like Java
;; - custom error object
;; - stacktrace like Java
;;
;; For detail, see <https://github.com/aki2o/yaxception/blob/master/README.md>
;;
;; Enjoy!!!

;;; Code:

(require 'cl-lib)
(require 'backtrace)
(require 'dash)


(defvar yaxception-debug-enable nil)
(defvar yaxception-debug-buffer-name "*YAX Debug*")

(cl-defmacro yaxception-debug (msg &rest args)
  `(when yaxception-debug-enable
     (condition-case e
         (with-current-buffer (get-buffer-create yaxception-debug-buffer-name)
           (goto-char (point-max))
           (insert (format ,msg ,@args) "\n"))
       (error (message "[yaxception-debug] %s" (error-message-string e))))))

(defun yaxception:toggle-debug-enable ()
  "Toggle debug is enabled/disabled."
  (interactive)
  (message "Yaxception Debug: %s" (setq yaxception-debug-enable (not yaxception-debug-enable))))

(defun yaxception:clear-debug-log ()
  "Clear debug log."
  (interactive)
  (with-current-buffer (get-buffer-create yaxception-debug-buffer-name)
    (erase-buffer)))


(defvar yaxception-err (gensym))
(defvar yaxception-errsymbol (gensym))
(defvar yaxception-errcatched (gensym))
(defvar yaxception-return-value (gensym))
(defvar yaxception-stack-trace-stores nil)

(defun yaxception-signal-hook-function (error-symbol data)
  (when yaxception-stack-trace-stores
    (let ((signal-hook-function nil))
      (set (nth 0 yaxception-stack-trace-stores) (backtrace-to-string))
      (yaxception-debug "Called signal-hook-function with %s : %s\n%s" error-symbol (nth 0 yaxception-stack-trace-stores) (symbol-value (nth 0 yaxception-stack-trace-stores))))))

(cl-defmacro yaxception:$ (try &rest catch_or_finally)
  "Handle error like Java.
TRY is a `yaxception:try' sexp.
CATCH_OR_FINALLY is a `yaxception:catch' or `yaxception:finally' sexp."
  (declare (indent 0))
  (let (catches finally)
    (condition-case err
        (cl-loop for e in catch_or_finally
                 for s = (when (listp e) (car e))
                 for symbolnm = (when s (format "%s" s))
                 do (cond ((string= symbolnm "yaxception:catch")   (setq catches (append catches (list e))))
                          ((string= symbolnm "yaxception:finally") (setq finally e))))
      (error (message "[yaxception:$] %s" (error-message-string err))))
    `(let* ((,yaxception-err)
            (,yaxception-errcatched nil)
            (signal-hook-function 'yaxception-signal-hook-function)
            (yaxception-stack-trace-stores (append (list (gensym)) yaxception-stack-trace-stores)))
       (unwind-protect
           (condition-case ,yaxception-err
               ,try
             ,@(mapcar 'macroexpand catches))
         (when (not ,yaxception-errcatched)
           (let ((sym (pop yaxception-stack-trace-stores)))
             (when (and (boundp sym) yaxception-stack-trace-stores)
               (set (nth 0 yaxception-stack-trace-stores) (symbol-value sym)))))
         ,finally))))

(cl-defmacro yaxception:$~ (try &rest catch_or_finally)
  "This is obsolete already. Use `yaxception:$'."
  (declare (indent 0))
  `(yaxception:$ ,try ,@catch_or_finally))
(make-obsolete 'yaxception:$~ 'yaxception:$ "20231231")

(cl-defmacro yaxception:try (&rest body)
  "Expand to a bodyform of `condition-case'.
This can be used only in `yaxception:$'."
  (declare (indent 0))
  `(progn ,@body))

(cl-defmacro yaxception:catch (errsymbol errvar &rest body)
  "Expand to a handler of `condition-case'.
This can be used only in `yaxception:$'."
  (declare (indent 2))
  `(,errsymbol (let ((,errvar (list :symbol (car ,yaxception-err)
                                    :data (cdr ,yaxception-err)
                                    :backtrace (symbol-value (pop yaxception-stack-trace-stores)))))
                 (setq ,yaxception-errcatched t)
                 ,@body)))

(cl-defmacro yaxception:finally (&rest body)
  "Expand to a unwindform of `unwind-protect'
This can be used only in `yaxception:$'."
  (declare (indent 0))
  `(progn ,@body))

(cl-defmacro yaxception:throw (err_or_errsymbol &rest args)
  "Call `signal'.
ERR_OR_ERRSYMBOL is the value from `yaxception:catch' or symbol.
ARGS will be pass `signal' and can be accessed by `yaxception:get-prop'."
  (declare (indent 0))
  `(progn
     (cond ((and ,err_or_errsymbol (symbolp ,err_or_errsymbol))
            (signal ,err_or_errsymbol ',args))
           ((and ,err_or_errsymbol (listp ,err_or_errsymbol))
            ;; re-throw err
            (signal (plist-get ,err_or_errsymbol :symbol) (or ,args (plist-get ,err_or_errsymbol :data))))
           (t
            (error "[yaxception:throw] Invalid argument : %s" ,err_or_errsymbol)))))

(defun yaxception:get-raw (err)
  "Expose raw error object from ERR that's given to `yaxception:catch'."
  (cons (or (plist-get err :symbol) (error "[yaxception:get-raw] Invalid argument : %s" err))
        (plist-get err :data)))

(defun yaxception:get-text (err)
  "Call `error-message-string' with ERR that's given to `yaxception:catch'."
  (error-message-string (yaxception:get-raw err)))

(defun yaxception:get-data (err)
  "Expose the data of raw error object from ERR that's given to `yaxception:catch'."
  (plist-get err :data))

(defun yaxception:get-prop (err name)
  "Call `plist-get' with the value of `yaxception:get-data'."
  (let* ((name (intern-soft (concat ":" (replace-regexp-in-string "^:" "" (symbol-name name))))))
    (plist-get (yaxception:get-data err) name)))

(cl-defun yaxception:get-stack-trace-string (err &key (filter nil) (limit nil))
  "Return a string like printStackTrace of Java.
FILTER is a function as predicate of `-filter' the stack traces.
  - It's given function name.
LIMIT is a number to `-take' the stack traces."
  (let* ((traces (yaxception-expose-stack-traces err))
         (traces (if filter (-filter (lambda (x) (funcall filter (plist-get x :name))) traces) traces))
         (traces (if limit (-take limit traces) traces))
         (formatter (lambda (c)
                      (concat "  at " (plist-get c :name) "(" (plist-get c :argstr) ")"))))
    (concat (yaxception:get-text err) "\n"
            (mapconcat formatter traces "\n"))))

(defvar yaxception-regexp-function-in-backtrace (rx-to-string `(and bos (+ space)
                                                                    (group (+ (not (any space "("))))
                                                                    "(" (group (* not-newline)) ")" (* space) eos)))
(defun yaxception-expose-stack-traces (err)
  (with-temp-buffer
    (insert (plist-get err :backtrace))
    (goto-char (point-min))
    (if (not (re-search-forward "^\\s-+yaxception-signal-hook-function(" nil t))
        (error "[yaxception] Failed to expose stack traces : Not found yaxception-signal-hook-function called")
      (forward-line 1)
      (beginning-of-line)
      (cl-loop for line = (replace-regexp-in-string "[\0\r\n]" "" (thing-at-point 'line))
               until (eobp)
               if (string-match yaxception-regexp-function-in-backtrace line)
               collect (let* ((funcnm (match-string-no-properties 1 line))
                              (argtext (match-string-no-properties 2 line)))
                         `(:name ,funcnm :argstr ,argtext))
               do (forward-line 1)))))


(provide 'yaxception)
;;; yaxception.el ends here
