;;; setup-functions.el ---                   -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2022  Abelardo Jara-Berrocal

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

;; Missing function
(when (not (fboundp 'custom-add-choice))
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
             (append choices (list choice)))))))

(defvar after-find-file-from-revert-buffer nil)

(defun -partial (fn &rest args)
  "Takes a function FN and fewer than the normal arguments to FN,
and returns a fn that takes a variable number of additional ARGS.
When called, the returned function calls FN with ARGS first and
then additional args."
  (apply 'apply-partially fn args))

(defun -rpartial (fn &rest args)
  "Takes a function FN and fewer than the normal arguments to FN,
and returns a fn that takes a variable number of additional ARGS.
When called, the returned function calls FN with the additional
args first and then ARGS."
  (lambda (&rest args-before) (apply fn (append args-before args))))

(defun -juxt (&rest fns)
  "Takes a list of functions and returns a fn that is the
juxtaposition of those fns. The returned fn takes a variable
number of args, and returns a list containing the result of
applying each fn to the args (left-to-right)."
  (lambda (&rest args) (mapcar (lambda (x) (apply x args)) fns)))

(defun -compose (&rest fns)
  "Takes a list of functions and returns a fn that is the
composition of those fns. The returned fn takes a variable
number of arguments, and returns the result of applying
each fn to the result of applying the previous fn to
the arguments (right-to-left)."
  (lambda (&rest args)
    (car (-reduce-r-from (lambda (fn xs) (list (apply fn xs)))
                         args fns))))

(defun -applify (fn)
  "Changes an n-arity function FN to a 1-arity function that
expects a list with n items as arguments"
  (apply-partially 'apply fn))

(defun -on (operator transformer)
  "Return a function of two arguments that first applies
TRANSFORMER to each of them and then applies OPERATOR on the
results (in the same order).

In types: (b -> b -> c) -> (a -> b) -> a -> a -> c"
  (lambda (x y) (funcall operator (funcall transformer x) (funcall transformer y))))

(defun -flip (func)
  "Swap the order of arguments for binary function FUNC.

In types: (a -> b -> c) -> b -> a -> c"
  (lambda (x y) (funcall func y x)))

(defun -const (c)
  "Return a function that returns C ignoring any additional arguments.

In types: a -> b -> a"
  (lambda (&rest _) c))

(defmacro -cut (&rest params)
  "Take n-ary function and n arguments and specialize some of them.
Arguments denoted by <> will be left unspecialized.

See SRFI-26 for detailed description."
  (let* ((i 0)
         (args (mapcar (lambda (_) (setq i (1+ i)) (make-symbol (format "D%d" i)))
                       (-filter (-partial 'eq '<>) params))))
    `(lambda ,args
       ,(--map (if (eq it '<>) (pop args) it) params))))

(defun -not (pred)
  "Take an unary predicates PRED and return an unary predicate
that returns t if PRED returns nil and nil if PRED returns
non-nil."
  (lambda (x) (not (funcall pred x))))

(defun -orfn (&rest preds)
  "Take list of unary predicates PREDS and return an unary
predicate with argument x that returns non-nil if at least one of
the PREDS returns non-nil on x.

In types: [a -> Bool] -> a -> Bool"
  (lambda (x) (-any? (-cut funcall <> x) preds)))

(defun -andfn (&rest preds)
  "Take list of unary predicates PREDS and return an unary
predicate with argument x that returns non-nil if all of the
PREDS returns non-nil on x.

In types: [a -> Bool] -> a -> Bool"
  (lambda (x) (-all? (-cut funcall <> x) preds)))

(defun -iteratefn (fn n)
  "Return a function FN composed N times with itself.

FN is a unary function.  If you need to use a function of higher
arity, use `-applify' first to turn it into an unary function.

With n = 0, this acts as identity function.

In types: (a -> a) -> Int -> a -> a.

This function satisfies the following law:

  (funcall (-iteratefn fn n) init) = (-last-item (-iterate fn init (1+ n)))."
  (lambda (x) (--dotimes n (setq x (funcall fn x))) x))

(defun -fixfn (fn)
  "Return a function that computes the (least) fixpoint of FN.

FN is a unary function, results are compared with `equal'.

In types: (a -> a) -> a -> a."
  (lambda (x)
    (let ((re (funcall fn x)))
      (while (not (equal x re))
        (setq x re)
        (setq re (funcall fn re)))
      re)))

(defun -prodfn (&rest fns)
  "Take a list of n functions and return a function that takes a
list of length n, applying i-th function to i-th element of the
input list.  Returns a list of length n.

In types (for n=2): ((a -> b), (c -> d)) -> (a, c) -> (b, d)

This function satisfies the following laws:

  (-compose (-prodfn f g ...) (-prodfn f' g' ...)) = (-prodfn (-compose f f') (-compose g g') ...)
  (-prodfn f g ...) = (-juxt (-compose f (-partial 'nth 0)) (-compose g (-partial 'nth 1)) ...)
  (-compose (-prodfn f g ...) (-juxt f' g' ...)) = (-juxt (-compose f f') (-compose g g') ...)
  (-compose (-partial 'nth n) (-prod f1 f2 ...)) = (-compose fn (-partial 'nth n))"
  (lambda (x) (-zip-with 'funcall fns x)))

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
