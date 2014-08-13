;;;; Intial attempt at making a SKILL mode
;;;; derived from lisp-mode with specific
;;;; overrides
;;;;
;;;; A. Vincent Rayappa
;;;;
;;;; Initial-revision Nov 04, 2007
;;;;
;;;; $Id: skill-mode.el,v 1.2 2009/07/04 23:57:12 vrayapp Exp $
;;;;

;;; Load functions that define docstring for use
;;; by eldoc callback
(load "skill-fn-info.el")

;;; Load function needed to do html document lookup
(load "skill-html-doc-lookup.el")

;;; Load feature to show pretty math symbols in skill mode
(load "skill-pretty-symbols.el")

;;; Figure out what the function name is
(defun skill-get-fnsym ()
  (let ((p (point))
        (ret nil))
    ;; Don't do anything if current word is inside a string.
    (if (= (or (char-after (1- (point))) 0) ?\")
        nil
        (progn
          (backward-up-list)
          (forward-word)
          (setq ret (thing-at-point 'symbol))))
    (goto-char p)
    ret))


;;; Function that looks up and return the docstring
;;; for the function that is being entered in the
;;; buffer
(defun skill-eldoc-function ()
  "Returns a documentation string appropriate for the current context or nil."
  (condition-case err
      (let* ((current-fnsym  (skill-get-fnsym))
             (doc (skill-fn-info-get current-fnsym)))       
        doc)
    ;; This is run from post-command-hook or some idle timer thing,
    ;; so we need to be careful that errors aren't ignored.
    (error (message "eldoc error: %s" err))))

(defvar skill-mode-hook nil
  "List of functions to call when skill-mode starts")

;;; Main code for SKILL mode
(define-derived-mode skill-mode
    lisp-mode "skill"
    "Major mode for SKILL mode, based on lisp-mode."
    ;; set eldoc mode
    (set (make-local-variable 'eldoc-documentation-function) 
         'skill-eldoc-function)
    t)

;; associate .il and .ils files with skill mode
;; ("\\.il$" . skill-mode)
(add-to-list 'auto-mode-alist '("\\.il$" . skill-mode))
(add-to-list 'auto-mode-alist '("\\.ils$" . skill-mode))

(provide 'skill-mode)

