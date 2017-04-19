;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(defun avr-tap ()
  (interactive)
  (message "thing-at-pint = %s" (thing-at-point 'symbol)))

(defun avr-ecs ()
  (interactive)
  (message "current symbol = %s ; fnsym = %s " (eldoc-current-symbol) (eldoc-fnsym-in-current-sexp)))

(defun avr-get-fnsym ()
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

(defun avr-dmp-fnsym ()
  (interactive)
  (message "fnsym = %s" (avr-get-fnsym)))


( somefn)
