;;; csh-mode.el
;;; 
;;; hacked out of forth.el from the tile-forth package
;;;
;;; comments/complaints/flames to strong+@cmu.edu
;;;

(defvar csh-mode-positives
  " foreach while else then switch default: case "
  "Contains all words which will cause the indent-level to be incremented
on the next line.
OBS! All words in csh-mode-positives must be surrounded by spaces.")

(defvar csh-mode-negatives
  " endif else breaksw endsw end "
  "Contains all words which will cause the indent-level to be decremented
on the current line.
OBS! All words in csh-mode-negatives must be surrounded by spaces.")

(defvar csh-mode-zeroes
  " #!/bin/csh "
  "Contains all words which causes the indent to go to zero")

(defvar csh-mode-abbrev-table nil
  "Abbrev table in use in csh-mode buffers.")

(define-abbrev-table 'csh-mode-abbrev-table ())

(defvar csh-mode-map nil
  "Keymap used in csh mode.")

(if (not csh-mode-map)
    (setq csh-mode-map (make-sparse-keymap)))

(define-key csh-mode-map "\t" 'csh-indent-command)
(define-key csh-mode-map "\C-m" 'reindent-then-newline-and-indent)

(defvar csh-mode-syntax-table nil
  "Syntax table in use in csh-mode buffers.")

(defvar csh-indent-level 2
  "*Indentation of csh statements.")

(defun csh-mode-variables ()
  (setq local-abbrev-table csh-mode-abbrev-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'csh-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t))
  
(defun csh-mode ()
  "Major mode for editing csh scripts.
\\[csh-indent-command] properly indents subexpressions of multi-line
if, while, foreach, and switch statements, taking nesting into account.
Caveats:
The file must start with '#!/bin/csh' for the indentation to start properly.
Extra spaces should be inserted to make sure the indentation algorithm can
figure out what is a keyword, string, etc.  For example, write
    if ($foo == \"bar\")
not
    if($foo==\"bar\")
or later lines may get indented wrong.  (Many lines like this are also
illegal csh code, so this shouldn't cramp your style.)

The variable csh-indent-level controls the amount of indentation.
\\{csh-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map csh-mode-map)
  (setq mode-name "csh")
  (setq major-mode 'csh-mode)
  (csh-mode-variables)
  (run-hooks 'csh-mode-hook))

(defun csh-current-indentation ()
  (save-excursion
    (beginning-of-line)
    (back-to-indentation)
    (current-column)))

(defun csh-delete-indentation ()
  (let
      ((b nil)
       (m nil))
    (save-excursion
      (beginning-of-line)
      (setq b (point))
      (back-to-indentation)
      (setq m (point)))
    (delete-region b m)))

(defun csh-indent-line (&optional flag)
  "Correct indentation of the current csh line."
  (let
      ((x (csh-calculate-indent)))
    (csh-indent-to x)))
  
(defun csh-indent-command ()
  (interactive)
  (csh-indent-line t))

(defun csh-indent-to (x)
  (let
      ((p nil))
    (setq p (- (current-column) (csh-current-indentation)))
    (csh-delete-indentation)
    (beginning-of-line)
    (indent-to x)
    (if (> p 0) (forward-char p))))

;;Calculate indent
(defun csh-calculate-indent ()
  (let ((w1 nil)
	(indent 0)
	(centre 0))
    (save-excursion
      (beginning-of-line)
      (skip-chars-backward " \t\n")
      (beginning-of-line)
      (back-to-indentation)
      (setq indent (current-column))
      (setq centre indent)
      (setq indent (+ indent (csh-sum-line-indentation))))
    (save-excursion
      (beginning-of-line)
      (back-to-indentation)
      (let ((p (point)))
	(skip-chars-forward "^ \t\n")
	(setq w1 (buffer-substring p (point)))))
    (if (> (- indent centre) csh-indent-level)
	(setq indent (+ centre csh-indent-level)))
    (if (> (- centre indent) csh-indent-level)
	(setq indent (- centre csh-indent-level)))
    (if (< indent 0) (setq indent 0))
    (setq indent (- indent
		    (if (string-match 
			 (regexp-quote (concat " " w1 " "))
			 csh-mode-negatives)
			csh-indent-level 0)))
    (if (string-match (regexp-quote (concat " " w1 " ")) csh-mode-zeroes)
	(setq indent 0))
    indent))

(defun csh-sum-line-indentation ()
  "Add up the positive and negative weights of all words on the current line."
  (let ((b (point))
	(e nil)
	(sum 0)
	(w nil)
	(t1 nil)
	(t2 nil)
	(first t))
    (end-of-line) (setq e (point))
    (goto-char b)
    (while (< (point) e)
      (setq w (csh-next-word))
      (setq t1 (string-match (regexp-quote (concat " " w " "))
			     csh-mode-positives))
      (setq t2 (string-match (regexp-quote (concat " " w " "))
			     csh-mode-negatives))
      (if (and t1 t2)
	  (setq sum (+ sum csh-indent-level)))
      (if t1
	  (setq sum (+ sum csh-indent-level)))
      (if (and t2 (not first))
	  (setq sum (- sum csh-indent-level)))
      (skip-chars-forward " \t")
      (setq first nil))
    sum))

(defun csh-next-word ()
  "Return the next csh-word. Skip anything enclosed in double quotes."
  (let ((w1 nil))
    (while (not w1)
      (skip-chars-forward " \t\n")
      (if (not (looking-at "\""))
	  (let ((p (point)))
	    (skip-chars-forward "^ \t\n")
	    (setq w1 (buffer-substring p (point))))
	(skip-chars-forward "^\"")
	(forward-char 1)))
    w1))
