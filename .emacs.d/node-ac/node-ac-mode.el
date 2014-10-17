;; node-ac.el --- Auto-complete for Node.js in Emacs

;; Copyright (C) 2013 Maokai Lin

;; Version: 0.1
;; Keywords: Node.js Emacs Autocomplete
;; Author: Maokai Lin <Maokai.Lin@gmail.com>
;; URL: https://github.com/MaokaiLin/node-ac

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary

;; Provide a Node.js auto-complete source for Emacs auto-complete mode.

;; The package includes the following parts:

;; 1. Finds all auto-complete candidates for the object, function, variable,
;;    etc. under cursor.

;; 2. Provide the documentation for objects, functions, and variables. Activate
;;    yasnippet when auto-complete candidate is selected.

;; 3. TODO: Jump to function/object/variable definition.

;;; Code:

(require 'js2-mode)
(require 'auto-complete)
(require 'yasnippet)

;;; Customization options

(defgroup node-ac nil
  "Auto-completion for Node.js."
  :group 'completion
  :prefix "node-ac-")

(defcustom node-ac-max-code-length-without-simplification 1024
  "Context generation will simplify block of codes with length above this threshold.

The threshold is the length of the code. If set to a large
number, node-ac will send all the context, such as full function
definition, full object definition, etc. in the buffer for
node.js to evaluate. This will result in better context-aware
completion, such as completion candidates for
`foo[2].bar('baz').`, but might take a long time when the program
complex or involves in large amount of loop and calculation.

Note: if the code is simplified, the value shown in the
documentation might be inaccurate. You should not rely on that
value without further tests.")

(defcustom node-ac-node-modules-path "/usr/local/lib/node_modules"
  "Path to the folder where node modules are installed."
  :group 'node-ac)

(defcustom node-ac-doc-min-width 20
  "The min-width of the document strings for each line")

(defcustom node-ac-completion-delay 0.25
  "The delay sections before sending completion request to node.js.")


;;; Private variables

(defvar node-ac-yas-activated nil
  "Indicate whether the node-ac is working with yas in the current buffer.")

(defvar node-ac-current-process nil
  "The node.js process currently running.")

(defvar node-ac-current-process-name "node-ac-process"
  "The name of the process.")

(defvar node-ac-output-buffer-name "node-ac-result-buffer"
  "The name of the buffer where the process writes results to.")

(defvar node-ac-evaluating-syntax nil
  "The syntax sent to node.js for completion.")

(defvar node-ac-evaluated-syntax nil
  "The evaluated syntax corresponding to the completion info stored in `node-ac-completion-table`.")

(defvar node-ac-completion-table nil
  "The complete information of auto-complete candidates.
The info is stored in terms of a list. Each entry is a list:
 (list candidate symbol description documentation yasnippet)")

(defvar node-ac-completion-popup-display-info nil
  "The complete information for popup display, derived from
  node-ac-completion-table.")

(defvar node-ac-idle-timer nil
  "The timer that triggers auto-complete when Emacs becomes idle.")

(defvar node-ac-source-dir (if load-file-name
							   (file-name-directory load-file-name)
							 default-directory)
  "The directory path to the source files of node-ac library.")

(defvar node-ac-js-source (expand-file-name "ac.js" node-ac-source-dir)
  "The auto-complete-get-candidates js file.")

(defvar node-ac-js-doc-source (expand-file-name "docgen.js" node-ac-source-dir)
  "The get-documentation js file.")

(defvar node-ac-mode-map (make-sparse-keymap))


;;; Functions for auto-complete

;; Some assistant functions
(defun node-ac-buffer-substring (start end)
  "Get buffer substring between START and END poitns with a boundary check."
  (let ((first-point (max 1 (min start end)))
		(last-point (min (max start end) (point-max))))
	;; check if start == end
	(if (> first-point last-point)
		(buffer-substring-no-properties first-point last-point)
		(buffer-substring-no-properties first-point (+ 1 last-point)))))

(defun node-ac-js2-node-syntax (node)
  "Get the syntax corresponding to the js2 node."
  (when (and node
			 (or (js2-prop-get-node-p node)
				 (js2-elem-get-node-p node)
				 (js2-call-node-p node)
				 (js2-name-node-p node)))
	;; Find the top-level get-property node
	(let (parent-node)
	  (while (and (setq parent-node (js2-node-parent node))
				  (or (js2-prop-get-node-p parent-node)
					  (js2-elem-get-node-p parent-node)))
		(setq node parent-node))
	  (node-ac-get-node-scripts node))))

(defun node-ac-reset ()
  "Reset the auto-complete.
Kill the ongoing Node.js evaluation process, and reset all existing data."
  ;; Cancel timer
  (when (timerp node-ac-idle-timer)
  	(cancel-timer node-ac-idle-timer))
  (setq node-ac-idle-timer nil)
  ;; Kill process
  (when node-ac-current-process
	(when (process-live-p node-ac-current-process)
	  (kill-process node-ac-current-process))
	(delete-process node-ac-current-process)
	(setq node-ac-current-process nil))
  ;; Reset cached data
  (setq node-ac-evaluated-syntax nil)
  (setq node-ac-completion-table nil))

(defun node-ac-syntax-under-cursor ()
  "Get the syntax under cursor, for auto-complete."
  (or (let* ((dot-complete? (looking-back "\\."))
			 (end-point (- (point)
						   (if dot-complete? 1 0)))
			 (line-code (node-ac-buffer-substring (line-beginning-position) end-point))
			 syntax)
		(with-temp-buffer
		  (insert line-code)
		  (js2-parse)
		  (setq syntax (node-ac-js2-node-syntax (js2-node-at-point (1- (point)))))
		  (when syntax
			(if dot-complete?
				(concat syntax ".")
			  syntax))))
	  ;; If not match, see if it is `require('foo<TAB>` type of completion request
	  (save-excursion
		(let ((orig-point (point)))
		  (when (re-search-backward "\\(?:[^_a-zA-Z0-9$]\\|^\\)\\(require\s*\([\'\"][^\'\"\)]*\\)\\=" (line-beginning-position) t)  ;; require("./foo<TAB>
			(node-ac-buffer-substring (match-beginning 1) orig-point))))))
		  
(defun node-ac-possible-syntax ()
  "Return the possible current syntax based on the last evaluated syntax."
  (when node-ac-evaluated-syntax
	(save-excursion
	  (let ((orig-point (point)))
		(when (re-search-backward (rx-to-string node-ac-evaluated-syntax) (line-beginning-position) t)
		  (node-ac-buffer-substring (match-beginning 0) orig-point))))))

(defun node-ac-evaluated-syntax-is-prefix-of-current-syntax ()
  "Check if the evaluated syntax could be a prefix of the current syntax.
This checking is to avoid multiple evaluations of syntax with the
same prefix."
  (let ((possible-syntax (node-ac-possible-syntax)))
	(when possible-syntax
	  (not (string-match
			"[^_a-zA-Z0-9]"
			(substring-no-properties possible-syntax
									 (length node-ac-evaluated-syntax)))))))

;;; Send auto-complete request to node.js for evaluation
(defun node-ac-send-request-to-nodejs ()
  "Send the auto-complete request to node.js for evaluation.
It spawns a new process async, and processes the results with
`node-ac-update' as call-back."
  (let ((syntax-for-eval (node-ac-syntax-under-cursor))
		context)
	(node-ac-reset)
	(setq node-ac-evaluating-syntax syntax-for-eval)
	;; Put together a context for node.js evaluation before invoking auto-complete
	(setq context (node-ac-generate-context (line-beginning-position)))
	;; Start the process async
	(setq node-ac-current-process
		  (start-process node-ac-current-process-name node-ac-output-buffer-name
						 "node" node-ac-js-source context (or syntax-for-eval "")))  ;; Execute this line in shell
	;; When kill the process, kill quitely
	(set-process-query-on-exit-flag node-ac-current-process nil)
	;; Set call-back function
	(set-process-sentinel node-ac-current-process 'node-ac-update-candidates)))

;;; Auto-complete call-back function
(defun node-ac-update-candidates (process event)
  "Update the auto-complete candidates when evaluation process completes or terminates abnormally."
  
  ;; Fetch results from output buffer
  (save-excursion
  	(switch-to-buffer node-ac-output-buffer-name)
	
	;; Check the result
	(when (and (string= event "finished\n")  ;; Finished
			   (not (string= "!!ERROR!!" (node-ac-buffer-substring 1 10)))  ;; No error
			   (or (null node-ac-evaluated-syntax)
				   (node-ac-evaluated-syntax-is-prefix-of-current-syntax)))  ;; Matches syntax under cursor
	  ;; Parse result
	  (setq node-ac-evaluated-syntax node-ac-evaluating-syntax)
	  (unless (string= "\n" (buffer-string))
		;; Completion found
		(setq node-ac-completion-table
			  (mapcar (lambda(elem) (split-string elem ">~<"))
					  (split-string (node-ac-buffer-substring 1 (1- (point))) "~~<")))
		(setq node-ac-completion-popup-display-info
			  (mapcar
			   (lambda (info)
				 (popup-make-item (car info)
								  :symbol (cadr info)
								  :summary (caddr info)
								  :document (node-ac-format-document (cadddr info))))
			   node-ac-completion-table))))
	
	;; (unless node-ac-completion-table
	;;   (message (buffer-string)))  ;; On error, output the buffer string
	(setq node-ac-evaluating-syntax nil)
  	(kill-buffer node-ac-output-buffer-name))
  
  ;; Invoke auto complete
  (if (and node-ac-completion-table
		   (current-idle-time))
	  (ac-complete-node)
  	(unless (or (string= event "killed: 9\n")
				(string= event "finished\n"))
  	  ;; Unsuccessful but not killed or empty. Retry
  	  (node-ac-send-request-to-nodejs))))

;;; Yasnippet insertion on function complete
(defun node-ac-expand-snippet (selected-candidate)
  "Insert and expand the yasnippet corresponding to the selected auto-complete candidate."
  (let ((start-point nil)
		(snippet (car (cddddr
					   (assoc selected-candidate
							  node-ac-completion-table)))))
	(unless (string= "" snippet)  ;; Only continue when a snippet is returned
	  ;; Get the point where the syntax of the candidate starts
	  (save-excursion
		(when (re-search-backward (concat selected-candidate "\\=") nil t)
		  (setq start-point (point))))
	  ;; When syntax match found, expand the snippet
	  (when start-point
		(yas-expand-snippet snippet start-point (point))))))

(defadvice ac-complete (around node-ac-expand-yasnippet-after-complete activate)
  "Start yasnippet when a function is selected."
  (if node-ac-yas-activated
	  (let ((selected-completion (ac-selected-candidate)))
		ad-do-it
		(node-ac-expand-snippet selected-completion))
	ad-do-it))

;;; Functions for getting documentations

(defun node-ac-syntax-for-doc ()
  "Get the syntax for getting documentation."
  (save-excursion
	;; Go back to the last dot position, or the last left parenthesis where no right parenthesis trails it
	(or (re-search-backward "\\.[^\s\.]\\=" (line-beginning-position) t)
		(re-search-backward "([^)]\\=" (line-beginning-position) t)
		(re-search-backward "\\[[^]]\\=" (line-beginning-position) t))
	(re-search-forward "\\=[\\.(\\[]?[_$a-zA-Z0-9]*" (line-end-position) t)
	(node-ac-syntax-under-cursor)))

(defun node-ac-get-document ()
  "Start the process of getting document for the syntax under cursor."
  (let ((syntax (node-ac-syntax-for-doc))
		context)
	(when syntax
	  (node-ac-reset)
	  (setq context (node-ac-generate-context (line-beginning-position)))
	  (setq node-ac-current-process
			(start-process node-ac-current-process-name
						   node-ac-output-buffer-name
						   "node" node-ac-js-doc-source context syntax))  ;; Execute this line
	  ;; When kill the process, kill quitely
	  (set-process-query-on-exit-flag node-ac-current-process nil)
	  ;; Set call-back function
	  (set-process-sentinel node-ac-current-process 'node-ac-display-doc))))

(defun node-ac-display-doc (process event)
  "Display the documentation when get-documentation process returns results."
  (let ((doc nil))
	;; Fetch the result from output buffer
	(save-excursion
	  (switch-to-buffer node-ac-output-buffer-name)
	  (when (and (string= event "finished\n")
				 (not (string-match "!!ERROR!!" (buffer-string))))
		(setq doc (node-ac-buffer-substring 1 (1- (point)))))
	  (kill-buffer node-ac-output-buffer-name))
	(when doc
	  (popup-tip (format "%s" doc)))))


;;; Functions and definitions for node-ac auto-complete sources

(defun node-ac-invoke-ac-when-idle ()
  "Invoke auto-complete to show candidates when Emacs is idle."
  (let ((idle-time (current-idle-time))
		delay-time)
	(when (timerp node-ac-idle-timer)
	  (cancel-timer node-ac-idle-timer)
	  (setq node-ac-idle-timer nil))
	(if idle-time
		(let ((micro-sec (caddr idle-time)))
		  (if (> micro-sec (* node-ac-completion-delay 900000))
			  (node-ac-send-request-to-nodejs)
			(setq delay-time (- node-ac-completion-delay (/ (float micro-sec) 1000000)))))
	  (setq delay-time node-ac-completion-delay))
	;; Set up idle timer to request for completion later
	(when delay-time
	  (setq node-ac-idle-timer (run-with-idle-timer delay-time nil 'node-ac-invoke-ac-when-idle)))))

(defun node-ac-candidates ()
  "Get the auto-complete candidates.

Each candidate consists of:
  - Name of the candidate syntax
  - A symbol (f/o/n/u for function/object/number/undefined)
  - A summary (type of the syntax, function, object, etc.)
  - Documentation (one-level content for objects, value for
    numbers and strings, definition for functions."
  (if (node-ac-evaluated-syntax-is-prefix-of-current-syntax)
	  node-ac-completion-popup-display-info
	(unless node-ac-idle-timer
	  (setq node-ac-idle-timer (run-with-idle-timer 0 nil 'node-ac-invoke-ac-when-idle)))
	nil))

(defun node-ac-format-document (doc)
  "Format the document."
  (unless (string= doc "")
	(let ((doc-lines (split-string doc "\n")))
	  (mapconcat (lambda (line)
				   (let ((space-filler-width (- node-ac-doc-min-width (length line))))
					 (concat " " line (make-string (max space-filler-width 2) 32))))
				 doc-lines "\n"))))

(defun node-ac-prefix ()
  "Get the prefix point of syntax under cursor for auto-complete.

It also controls when to show auto-complete based on the set
`ac-auto-start` value.

The prefix point of the syntax under cursor is either
  - the starting point of the whole syntax if there is no dot in
     it, or
  - the point behind the last dot.

For  require('  case, the prefix point is at 'r'."
  (if node-ac-evaluated-syntax
	  ;; When there's an evaluated syntax, try to match the prefix
	  (let ((possible-syntax (node-ac-possible-syntax))
			last-property-start-point)
		(if (and possible-syntax
				 (or (null ac-auto-start)
					 (>= (length possible-syntax) ac-auto-start)))
			;; Prefix matches, should start an auto-complete
			(if (setq last-property-start-point (string-match "\\.[^\.]*$" possible-syntax))
				(+ (- (point) (length possible-syntax)) last-property-start-point 1)
			  (- (point) (length possible-syntax)))
		  ;; Else, reset and trigger a new evaluation
		  (node-ac-reset)
		  (point)))
	;; If no syntax evaluated, return current point just to trigger a new evaluation
	(point)))

(ac-define-source "node"
  '((candidates . node-ac-candidates)
    (prefix . node-ac-prefix)
    (requires . -1)))


;; Functions for jumping to definition

(defun node-ac-find-param-with-name (name params-nodes)
  "Find the parameter node with `name` in `params-nodes`."
  (let ((param-node (car params-nodes)))
	(car (loop for param-node in params-nodes
			   when (and (js2-name-node-p param-node)
						 (string= (js2-name-node-name param-node) name))
			   collect param-node))))

(defun node-ac-find-function-with-name (name node)
  "Find the function node with `name` in `node`."
  (let* ((body-node (if (js2-function-node-p node)
						(js2-function-node-body node)
					  node))
		 (clause-nodes (when (js2-block-node-p body-node)
						 (js2-block-node-kids body-node))))
	(car (loop for clause-node in clause-nodes
			   when (and (js2-function-node-p clause-node)
						 (string= (js2-name-node-name (js2-function-node-name clause-node)) name))
			   collect (js2-function-node-name clause-node)))))

(defun node-ac-find-symbol-point (name node)
  "Find the definition point of the `name` within `node`. If not found return nil."
  (let* ((symbol-table (js2-scope-symbol-table node))
		 (def-node (assoc (intern name) symbol-table))
		 def-symbol-node def-name-node)
	(when def-node
	  ;; Definition found
	  (if (setq def-symbol-node (js2-symbol-ast-node (cdr def-node)))
		  ;; It's a symbol
		  (when (setq def-name-node (js2-node-parent def-symbol-node))
			(if (js2-function-node-p def-name-node)
				;; It's a parameter, search through the parameter list
				(when (setq def-name-node
							(node-ac-find-param-with-name name (js2-function-node-params def-name-node)))
				  (js2-node-abs-pos def-name-node))
			  ;; Not a parameter, get the point directly
			  (js2-node-abs-pos def-name-node)))
		;; It's a function, search the clauses and find the function node
		(when (setq def-symbol-node (node-ac-find-function-with-name name node))
		  (js2-node-abs-pos def-symbol-node))))))
			  
(defun node-ac-get-definition-point ()
  "Find the definition node of the given name."
  (let ((node (js2-node-at-point))
		name jump-point)
	(when (js2-name-node-p node)
	  (setq name (js2-name-node-name node))
	  (while (and node
				  (not jump-point))
		;; Go upwards untill reaching a block node or the top level
		(while (not (js2-scope-p node))
		  (setq node (js2-node-parent node)))
		;; Get the jump point
		(setq jump-point (node-ac-find-symbol-point name node))
		;; Go on to the parent node
		(setq node (js2-node-parent node))))
	jump-point))


;;; Auto-complete, show doc, and jump to definition

;;;###autoload
(defun node-ac-auto-complete ()
  "Auto complete the syntax under cursor."
  (interactive)
  (node-ac-reset)
  (node-ac-send-request-to-nodejs))

;;;###autoload
(defun node-ac-dot-complete ()
  "Omni-complete after typing dot.
Note: it sends the completion request before inserting a dot in
order not to mess up the syntax analysis with the newly inserted
dot."
  (interactive)
  (node-ac-reset)
  (insert ".")
  (node-ac-send-request-to-nodejs))

;;;###autoload
(defun node-ac-show-document ()
  "Show the documentation of the syntax under cursor."
  (interactive)
  (node-ac-get-document))

;;;###autoload
(defun node-ac-jump-to-definition ()
  "Jump to the definition of the symbol under cursor."
  (interactive)
  (let ((jump-point (node-ac-get-definition-point)))
	(when (and jump-point
			   (not (= jump-point (point))))
	  (push-mark)
	  (goto-char jump-point))))


;;; node-ac minor mode
;;;###autoload
(defun node-ac-setup ()
  "Setup node path, ac-sources, and basic key strokes."
  (unless (fboundp 'node-ac-generate-context)
	(load-file (expand-file-name "node-ac-js-context.el" node-ac-source-dir)))
  (setenv "NODE_PATH" node-ac-node-modules-path)
  (add-to-list 'ac-sources 'ac-source-node)
  (set (make-local-variable 'node-ac-yas-activated) t)
  (setq ac-expand-on-auto-complete nil)
  (define-key node-ac-mode-map "." 'node-ac-dot-complete))

;;;###autoload
(define-minor-mode node-ac-mode
  "node-ac mode."
  :keymap node-ac-mode-map
  :group 'node-ac
  :name "node-ac"
  (node-ac-setup))

;;;###autoload
(add-hook 'js2-mode-hook 'node-ac-mode)

(provide 'node-ac-mode)

;;; node-ac.el ends here
