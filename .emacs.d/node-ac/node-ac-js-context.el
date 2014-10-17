;; node-ac-js-context.el --- Generate relevant context for Node.js

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

;; Functions that generate a relevant and simplified context.

;; Note: this file should be loaded from node-ac-model.el, not to be used as a
;; stand-along module.

;;   Main Function:
;;     - node-ac-generate-context(&optional point)
;;       Generate a relevant context at the given point (defaults to the current
;;       point) as an input for ac.js

;;   Key parts:
;;     * node-ac-get-context-script-list (point)
;;       Generates a list of relevant context scripts, sorted in their original
;;       positions.

;;     * node-ac-symbol-def-scripts (point symbol-table)
;;       Generates simplified variable assignment scripts before the point from
;;       the symbol table.

;;     * node-ac-find-and-simplify-value-changing-clauses (point clause-list)
;;       Generates simplified value-changing scripts before the point from the
;;       clause list.

;;     * node-ac-find-and-simplify-functions (clause-list)
;;       Generates simplified function definition scripts from the clause list.

;;; Code:

(defun node-ac-generate-context (&optional point)
  "Generate the relevant context for auto-complete."
  (mapconcat
   (lambda(clause)
	 (node-ac-clause-wrap (car clause)))
   (node-ac-get-context-script-list (point))
   "\n"))

(defun node-ac-get-context-script-list (point)
  "Get a list of relevant context scripts.

We consider scripts are relevant for auto-complete if one of the
following is true:

  1. It's a variable declaration before the given point and it's
     visible to the scope of the point.
  2. It affects the value or type of a variable before the point.
  3. It's a function definition in terms of
       function a() {}
     not
       var a = function() {}
     in a scope visible at the given point.

In the implementation, we separate the search for variable
definition (1) and function defition (3) because js2-mode does
not associate function definitions with their declaration nodes.
"
  (unless point (setq point (point)))
  (let ((node (js2-node-at-point point))
		(clauses (list)))
	(while node
	  ;; Go upwards untill reaching a block node or the top level
	  (while (not (js2-scope-p node))
		(setq node (js2-node-parent node)))
	  
	  ;; Join a list of symbol def scripts and a list of function def scripts
	  (let ((clause-list (node-ac-clauses-list node))
			(symbol-table (js2-scope-symbol-table node)))
		;; Join and sort the list of scripts
		(setq clauses
			  (append (node-ac-symbol-def-scripts point symbol-table)
					  (node-ac-find-and-simplify-value-changing-clauses point clause-list)
					  (node-ac-find-and-simplify-functions clause-list)
					  clauses)))

	  ;; Go on to the parent node
	  (setq node (js2-node-parent node)))

	;; Return the clauses in order
	(sort clauses
		  (lambda (x y)
			(< (cdr x) (cdr y))))))

(defun node-ac-symbol-def-scripts (point symbol-table)
  "Find all non-functional definitions before the point in the symbol table.
Return a list of associations (simplified-scripts . abs-position)."
  (loop with script = nil
		for symbol in symbol-table
		when (setq script (node-ac-get-symbol-def-script point symbol))
		collect script))

(defun node-ac-find-and-simplify-value-changing-clauses (point clause-list)
  "Find and simplify each clause that changes a variable's value.
Returns a list of associations (simplified-scripts . abs-position)."
  (let ((script-list (list))
		script abs-pos)
	(while (and clause-list
				(< (setq abs-pos
						 (js2-node-abs-pos (car clause-list)))
				   point))
	  (setq script (node-ac-simplify-value-changing-clause (car clause-list)))
	  (when script
		(setq script-list
			  (append script-list
					  (list (cons script abs-pos)))))
	  (setq clause-list (cdr clause-list)))
	script-list))

(defun node-ac-find-and-simplify-functions (clause-list)
  "Find all function definitions in the clause list.
Returns a list of associations (simplified-function-definition-scripts . abs-position)."
  (loop for clause-node in clause-list
		when (js2-function-node-p clause-node)
		collect (cons (node-ac-simplify-function clause-node)
					  (js2-node-abs-pos clause-node))))


;; Symbol table processing 

(defun node-ac-get-symbol-def-script (point symbol-assoc)
  "Get simplified scripts of the symbol's definition.
Returns an assoc (simplified-script . abs-position)."
  (let* ((symbol (cdr symbol-assoc))
		 ;; Type: one of js2-FUNCTION, js2-LP (for parameters), js2-VAR,
		 ;; js2-LET, and js2-CONST
		 (type (js2-symbol-decl-type symbol)))
	(unless (= type js2-FUNCTION)
	  (let* ((name-node (js2-symbol-ast-node symbol)))
		(if (js2-var-init-node-p (js2-node-parent name-node))
			
		  ;; Var definition
			(let* ((var-init-node (js2-node-parent name-node))
				   (pos (js2-node-abs-pos var-init-node)))
			  ;; Skip function defs and symbols defined after the given point
			  (when (< pos point)
				;; Treat const definition as a var definition, they are same when only
				;; auto-complete is concerned
				(cons (concat "var "
							  (node-ac-get-node-scripts (js2-var-init-node-target var-init-node))
							  (when (js2-var-init-node-initializer var-init-node)
								(concat " = "
										(node-ac-simplify-node (js2-var-init-node-initializer var-init-node))))
							  ";")
					  pos)))
		  
		  ;; No parent node, probably function parameter
		  (cons (concat "var " (js2-name-node-name name-node) ";")
				(js2-node-abs-pos name-node)))))))

;; Value-changing scripts processiong

(defun node-ac-simplify-value-changing-clause (clause-node)
  "Get simplified scripts if the node represents a clause that changes a variable's value.
Returns nil if it is not a value-changing clause."
  (when (js2-expr-stmt-node-p clause-node)
	(let ((assign-node (js2-expr-stmt-node-expr clause-node)))
	  (when (js2-assign-node-p assign-node)
		(let ((var (js2-assign-node-left assign-node))
			  (value (js2-assign-node-right assign-node)))
		  (concat (node-ac-get-node-scripts var)
				  " = "
				  (node-ac-simplify-node value)
				  ";"))))))

;;; Function simplification

(defun node-ac-simplify-function (fn-node)
  "Simplify scripts of the function corresponding to the function node.
Caveat: the function does not check if the fn-node is actually a function node."
  (if (node-ac-should-simplify-code? fn-node)
	  (let ((scripts "")  ;; Simplified scripts to return
			(getter (js2-node-get-prop fn-node 'GETTER_SETTER))
			(name-node (js2-function-node-name fn-node))
			(params-nodes (js2-function-node-params fn-node))
			(rest-p (js2-function-node-rest-p fn-node)))
		;; First put together the function head
		(unless getter
		  (setq scripts (concat scripts "function")))
		(when name-node
		  (setq scripts (concat scripts " " (node-ac-simplify-node name-node))))
		
		;; Add function parameters
		(setq scripts (concat scripts "("))
		(loop with len = (length params-nodes)
			  for param-node in params-nodes
			  for count from 1
			  do
			  (when (and rest-p
						 (= count len))
				(setq scripts (concat scripts "...")))
			  (setq scripts (concat scripts (node-ac-simplify-node param-node)))
			  (when (< count len)
				(setq scripts (concat scripts ", "))))
		
		;; Simplify scripts in the body block
		(concat scripts ") {"
				;; Join the list
				(mapconcat (lambda (x) x)
						   (node-ac-simplify-function-body fn-node)
						   "")
				"}"))
	(node-ac-get-node-scripts fn-node)))

(defun node-ac-simplify-function-body (fn-node)
  "Simplify the scripts within a function's body.
'Simplify' here means to only keep relevant codes. In the context
of a function definition, only assignment codes containing 'this'
on the left-hand-side is relevant."
  (let ((clause-list (node-ac-clauses-list fn-node))
		relevant-script)
	(loop for clause-node in clause-list
		  when (setq relevant-script (node-ac-relevant-in-function-script clause-node))
		  collect relevant-script)))

(defun node-ac-relevant-in-function-script (clause-node)
  "Returns relevant script of the clause node. If no relevant script, return nil.
Note: only assignment codes containing 'this' on the left-hand-side is relevant."
  (let (stmt-node
		left-node)
	(when (and
		   (js2-expr-stmt-node-p clause-node)
		   (js2-assign-node-p
			(setq stmt-node (js2-expr-stmt-node-expr clause-node)))
		   (js2-prop-get-node-p
			(setq left-node (js2-assign-node-left stmt-node)))
		   (js2-this-node-p
			(js2-prop-get-node-left left-node)))
	  (concat (node-ac-get-node-scripts left-node)
			  " = "
			  (node-ac-simplify-node (js2-assign-node-right stmt-node))
			  ";"))))


;;; General function

(defun node-ac-simplify-node (node)
  "Get simplified scripts of the node."
  (cond
   ;; No need to simplify code
   ((not (node-ac-should-simplify-code? node))
	(node-ac-get-node-scripts node))
   
   ;; Simplify code based on node type
   ((js2-function-node-p node)
	(node-ac-simplify-function node))
   ((js2-object-prop-node-p node)
	(concat (node-ac-simplify-node (js2-object-prop-node-left node))
			":"
			(node-ac-simplify-node (js2-object-prop-node-right node))))
   ((js2-object-node-p node)
	(concat "{"
			(mapconcat 'node-ac-simplify-node
					   (js2-object-node-elems node)
					   ", ")
			"}"))
   ((js2-array-node-p node)
	(concat "["
			(mapconcat 'node-ac-simplify-node
					   (js2-array-node-elems node)
					   ", ")
			"]"))
   (t (node-ac-get-node-scripts node))))

(defun node-ac-should-simplify-code? (node)
  "Determine whether to simplify the code in the node."
  (> (js2-node-len node) node-ac-max-code-length-without-simplification))

;;; Assistant functions

(defun node-ac-clauses-list (node)
  "Return a clauses list of the given node."
  (cond ((js2-function-node-p node)
		 (js2-block-node-kids (js2-function-node-body node)))
		((js2-block-node-p node)
		 (js2-block-node-kids node))))

(defun node-ac-get-node-scripts (node)
  "Get the scripts of the given node."
  (condition-case nil
	  (with-temp-buffer
		(js2-print-tree node)
		(buffer-string))
	(error "")))

(defun node-ac-clause-wrap (script)
  "Wrap a line of script with try {} catch(e) {} block."
  (concat "~~>" script))

