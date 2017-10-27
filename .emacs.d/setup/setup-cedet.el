;;; setup-cedet.el ---

;; Copyright (C) 2014, 2015, 2016, 2017  Abelardo Jara-Berrocal

;; Author: Abelardo Jara-Berrocal <abelardojara@Abelardos-MacBook-Pro.local>
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

(use-package semantic
  :config (progn

            ;; To use additional features for names completion, and displaying of information for tags & classes,
            ;; you also need to load the semantic-ia package. Unfortunately, semantic makes Emacs slow
            (use-package semantic/ia)

            ;; Enable support for parsing additional languages
            (use-package semantic/wisent)

            ;; Enabled features
            (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                              global-semanticdb-minor-mode
                                              global-semantic-idle-summary-mode
                                              global-semantic-mru-bookmark-mode
                                              global-semantic-load-enable-code-helpers
                                              global-semantic-load-enable-excessive-code-helpers
                                              global-semantic-idle-completions-mode))

            ;; Assure .emacs.cache/semanticdb directory exists
            (if (not (file-exists-p "~/.emacs.cache/semanticdb"))
                (make-directory "~/.emacs.cache/semanticdb") t)

            ;; Enable case-insensitive searching
            (set-default 'semantic-case-fold t)

            ;; Faster parsing
            (setq semantic-idle-work-parse-neighboring-files-flag nil
                  semantic-idle-work-update-headers-flag      nil
                  semantic-idle-scheduler-idle-time               432000
                  semantic-idle-scheduler-work-idle-time      1800 ;; default is 60
                  semantic-idle-scheduler-max-buffer-size     1)

            ;; Disable Semantics for large files
            (add-hook 'semantic--before-fetch-tags-hook
                      (lambda () (if (and (> (point-max) 500)
                                     (not (semantic-parse-tree-needs-rebuild-p)))
                                nil
                              t)))

            ;; Enable decoration mode
            (global-semantic-decoration-mode t)

            ;; for semantic-ia-fast-jump
            (use-package semantic/analyze/refs)

            ;; Fixing a bug in semantic, see #22287
            (defun semanticdb-save-all-db-idle ()
              "Save all semantic tag databases from idle time.
Exit the save between databases if there is user input."

              ;; save-mark-and-excursion is defined in Emacs 25.1-forward
              (if (fboundp 'save-mark-and-excursion)
                  (semantic-safe "Auto-DB Save: %S"
                    ;; FIXME: Use `while-no-input'?
                    (save-mark-and-excursion ;; <-- added line
                     (semantic-exit-on-input 'semanticdb-idle-save
                       (mapc (lambda (db)
                               (semantic-throw-on-input 'semanticdb-idle-save)
                               (semanticdb-save-db db t))
                             semanticdb-database-list))))
                (if (fboundp 'save-excursion)
                    (save-excursion ;; <-- added line
                      (semantic-exit-on-input 'semanticdb-idle-save
                        (mapc (lambda (db)
                                (semantic-throw-on-input 'semanticdb-idle-save)
                                (semanticdb-save-db db t))
                              semanticdb-database-list))))))

            ;; Disable semanticdb, slows down Emacs
            (global-semanticdb-minor-mode nil)
            (setq semanticdb-search-system-databases t)
            (add-hook 'c-mode-common-hook
                      (lambda ()
                        (setq semanticdb-project-system-databases
                              (list (semanticdb-create-database
                                     semanticdb-new-database-class
                                     "/usr/include")))))

            ;; Set project roots to start from /
            (setq semanticdb-project-roots
                  (list
                   (expand-file-name "/")))

            ;; This prevents Emacs to become uresponsive
            (defun semanticdb-kill-hook ()
              nil)
            (defun semanticdb-create-table-for-file-not-in-buffer (arg)
              nil)

	    (define-overloadable-function semantic-analyze-possible-completions (context &rest flags)
	      "Return a list of semantic tags which are possible completions.
CONTEXT is either a position (such as point), or a precalculated
context.  Passing in a context is useful if the caller also needs
to access parts of the analysis.
The remaining FLAGS arguments are passed to the mode specific completion engine.
Bad flags should be ignored by modes that don't use them.
See `semantic-analyze-possible-completions-default' for details on the default FLAGS.

Completions run through the following filters:
  * Elements currently in scope
  * Constants currently in scope
  * Elements match the :prefix in the CONTEXT.
  * Type of the completion matches the type of the context.
Context type matching can identify the following:
  * No specific type
  * Assignment into a variable of some type.
  * Argument to a function with type constraints.
When called interactively, displays the list of possible completions
in a buffer."
	      (interactive "d")
	      ;; In theory, we don't need the below since the context will
	      ;; do it for us.
	      ;;(semantic-refresh-tags-safe)
	      (with-syntax-table semantic-lex-syntax-table
		(let* ((context (if (semantic-analyze-context-child-p context)
				    context
				  (semantic-analyze-current-context context)))
		       (ans (if (not context)
				nil ;; (error "Nothing to complete")
			      (:override))))
		  ;; If interactive, display them.
		  (when (cedet-called-interactively-p 'any)
		    (with-output-to-temp-buffer "*Possible Completions*"
		      (semantic-analyze-princ-sequence ans "" (current-buffer)))
		    (shrink-window-if-larger-than-buffer
		     (get-buffer-window "*Possible Completions*")))
		  ans)))

	    ;; Ignore errors on semantic-analyze-possible-completions
	    (defun semantic-analyze-possible-completions-default (context &optional flags)
	      "Default method for producing smart completions.
Argument CONTEXT is an object specifying the locally derived context.
The optional argument FLAGS changes which return options are returned.
FLAGS can be any number of:
  'no-tc     - do not apply data-type constraint.
  'no-unique - do not apply unique by name filtering."
	      (ignore-errors
		(let* ((a context)
		       (desired-type (semantic-analyze-type-constraint a))
		       (desired-class (oref a prefixclass))
		       (prefix (oref a prefix))
		       (prefixtypes (oref a prefixtypes))
		       (completetext nil)
		       (completetexttype nil)
		       (scope (oref a scope))
		       (localvar (when scope (oref scope localvar)))
		       (origc nil)
		       (c nil)
		       (any nil)
		       (do-typeconstraint (not (memq 'no-tc flags)))
		       (do-unique (not (memq 'no-unique flags))))

		  ;; Calculate what our prefix string is so that we can
		  ;; find all our matching text.
		  (setq completetext (car (reverse prefix)))
		  (if (semantic-tag-p completetext)
		      (setq completetext (semantic-tag-name completetext)))

		  ;; (if (and (not completetext) (not desired-type))
		  ;;     (error "Nothing to complete"))
		  (unless (and (not completetext) (not desired-type))
		    (if (not completetext) (setq completetext ""))

		    ;; This better be a reasonable type, or we should fry it.
		    ;; The prefixtypes should always be at least 1 less than
		    ;; the prefix since the type is never looked up for the last
		    ;; item when calculating a sequence.
		    (setq completetexttype (car (reverse prefixtypes)))
		    (when (or (not completetexttype)
			      (not (and (semantic-tag-p completetexttype)
					(eq (semantic-tag-class completetexttype) 'type))))
		      ;; What should I do here?  I think this is an error condition.
		      (setq completetexttype nil)
		      ;; If we had something that was a completetexttype but it wasn't
		      ;; valid, then express our dismay!
		      (when (> (length prefix) 1)
			(let* ((errprefix (car (cdr (reverse prefix)))))
			  (error "Cannot find types for `%s'"
				 (cond ((semantic-tag-p errprefix)
					(semantic-format-tag-prototype errprefix))
				       (t (format "%S" errprefix)))))))

		    ;; There are many places to get our completion stream for.
		    ;; Here we go.
		    (if completetexttype
			(setq c (semantic-find-tags-for-completion
				 completetext
				 (semantic-analyze-scoped-type-parts completetexttype scope)))

		      ;; No type based on the completetext.  This is a free-range
		      ;; var or function.  We need to expand our search beyond this
		      ;; scope into semanticdb, etc.
		      (setq c (nconc
			       ;; Argument list and local variables
			       (semantic-find-tags-for-completion completetext localvar)
			       ;; The current scope
			       (semantic-find-tags-for-completion completetext (when scope (oref scope fullscope)))
			       ;; The world
			       (semantic-analyze-find-tags-by-prefix completetext))))

		    (let ((loopc c)
			  (dtname (semantic-tag-name desired-type)))

		      ;; Save off our first batch of completions
		      (setq origc c)

		      ;; Reset c.
		      (setq c nil)

		      ;; Loop over all the found matches, and catagorize them
		      ;; as being possible features.
		      (while (and loopc do-typeconstraint)

			(cond
			 ;; Strip operators
			 ((semantic-tag-get-attribute (car loopc) :operator-flag)
			  nil)

			 ;; If we are completing from within some prefix,
			 ;; then we want to exclude constructors and destructors
			 ((and completetexttype
			       (or (semantic-tag-get-attribute (car loopc) :constructor-flag)
				   (semantic-tag-get-attribute (car loopc) :destructor-flag)))
			  nil)

			 ;; If there is a desired type, we need a pair of restrictions
			 (desired-type

			  (cond
			   ;; Ok, we now have a completion list based on the text we found
			   ;; we want to complete on.  Now filter that stream against the
			   ;; type we want to search for.
			   ((string= dtname (semantic-analyze-type-to-name (semantic-tag-type (car loopc))))
			    (setq c (cons (car loopc) c)))

			   ;; Now anything that is a compound type which could contain
			   ;; additional things which are of the desired type
			   ((semantic-tag-type (car loopc))
			    (let ((att (semantic-analyze-tag-type (car loopc) scope)))
			      (if (and att (semantic-tag-type-members att))
				  (setq c (cons (car loopc) c))))))) ;; desired type

			 ;; No desired type, no other restrictions.  Just add.
			 (t (setq c (cons (car loopc) c)))); cond

			(setq loopc (cdr loopc)))

		      (when desired-type
			;; Some types, like the enum in C, have special constant values that
			;; we could complete with.  Thus, if the target is an enum, we can
			;; find possible symbol values to fill in that value.
			(let ((constants
			       (semantic-analyze-type-constants desired-type)))
			  (if constants
			      (progn
				;; Filter
				(setq constants
				      (semantic-find-tags-for-completion
				       completetext constants))
				;; Add to the list
				(setq c (nconc c constants)))))))

		    (when desired-class
		      (setq c (semantic-analyze-tags-of-class-list c desired-class)))

		    (if do-unique
			(if c
			    ;; Pull out trash.
			    ;; NOTE TO SELF: Is this too slow?
			    (setq c (semantic-unique-tag-table-by-name c))
			  (setq c (semantic-unique-tag-table-by-name origc)))
		      (when (not c)
			(setq c origc)))

		    ;; All done!
		    c))))

            ;; Default semanticdb directory
            (setq-default semanticdb-default-save-directory "~/.emacs.cache/semanticdb")

            ;; semanticdb support for global/gtags
            (when (executable-find "global")
              (semanticdb-enable-gnu-global-databases 'c-mode t)
              (semanticdb-enable-gnu-global-databases 'c++-mode t))

	    ;; Helper functions to parse source code under directory using Semantic
            (defvar semantic/c-files-regex ".*\\.\\(c\\|cpp\\|h\\|hpp\\)"
              "A regular expression to match any c/c++ related files under a directory")

            (defun semantic/semantic-parse-dir (root regex)
              "This function is an attempt of mine to force semantic to
     parse all source files under a root directory. Arguments:
     -- root: The full path to the root directory
     -- regex: A regular expression against which to match all files in the directory"
              (let (;;make sure that root has a trailing slash and is a dir
                    (root (file-name-as-directory root))
                    (files (directory-files root t )))
                ;; remove current dir and parent dir from list
                (setq files (delete (format "%s." root) files))
                (setq files (delete (format "%s.." root) files))
                (while files
                  (setq file (pop files))
                  (if (not(file-accessible-directory-p file))
                      ;;if it's a file that matches the regex we seek
                      (progn (when (string-match-p regex file)
                               (save-excursion
                                 (semanticdb-file-table-object file))))
                    ;;else if it's a directory
                    (semantic/semantic-parse-dir file regex)))))

            (defun semantic/semantic-parse-current-dir (regex)
              "Parses all files under the current directory matching regex"
              (semantic/semantic-parse-dir (file-name-directory(buffer-file-name)) regex))

            (defun semantic/parse-curdir-c ()
              "Parses all the c/c++ related files under the current directory
     and inputs their data into semantic"
              (interactive)
              (semantic/semantic-parse-current-dir semantic/c-files-regex))

            (defun semantic/parse-dir-c (dir)
              "Prompts the user for a directory and parses all c/c++ related files
     under the directory"
              (interactive (list (read-directory-name "Provide the directory to search in:")))
              (semantic/semantic-parse-dir (expand-file-name dir) semantic/c-files-regex))))

;; Load contrib library
(use-package eassist)

;; EDE project managment, slows down Emacs
(use-package ede
  :disabled t
  :config (progn
            (global-ede-mode 1)
            (ede-enable-generic-projects)
            (setq ede-project-directories t)

            ;; Default EDE directory
            (setq-default ede-project-placeholder-cache-file "~/.emacs.cache/ede-projects.el")

            ;; Redefine ede add projects to avoid errors
            (defun ede-add-project-to-global-list (proj)
              "Add the project PROJ to the master list of projects.
On success, return the added project."
              (ignore-errors
                (when (not proj)
                  (error "No project created to add to master list"))
                (when (not (eieio-object-p proj))
                  (error "Attempt to add non-object to master project list"))
                (when (not (obj-of-class-p proj ede-project-placeholder))
                  (error "Attempt to add a non-project to the ede projects list"))
                (if (stringp proj)
                    (add-to-list 'ede-projects proj)))
              proj)))

;; Show function in mode-line
(use-package which-func
  :defer t
  :commands which-function-mode
  :config (progn
            ;; Enable which-function-mode for selected major modes
            (setq which-func-unknown "⊥"
                  which-func-maxout 1024
                  which-func-modes '(latex-mode
                                     markdown-mode
                                     c-mode
                                     emacs-lisp-mode
                                     org-mode
                                     c++-mode)
                  which-func-format
                  `(" "
                    (:propertize which-func-current local-map
                                 (keymap
                                  (mode-line keymap
                                             (mouse-3 . end-of-defun)
                                             (mouse-2 . narrow-to-defun)
                                             (mouse-1 . beginning-of-defun)))
                                 face which-func
                                 mouse-face mode-line-highlight
                                 help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end")
                    " "))))

(provide 'setup-cedet)
;;; setup-cedet.el ends here
