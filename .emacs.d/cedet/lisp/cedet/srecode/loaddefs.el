;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "android" "android.el" (0 0 0 0))
;;; Generated autoloads from android.el

(autoload 'srecode-semantic-handle-:android "android" "\
Add android specific symbols into DICT based on the current project.

\(fn DICT)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "android" '("srecode-android-read-resource-id")))

;;;***

;;;### (autoloads nil "args" "args.el" (0 0 0 0))
;;; Generated autoloads from args.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "args" '("srecode-")))

;;;***

;;;### (autoloads nil "ctxt" "ctxt.el" (0 0 0 0))
;;; Generated autoloads from ctxt.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ctxt" '("srecode-")))

;;;***

;;;### (autoloads nil "dictionary" "dictionary.el" (0 0 0 0))
;;; Generated autoloads from dictionary.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dictionary" '("srecode-" "initialize-instance")))

;;;***

;;;### (autoloads nil "extract" "extract.el" (0 0 0 0))
;;; Generated autoloads from extract.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "extract" '("srecode-")))

;;;***

;;;### (autoloads nil "fields" "fields.el" (0 0 0 0))
;;; Generated autoloads from fields.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "fields" '("srecode-" "initialize-instance")))

;;;***

;;;### (autoloads nil "filters" "filters.el" (0 0 0 0))
;;; Generated autoloads from filters.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "filters" '("srecode-comment-prefix")))

;;;***

;;;### (autoloads nil "find" "find.el" (0 0 0 0))
;;; Generated autoloads from find.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "find" '("srecode-")))

;;;***

;;;### (autoloads nil "semantic" "semantic.el" (0 0 0 0))
;;; Generated autoloads from semantic.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "semantic" '("srecode-semantic-")))

;;;***

;;;### (autoloads nil "srecode/compile" "compile.el" (0 0 0 0))
;;; Generated autoloads from compile.el

(autoload 'srecode-compile-templates "srecode/compile" "\
Compile a semantic recode template file into a mode-local variable.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "srecode/compile" '("srecode-")))

;;;***

;;;### (autoloads nil "srecode/cpp" "cpp.el" (0 0 0 0))
;;; Generated autoloads from cpp.el

(autoload 'srecode-semantic-handle-:c "srecode/cpp" "\
Add macros into the dictionary DICT based on the current c file.
Adds the following:
FILENAME_SYMBOL - filename converted into a C compat symbol.
HEADER - Shown section if in a header file.

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:cpp "srecode/cpp" "\
Add macros into the dictionary DICT based on the current c file.
Calls `srecode-semantic-handle-:c.
Also adds the following:
 - nothing -

\(fn DICT)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "srecode/cpp" '("srecode-")))

;;;***

;;;### (autoloads nil "srecode/document" "document.el" (0 0 0 0))
;;; Generated autoloads from document.el

(autoload 'srecode-document-insert-comment "srecode/document" "\
Insert some comments.
Whack any comments that may be in the way and replace them.
If the region is active, then insert group function comments.
If the cursor is in a comment, figure out what kind of comment it is
  and replace it.
If the cursor is in a function, insert a function comment.
If the cursor is on a one line prototype, then insert post-fcn comments.

\(fn)" t nil)

(autoload 'srecode-document-insert-function-comment "srecode/document" "\
Insert or replace a function comment.
FCN-IN is the Semantic tag of the function to add a comment too.
If FCN-IN is not provided, the current tag is used instead.
It is assumed that the comment occurs just in front of FCN-IN.

\(fn &optional FCN-IN)" t nil)

(autoload 'srecode-document-insert-variable-one-line-comment "srecode/document" "\
Insert or replace a variable comment.
VAR-IN is the Semantic tag of the function to add a comment too.
If VAR-IN is not provided, the current tag is used instead.
It is assumed that the comment occurs just after VAR-IN.

\(fn &optional VAR-IN)" t nil)

(autoload 'srecode-document-insert-group-comments "srecode/document" "\
Insert group comments around the active between BEG and END.
If the region includes only parts of some tags, expand out
to the beginning and end of the tags on the region.
If there is only one tag in the region, complain.

\(fn BEG END)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "srecode/document" '("srecode-document-")))

;;;***

;;;### (autoloads nil "srecode/el" "el.el" (0 0 0 0))
;;; Generated autoloads from el.el

(autoload 'srecode-semantic-handle-:el "srecode/el" "\
Add macros into the dictionary DICT based on the current Emacs Lisp file.
Adds the following:
  PRENAME - The common name prefix of this file.

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:el-custom "srecode/el" "\
Add macros into the dictionary DICT based on the current Emacs Lisp file.
Adds the following:
  GROUP - The 'defgroup' name we guess you want for variables.
  FACEGROUP - The `defgroup' name you might want for faces.

\(fn DICT)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "srecode/el" '("srecode-semantic-apply-tag-to-dict")))

;;;***

;;;### (autoloads nil "srecode/expandproto" "expandproto.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from expandproto.el

(autoload 'srecode-insert-prototype-expansion "srecode/expandproto" "\
Insert get/set methods for the current class.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "srecode/expandproto" '("srecode-")))

;;;***

;;;### (autoloads nil "srecode/getset" "getset.el" (0 0 0 0))
;;; Generated autoloads from getset.el

(autoload 'srecode-insert-getset "srecode/getset" "\
Insert get/set methods for the current class.
CLASS-IN is the semantic tag of the class to update.
FIELD-IN is the semantic tag, or string name, of the field to add.
If you do not specify CLASS-IN or FIELD-IN then a class and field
will be derived.

\(fn &optional CLASS-IN FIELD-IN)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "srecode/getset" '("srecode-")))

;;;***

;;;### (autoloads nil "srecode/insert" "insert.el" (0 0 0 0))
;;; Generated autoloads from insert.el

(autoload 'srecode-insert "srecode/insert" "\
Insert the template TEMPLATE-NAME into the current buffer at point.
DICT-ENTRIES are additional dictionary values to add.

\(fn TEMPLATE-NAME &rest DICT-ENTRIES)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "srecode/insert" '("srecode-")))

;;;***

;;;### (autoloads nil "srecode/java" "java.el" (0 0 0 0))
;;; Generated autoloads from java.el

(autoload 'srecode-semantic-handle-:java "srecode/java" "\
Add macros into the dictionary DICT based on the current java file.
Adds the following:
FILENAME_AS_PACKAGE - file/dir converted into a java package name.
FILENAME_AS_CLASS - file converted to a Java class name.

\(fn DICT)" nil nil)

;;;***

;;;### (autoloads nil "srecode/m3" "m3.el" (0 0 0 0))
;;; Generated autoloads from m3.el

(autoload 'srecode-m3-items "srecode/m3" "\
Return a list of menu items based on SRecode features.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "srecode/map" "map.el" (0 0 0 0))
;;; Generated autoloads from map.el

(autoload 'srecode-get-maps "srecode/map" "\
Get a list of maps relevant to the current buffer.
Optional argument RESET forces a reset of the current map.

\(fn &optional RESET)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "srecode/map" '("srecode-")))

;;;***

;;;### (autoloads nil "srecode/mode" "mode.el" (0 0 0 0))
;;; Generated autoloads from mode.el

(autoload 'srecode-minor-mode "srecode/mode" "\
Toggle srecode minor mode.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled.

\\{srecode-mode-map}

\(fn &optional ARG)" t nil)

(defvar global-srecode-minor-mode nil "\
Non-nil if Global Srecode minor mode is enabled.
See the `global-srecode-minor-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-srecode-minor-mode'.")

(custom-autoload 'global-srecode-minor-mode "srecode/mode" nil)

(autoload 'global-srecode-minor-mode "srecode/mode" "\
Toggle global use of srecode minor mode.
If ARG is positive or nil, enable, if it is negative, disable.

\(fn &optional ARG)" t nil)

(add-to-list 'auto-mode-alist '("\\.srt$" . srecode-template-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "srecode/mode" '("srecode-")))

;;;***

;;;### (autoloads nil "srecode/srt" "srt.el" (0 0 0 0))
;;; Generated autoloads from srt.el

(autoload 'srecode-semantic-handle-:srt "srecode/srt" "\
Add macros into the dictionary DICT based on the current SRT file.
Adds the following:
ESCAPE_START - This files value of escape_start
ESCAPE_END - This files value of escape_end
MODE - The mode of this buffer.  If not declared yet, guess.

\(fn DICT)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "srecode/srt" '("srecode-read-")))

;;;***

;;;### (autoloads nil "srecode/srt-mode" "srt-mode.el" (0 0 0 0))
;;; Generated autoloads from srt-mode.el

(autoload 'srecode-template-mode "srecode/srt-mode" "\
Major-mode for writing SRecode macros.

\(fn)" t nil)

(defalias 'srt-mode 'srecode-template-mode)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "srecode/srt-mode" '("semantic-" "srecode-")))

;;;***

;;;### (autoloads nil "srecode/template" "template.el" (0 0 0 0))
;;; Generated autoloads from template.el

(autoload 'srecode-template-setup-parser "srecode/template" "\
Setup buffer for parse.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "srecode/template" '("semantic-tag-components")))

;;;***

;;;### (autoloads nil "srecode/texi" "texi.el" (0 0 0 0))
;;; Generated autoloads from texi.el

(autoload 'srecode-semantic-handle-:texi "srecode/texi" "\
Add macros into the dictionary DICT based on the current texinfo file.
Adds the following:
  LEVEL - chapter, section, subsection, etc
  NEXTLEVEL - One below level

\(fn DICT)" nil nil)

(autoload 'srecode-semantic-handle-:texitag "srecode/texi" "\
Add macros into the dictionary DICT based on the current :tag file.
Adds the following:
  TAGDOC - Texinfo formatted doc string for :tag.

\(fn DICT)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "srecode/texi" '("semantic-insert-foreign-tag" "srecode-texi-")))

;;;***

;;;### (autoloads nil "srt-wy" "srt-wy.el" (0 0 0 0))
;;; Generated autoloads from srt-wy.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "srt-wy" '("wisent-srecode-template-lexer" "srecode-template-")))

;;;***

;;;### (autoloads nil "table" "table.el" (0 0 0 0))
;;; Generated autoloads from table.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "table" '("srecode-" "object-sort-list")))

;;;***

(provide 'loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs.el ends here
