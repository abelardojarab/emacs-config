;;; autobuild-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "autobuild" "autobuild.el" (0 0 0 0))
;;; Generated autoloads from autobuild.el

(autoload 'autobuild-define-rule "autobuild" "\
Define a build rule NAME.

   When ‘major-mode' is in MODE-FILTER, or when MODE-FILTER is nil,
   the action-generator BODY is evaluated, which returns an action
   which must be one of the following types:

   nil if the generator doesn't know how to generate an action.
   string is interpreted as a compile-command, which is executed via ‘compile'
   function is executed via ‘funcall'

\(fn NAME MODE-FILTER &rest BODY)" nil t)

(function-put 'autobuild-define-rule 'lisp-indent-function 'defun)

(autoload 'autobuild-pipeline "autobuild" "\
Define a build pipeline.

  Each entry in BUFFER-RULE-LIST has the form (BUFFER RULE),
  where BUFFER is the next buffer in the pipeline, and RULE
  is the rule to invoke within BUFFER to generate an action.

  If ACTION is either a (compile command) string or a function that
  returns a compilation buffer, compilation is executed asynchronously
  and the pipeline is resumed upon compilation finish.  Otherwise, ACTION
  is executed synchronously.

  If any step in the compilation pipeline fails, via either an error or
  an abnormal compilation finish state, any remaining steps in the pipeline
  are aborted.

\(fn &rest BUFFER-RULE-LIST)" nil t)

(autoload 'autobuild-build "autobuild" "\
Build the current buffer.

   If PROMPT is non-nil or if there is no known last rule for
    the current buffer,
   prompt for selection of one of the currently-applicable build rules.
   Otherwise, chose the last-executed build rule, if known,
   or the rule with the lowest NICE property (highest priority).

\(fn &optional PROMPT)" t nil)

(defvar autobuild-mode nil "\
Non-nil if Autobuild mode is enabled.
See the `autobuild-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `autobuild-mode'.")

(custom-autoload 'autobuild-mode "autobuild" nil)

(autoload 'autobuild-mode "autobuild" "\
Define and execute build rules and compilation pipelines.

If called interactively, enable Autobuild mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it
if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "autobuild" '("autobuild-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; autobuild-autoloads.el ends here
