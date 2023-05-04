;;; autobuild-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
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
   string is interpreted as a ‘compile-command', which is executed via ‘compile'
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

This is a minor mode.  If called interactively, toggle the
`Autobuild mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='autobuild-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "autobuild" '("autobuild-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; autobuild-autoloads.el ends here
