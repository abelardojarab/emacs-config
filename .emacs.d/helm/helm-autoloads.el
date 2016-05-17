;;; helm-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (helm-debug-toggle helm-other-buffer helm helm-debug-open-last-log
;;;;;;  helm-define-key-with-subkeys helm-multi-key-defun helm-define-multi-key)
;;;;;;  "helm" "helm.el" (22331 28205))
;;; Generated autoloads from helm.el

(autoload 'helm-define-multi-key "helm" "\
In KEYMAP, define key sequence KEY for function list FUNCTIONS.
Each function runs sequentially for each KEY press. 
If DELAY is specified, switch back to initial function of FUNCTIONS list
after DELAY seconds.
The functions in FUNCTIONS list take no args.
e.g
  (defun foo ()
    (message \"Run foo\"))
  (defun bar ()
    (message \"Run bar\"))
  (defun baz ()
    (message \"Run baz\"))

\(helm-define-multi-key global-map \"<f5> q\" '(foo bar baz) 2)

Each time \"<f5> q\" is pressed, the next function is executed. Waiting 
more than 2 seconds between key presses switches back to executing the first 
function on the next hit.

\(fn KEYMAP KEY FUNCTIONS &optional DELAY)" nil nil)

(autoload 'helm-multi-key-defun "helm" "\
Define NAME as a multi-key command running FUNS.
After DELAY seconds, the FUNS list is reinitialized.
See `helm-define-multi-key'.

\(fn NAME DOCSTRING FUNS &optional DELAY)" nil (quote macro))

(autoload 'helm-define-key-with-subkeys "helm" "\
Defines in MAP a KEY and SUBKEY to COMMAND.

This allows typing KEY to call COMMAND the first time and
type only SUBKEY on subsequent calls.

Arg MAP is the keymap to use, SUBKEY is the initial short key-binding to
call COMMAND.

Arg OTHER-SUBKEYS is an alist specifying other short key-bindings
to use once started e.g:

    (helm-define-key-with-subkeys global-map
       (kbd \"C-x v n\") ?n 'git-gutter:next-hunk '((?p . git-gutter:previous-hunk)))


In this example, `C-x v n' will run `git-gutter:next-hunk'
subsequent \"n\"'s run this command again
and subsequent \"p\"'s run `git-gutter:previous-hunk'.

Arg MENU is a string displayed in minibuffer that 
describes SUBKEY and OTHER-SUBKEYS.
Arg EXIT-FN specifies a function to run on exit.

For any other keys pressed, run their assigned command as defined
in MAP and then exit the loop running EXIT-FN, if specified.

NOTE: SUBKEY and OTHER-SUBKEYS bindings support char syntax only 
\(e.g ?n), so don't use strings or vectors to define them.

\(fn MAP KEY SUBKEY COMMAND &optional OTHER-SUBKEYS MENU EXIT-FN)" nil nil)

(autoload 'helm-debug-open-last-log "helm" "\
Open helm log file of last helm session.
If `helm-last-log-file' is nil, switch to `helm-debug-buffer' .

\(fn)" t nil)

(autoload 'helm "helm" "\
Main function to execute helm sources.

Keywords supported:
:sources :input :prompt :resume :preselect
:buffer :keymap :default :history :allow-nest

Extra LOCAL-VARS keywords are supported, see below.

PLIST is a list like (:key1 val1 :key2 val2 ...) or
\(&optional sources input prompt resume
            preselect buffer keymap default history).

Basic keywords are the following:

:sources

A list of sources used for this session.  It also accepts a
symbol, interpreted as a variable of a helm source
i.e (a symbol can be passed instead of a list of sources).
It also accepts an alist representing a helm source, which is
detected by (assq 'name ANY-SOURCES).
NOTE: In this case the source is embedded in the helm command and
have no symbol name, so it is not reachable from outside.
It will be referenced in `helm-sources' as a whole alist.

:input

Temporary value of `helm-pattern', ie. initial input of minibuffer.

:prompt

Prompt other than \"pattern: \".

:resume

If t, Resurrect previously instance of `helm'.  Skip the initialization.
If 'noresume, this instance of `helm' cannot be resumed.

:preselect

Initially selected candidate.  Specified by exact candidate or a regexp.

:buffer

`helm-buffer' instead of *helm*.

:keymap

`helm-map' for current `helm' session.

:default

A default argument that will be inserted in minibuffer with
\\<minibuffer-local-map>\\[next-history-element]. When nil or not
present `thing-at-point' will be used instead. If
`helm--maybe-use-default-as-input' is non-`nil' display will be
updated using :default arg as input unless :input is specified,
which in this case will take precedence over :default. This is a
string or a list. If list, car of the list becomes initial
default input. \\<minibuffer-local-map>\\[next-history-element]
cycles through the list items.

:history

Minibuffer input, by default, is pushed to `minibuffer-history'.
When an argument HISTORY is provided, input is pushed to
HISTORY. The HISTORY element should be a valid symbol.

:allow-nest

Allow running this helm command in a running helm session.

Standard arguments are supported. These two are the same:

\(helm :sources sources :input input :prompt prompt :resume resume
       :preselect preselect :buffer buffer :keymap keymap :default default
       :history history)

and

\(helm sources input prompt resume preselect buffer keymap default history)

are the same for now. However, the use of non-keyword args is
deprecated and should not be used.

Other keywords are interpreted as local variables of this helm
session. The `helm-' prefix can be omitted. For example,

\(helm :sources 'helm-source-buffers-list
       :buffer \"*helm buffers*\" :candidate-number-limit 10)

starts helm session with `helm-source-buffers' source in
*helm buffers* buffer and sets variable `helm-candidate-number-limit'
to 10 as a session local variable.

\(fn &key SOURCES INPUT PROMPT RESUME PRESELECT BUFFER KEYMAP DEFAULT HISTORY ALLOW-NEST OTHER-LOCAL-VARS)" nil nil)

(autoload 'helm-other-buffer "helm" "\
Simplified `helm' interface with other `helm-buffer'.
Call `helm' only with ANY-SOURCES and ANY-BUFFER as args.

\(fn ANY-SOURCES ANY-BUFFER)" nil nil)

(autoload 'helm-debug-toggle "helm" "\
Enable/disable helm debugging from outside of helm session.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-reset-adaptive-history helm-adaptive-mode)
;;;;;;  "helm-adaptive" "helm-adaptive.el" (22331 28204))
;;; Generated autoloads from helm-adaptive.el

(defvar helm-adaptive-mode nil "\
Non-nil if Helm-Adaptive mode is enabled.
See the command `helm-adaptive-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-adaptive-mode'.")

(custom-autoload 'helm-adaptive-mode "helm-adaptive" nil)

(autoload 'helm-adaptive-mode "helm-adaptive" "\
Toggle adaptive sorting in all sources.

\(fn &optional ARG)" t nil)

(autoload 'helm-reset-adaptive-history "helm-adaptive" "\
Delete all `helm-adaptive-history' and his file.
Useful when you have a old or corrupted `helm-adaptive-history-file'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-apt) "helm-apt" "helm-apt.el" (22331 28204))
;;; Generated autoloads from helm-apt.el

(autoload 'helm-apt "helm-apt" "\
Preconfigured `helm' : frontend of APT package manager.
With a prefix arg reload cache.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (helm-filtered-bookmarks helm-bookmarks) "helm-bookmark"
;;;;;;  "helm-bookmark.el" (22331 28204))
;;; Generated autoloads from helm-bookmark.el

(autoload 'helm-bookmarks "helm-bookmark" "\
Preconfigured `helm' for bookmarks.

\(fn)" t nil)

(autoload 'helm-filtered-bookmarks "helm-bookmark" "\
Preconfigured helm for bookmarks (filtered by category).
Optional source `helm-source-bookmark-addressbook' is loaded
only if external library addressbook-bookmark.el is available.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-mini helm-buffers-list) "helm-buffers" "helm-buffers.el"
;;;;;;  (22331 28204))
;;; Generated autoloads from helm-buffers.el

(autoload 'helm-buffers-list "helm-buffers" "\
Preconfigured `helm' to list buffers.

\(fn)" t nil)

(autoload 'helm-mini "helm-buffers" "\
Preconfigured `helm' lightweight version (buffer -> recentf).

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-colors) "helm-color" "helm-color.el" (22331
;;;;;;  28204))
;;; Generated autoloads from helm-color.el

(autoload 'helm-colors "helm-color" "\
Preconfigured `helm' for color.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-M-x) "helm-command" "helm-command.el" (22331
;;;;;;  28204))
;;; Generated autoloads from helm-command.el

(autoload 'helm-M-x "helm-command" "\
Preconfigured `helm' for Emacs commands.
It is `helm' replacement of regular `M-x' `execute-extended-command'.

Unlike regular `M-x' emacs vanilla `execute-extended-command' command,
the prefix args if needed, are passed AFTER starting `helm-M-x'.

You can get help on each command by persistent action.

\(fn ARG &optional COMMAND-NAME)" t nil)

;;;***

;;;### (autoloads (helm-configuration) "helm-config" "helm-config.el"
;;;;;;  (22331 28204))
;;; Generated autoloads from helm-config.el

(autoload 'helm-configuration "helm-config" "\
Customize `helm'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-dabbrev) "helm-dabbrev" "helm-dabbrev.el"
;;;;;;  (22331 28204))
;;; Generated autoloads from helm-dabbrev.el

(autoload 'helm-dabbrev "helm-dabbrev" "\
Preconfigured helm for dynamic abbreviations.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-complex-command-history helm-timers helm-locate-library
;;;;;;  helm-manage-advice helm-apropos helm-lisp-completion-or-file-name-at-point
;;;;;;  helm-lisp-indent helm-complete-file-name-at-point helm-lisp-completion-at-point)
;;;;;;  "helm-elisp" "helm-elisp.el" (22331 28204))
;;; Generated autoloads from helm-elisp.el

(autoload 'helm-lisp-completion-at-point "helm-elisp" "\
Preconfigured helm for lisp symbol completion at point.

\(fn)" t nil)

(autoload 'helm-complete-file-name-at-point "helm-elisp" "\
Preconfigured helm to complete file name at point.

\(fn &optional FORCE)" t nil)

(autoload 'helm-lisp-indent "helm-elisp" "\
Not documented

\(fn)" t nil)

(autoload 'helm-lisp-completion-or-file-name-at-point "helm-elisp" "\
Preconfigured helm to complete lisp symbol or filename at point.
Filename completion happen if string start after or between a double quote.

\(fn)" t nil)

(autoload 'helm-apropos "helm-elisp" "\
Preconfigured helm to describe commands, functions, variables and faces.
In non interactives calls DEFAULT argument should be provided as a string,
i.e the `symbol-name' of any existing symbol.

\(fn DEFAULT)" t nil)

(autoload 'helm-manage-advice "helm-elisp" "\
Preconfigured `helm' to disable/enable function advices.

\(fn)" t nil)

(autoload 'helm-locate-library "helm-elisp" "\
Preconfigured helm to locate elisp libraries.

\(fn)" t nil)

(autoload 'helm-timers "helm-elisp" "\
Preconfigured `helm' for timers.

\(fn)" t nil)

(autoload 'helm-complex-command-history "helm-elisp" "\
Preconfigured helm for complex command history.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-list-elisp-packages-no-fetch helm-list-elisp-packages)
;;;;;;  "helm-elisp-package" "helm-elisp-package.el" (22331 28204))
;;; Generated autoloads from helm-elisp-package.el

(autoload 'helm-list-elisp-packages "helm-elisp-package" "\
Preconfigured helm for listing and handling emacs packages.

\(fn ARG)" t nil)

(autoload 'helm-list-elisp-packages-no-fetch "helm-elisp-package" "\
Preconfigured helm for emacs packages.
Same as `helm-list-elisp-packages' but don't fetch packages on remote.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-elscreen-history helm-elscreen) "helm-elscreen"
;;;;;;  "helm-elscreen.el" (22331 28204))
;;; Generated autoloads from helm-elscreen.el

(autoload 'helm-elscreen "helm-elscreen" "\
Preconfigured helm to list elscreen.

\(fn)" t nil)

(autoload 'helm-elscreen-history "helm-elscreen" "\
Preconfigured helm to list elscreen in history order.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-eshell-history helm-esh-pcomplete) "helm-eshell"
;;;;;;  "helm-eshell.el" (22331 28204))
;;; Generated autoloads from helm-eshell.el

(autoload 'helm-esh-pcomplete "helm-eshell" "\
Preconfigured helm to provide helm completion in eshell.

\(fn)" t nil)

(autoload 'helm-eshell-history "helm-eshell" "\
Preconfigured helm for eshell history.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-calcul-expression helm-eval-expression-with-eldoc
;;;;;;  helm-eval-expression) "helm-eval" "helm-eval.el" (22331 28204))
;;; Generated autoloads from helm-eval.el

(autoload 'helm-eval-expression "helm-eval" "\
Preconfigured helm for `helm-source-evaluation-result'.

\(fn ARG)" t nil)

(autoload 'helm-eval-expression-with-eldoc "helm-eval" "\
Preconfigured helm for `helm-source-evaluation-result' with `eldoc' support. 

\(fn)" t nil)

(autoload 'helm-calcul-expression "helm-eval" "\
Preconfigured helm for `helm-source-calculation-result'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-run-external-command) "helm-external" "helm-external.el"
;;;;;;  (22331 28204))
;;; Generated autoloads from helm-external.el

(autoload 'helm-run-external-command "helm-external" "\
Preconfigured `helm' to run External PROGRAM asyncronously from Emacs.
If program is already running exit with error.
You can set your own list of commands with
`helm-external-commands-list'.

\(fn PROGRAM)" t nil)

;;;***

;;;### (autoloads (helm-recentf helm-multi-files helm-for-files helm-find-files
;;;;;;  helm-find helm-browse-project) "helm-files" "helm-files.el"
;;;;;;  (22331 28204))
;;; Generated autoloads from helm-files.el

(autoload 'helm-browse-project "helm-files" "\
Preconfigured helm to browse projects.
Browse files and see status of project with its vcs.
Only HG and GIT are supported for now.
Fall back to `helm-browse-project-find-files'
if current directory is not under control of one of those vcs.
With a prefix ARG browse files recursively, with two prefix ARG
rebuild the cache.
If the current directory is found in the cache, start
`helm-browse-project-find-files' even with no prefix ARG.
NOTE: The prefix ARG have no effect on the VCS controlled directories.

Needed dependencies for VCS:
<https://github.com/emacs-helm/helm-ls-git>
and
<https://github.com/emacs-helm/helm-ls-hg>
and
<http://melpa.org/#/helm-ls-svn>.

\(fn ARG)" t nil)

(autoload 'helm-find "helm-files" "\
Preconfigured `helm' for the find shell command.

Recursively find files whose names are matched by all specified
globbing PATTERNs under the current directory using the external
program specified in `find-program' (usually \"find\").  Every
input PATTERN is silently wrapped into two stars: *PATTERN*.

With prefix argument, prompt for a directory to search.

When user option `helm-findutils-search-full-path' is non-nil,
match against complete paths, otherwise, against file names
without directory part.

The (possibly empty) list of globbing PATTERNs can be followed by
the separator \"*\" plus any number of additional arguments that
are passed to \"find\" literally.

\(fn ARG)" t nil)

(autoload 'helm-find-files "helm-files" "\
Preconfigured `helm' for helm implementation of `find-file'.
Called with a prefix arg show history if some.
Don't call it from programs, use `helm-find-files-1' instead.
This is the starting point for nearly all actions you can do on files.

\(fn ARG)" t nil)

(autoload 'helm-for-files "helm-files" "\
Preconfigured `helm' for opening files.
Run all sources defined in `helm-for-files-preferred-list'.

\(fn)" t nil)

(autoload 'helm-multi-files "helm-files" "\
Preconfigured helm similar to `helm-for-files' but that don't run locate.
Allow toggling from locate to others sources.
This allow seeing first if what you search is in other sources before launching
locate.

\(fn)" t nil)

(autoload 'helm-recentf "helm-files" "\
Preconfigured `helm' for `recentf'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-ucs helm-select-xfont) "helm-font" "helm-font.el"
;;;;;;  (22331 28204))
;;; Generated autoloads from helm-font.el

(autoload 'helm-select-xfont "helm-font" "\
Preconfigured `helm' to select Xfont.

\(fn)" t nil)

(autoload 'helm-ucs "helm-font" "\
Preconfigured helm for `ucs-names' math symbols.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-grep-do-git-grep helm-do-grep-ag helm-goto-next-file
;;;;;;  helm-goto-precedent-file) "helm-grep" "helm-grep.el" (22331
;;;;;;  28204))
;;; Generated autoloads from helm-grep.el

(autoload 'helm-goto-precedent-file "helm-grep" "\
Go to precedent file in helm grep/etags buffers.

\(fn)" t nil)

(autoload 'helm-goto-next-file "helm-grep" "\
Go to precedent file in helm grep/etags buffers.

\(fn)" t nil)

(autoload 'helm-do-grep-ag "helm-grep" "\
Preconfigured helm for grepping with AG in `default-directory'.
With prefix-arg prompt for type if available with your AG version.

\(fn ARG)" t nil)

(autoload 'helm-grep-do-git-grep "helm-grep" "\
Preconfigured helm for git-grepping `default-directory'.
With a prefix arg ARG git-grep the whole repository.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (helm-describe-helm-attribute helm-documentation)
;;;;;;  "helm-help" "helm-help.el" (22331 28204))
;;; Generated autoloads from helm-help.el

(autoload 'helm-documentation "helm-help" "\
Preconfigured helm for helm documentation.
With a prefix arg refresh the documentation.

Find here the documentation of all sources actually documented.

\(fn ARG)" t nil)

(defvar helm-comp-read-mode-line "\\<helm-comp-read-map>C/\\[helm-cr-empty-string]:Empty \\<helm-map>\\[helm-help]:Help \\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend")

(defvar helm-read-file-name-mode-line-string "\\<helm-read-file-map>\\[helm-help]:Help C/\\[helm-cr-empty-string]:Empty \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend" "\
String displayed in mode-line in `helm-source-find-files'.")

(defvar helm-top-mode-line "\\<helm-top-map>\\[helm-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend")

(autoload 'helm-describe-helm-attribute "helm-help" "\
Display the full documentation of HELM-ATTRIBUTE.
HELM-ATTRIBUTE should be a symbol.

\(fn HELM-ATTRIBUTE)" t nil)

;;;***

;;;### (autoloads (helm-gid) "helm-id-utils" "helm-id-utils.el" (22331
;;;;;;  28204))
;;; Generated autoloads from helm-id-utils.el

(autoload 'helm-gid "helm-id-utils" "\
Preconfigured helm for `gid' command line of `ID-Utils'.
Need A database created with the command `mkid'
above `default-directory'.
Need id-utils as dependency which provide `mkid', `gid' etc...
See <https://www.gnu.org/software/idutils/>.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-imenu-in-all-buffers helm-imenu) "helm-imenu"
;;;;;;  "helm-imenu.el" (22331 28204))
;;; Generated autoloads from helm-imenu.el

(autoload 'helm-imenu "helm-imenu" "\
Preconfigured `helm' for `imenu'.

\(fn)" t nil)

(autoload 'helm-imenu-in-all-buffers "helm-imenu" "\
Preconfigured helm for fetching imenu entries in all buffers with similar mode as current.
A mode is similar as current if it is the same or it is derived i.e `derived-mode-p'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-info-at-point helm-info) "helm-info" "helm-info.el"
;;;;;;  (22331 28204))
;;; Generated autoloads from helm-info.el

(autoload 'helm-info "helm-info" "\
Preconfigured `helm' for searching Info files' indices.

\(fn)" t nil)

(autoload 'helm-info-at-point "helm-info" "\
Preconfigured `helm' for searching info at point.
With a prefix-arg insert symbol at point.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-locate helm-projects-find-files) "helm-locate"
;;;;;;  "helm-locate.el" (22331 28204))
;;; Generated autoloads from helm-locate.el

(autoload 'helm-projects-find-files "helm-locate" "\
Find files with locate in `helm-locate-project-list'.
With a prefix arg refresh the database in each project.

\(fn UPDATE)" t nil)

(autoload 'helm-locate "helm-locate" "\
Preconfigured `helm' for Locate.
Note: you can add locate options after entering pattern.
See 'man locate' for valid options and also `helm-locate-command'.

You can specify a local database with prefix argument ARG.
With two prefix arg, refresh the current local db or create it
if it doesn't exists.

To create a user specific db, use
\"updatedb -l 0 -o db_path -U directory\".
Where db_path is a filename matched by
`helm-locate-db-file-regexp'.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (helm-man-woman) "helm-man" "helm-man.el" (22331
;;;;;;  28204))
;;; Generated autoloads from helm-man.el

(autoload 'helm-man-woman "helm-man" "\
Preconfigured `helm' for Man and Woman pages.
With a prefix arg reinitialize the cache.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (helm-comint-input-ring helm-minibuffer-history
;;;;;;  helm-stumpwm-commands helm-ratpoison-commands helm-insert-latex-math
;;;;;;  helm-world-time helm-browse-menubar) "helm-misc" "helm-misc.el"
;;;;;;  (22331 28204))
;;; Generated autoloads from helm-misc.el

(autoload 'helm-browse-menubar "helm-misc" "\
Preconfigured helm to the menubar using lacarte.el.

\(fn)" t nil)

(autoload 'helm-world-time "helm-misc" "\
Preconfigured `helm' to show world time.
Default action change TZ environment variable locally to emacs.

\(fn)" t nil)

(autoload 'helm-insert-latex-math "helm-misc" "\
Preconfigured helm for latex math symbols completion.

\(fn)" t nil)

(autoload 'helm-ratpoison-commands "helm-misc" "\
Preconfigured `helm' to execute ratpoison commands.

\(fn)" t nil)

(autoload 'helm-stumpwm-commands "helm-misc" "\
Preconfigured helm for stumpwm commands.

\(fn)" t nil)

(autoload 'helm-minibuffer-history "helm-misc" "\
Preconfigured `helm' for `minibuffer-history'.

\(fn)" t nil)

(autoload 'helm-comint-input-ring "helm-misc" "\
Preconfigured `helm' that provide completion of `comint' history.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-mode) "helm-mode" "helm-mode.el" (22331 28205))
;;; Generated autoloads from helm-mode.el

(cl-defun helm-comp-read (prompt collection &key test initial-input default preselect (buffer "*Helm Completions*") must-match fuzzy reverse-history (requires-pattern 0) history input-history (case-fold helm-comp-read-case-fold-search) (del-input t) (persistent-action nil) (persistent-help "DoNothing") (mode-line helm-comp-read-mode-line) help-message (keymap helm-comp-read-map) (name "Helm Completions") candidates-in-buffer exec-when-only-one quit-when-no-cand (volatile t) sort (fc-transformer 'helm-cr-default-transformer) hist-fc-transformer marked-candidates nomark (alistp t) (candidate-number-limit helm-candidate-number-limit)) "Read a string in the minibuffer, with helm completion.\n\nIt is helm `completing-read' equivalent.\n\n- PROMPT is the prompt name to use.\n\n- COLLECTION can be a list, vector, obarray or hash-table.\n  It can be also a function that receives three arguments:\n  the values string, predicate and t. See `all-completions' for more details.\n\nKeys description:\n\n- TEST: A predicate called with one arg i.e candidate.\n\n- INITIAL-INPUT: Same as input arg in `helm'.\n\n- PRESELECT: See preselect arg of `helm'.\n\n- DEFAULT: This option is used only for compatibility with regular\n  Emacs `completing-read' (Same as DEFAULT arg of `completing-read').\n\n- BUFFER: Name of helm-buffer.\n\n- MUST-MATCH: Candidate selected must be one of COLLECTION.\n\n- FUZZY: Enable fuzzy matching.\n\n- REVERSE-HISTORY: When non--nil display history source after current\n  source completion.\n\n- REQUIRES-PATTERN: Same as helm attribute, default is 0.\n\n- HISTORY: A list containing specific history, default is nil.\n  When it is non--nil, all elements of HISTORY are displayed in\n  a special source before COLLECTION.\n\n- INPUT-HISTORY: A symbol. the minibuffer input history will be\n  stored there, if nil or not provided, `minibuffer-history'\n  will be used instead.\n\n- CASE-FOLD: Same as `helm-case-fold-search'.\n\n- DEL-INPUT: Boolean, when non--nil (default) remove the partial\n  minibuffer input from HISTORY is present.\n\n- PERSISTENT-ACTION: A function called with one arg i.e candidate.\n\n- PERSISTENT-HELP: A string to document PERSISTENT-ACTION.\n\n- MODE-LINE: A string or list to display in mode line.\n  Default is `helm-comp-read-mode-line'.\n\n- KEYMAP: A keymap to use in this `helm-comp-read'.\n  (the keymap will be shared with history source)\n\n- NAME: The name related to this local source.\n\n- EXEC-WHEN-ONLY-ONE: Bound `helm-execute-action-at-once-if-one'\n  to non--nil. (possibles values are t or nil).\n\n- VOLATILE: Use volatile attribute.\n\n- SORT: A predicate to give to `sort' e.g `string-lessp'\n  Use this only on small data as it is ineficient.\n  If you want to sort faster add a sort function to\n  FC-TRANSFORMER.\n  Note that FUZZY when enabled is already providing a sort function.\n\n- FC-TRANSFORMER: A `filtered-candidate-transformer' function\n  or a list of functions.\n\n- HIST-FC-TRANSFORMER: A `filtered-candidate-transformer'\n  function for the history source.\n\n- MARKED-CANDIDATES: If non--nil return candidate or marked candidates as a list.\n\n- NOMARK: When non--nil don't allow marking candidates.\n\n- ALISTP: (default is non--nil) See `helm-comp-read-get-candidates'.\n\n- CANDIDATES-IN-BUFFER: when non--nil use a source build with\n  `helm-source-in-buffer' which is much faster.\n  Argument VOLATILE have no effect when CANDIDATES-IN-BUFFER is non--nil.\n\nAny prefix args passed during `helm-comp-read' invocation will be recorded\nin `helm-current-prefix-arg', otherwise if prefix args were given before\n`helm-comp-read' invocation, the value of `current-prefix-arg' will be used.\nThat's mean you can pass prefix args before or after calling a command\nthat use `helm-comp-read' See `helm-M-x' for example." (when (get-buffer helm-action-buffer) (kill-buffer helm-action-buffer)) (let ((action-fn `(("Sole action (Identity)" lambda (candidate) (if ,marked-candidates (helm-marked-candidates) (identity candidate)))))) (when (eq must-match 'confirm-after-completion) (setq must-match 'confirm)) (let* ((minibuffer-completion-confirm must-match) (must-match-map (when must-match helm-comp-read-must-match-map)) (loc-map (if must-match-map (make-composed-keymap must-match-map (or keymap helm-map)) (or keymap helm-map))) (minibuffer-completion-predicate test) (minibuffer-completion-table collection) (helm-read-file-name-mode-line-string (replace-regexp-in-string "helm-maybe-exit-minibuffer" "helm-confirm-and-exit-minibuffer" helm-read-file-name-mode-line-string)) (get-candidates (lambda nil (let ((cands (helm-comp-read-get-candidates collection test sort alistp))) (setq helm-cr-unknown-pattern-flag nil) (unless (or (eq must-match t) (string= helm-pattern "") (assoc helm-pattern cands) (assoc (intern helm-pattern) cands) (member helm-pattern cands) (member (downcase helm-pattern) cands) (member (upcase helm-pattern) cands)) (setq cands (append (list (replace-regexp-in-string "\\s\\" "" helm-pattern)) cands)) (setq helm-cr-unknown-pattern-flag t)) (if (and default (not (string= default ""))) (delq nil (cons default (delete default cands))) cands)))) (history-get-candidates (lambda nil (let ((all (helm-comp-read-get-candidates history test nil alistp))) (when all (delete "" (helm-fast-remove-dups (if (and default (not (string= default ""))) (delq nil (cons default (delete default all))) all) :test 'equal)))))) (src-hist (helm-build-sync-source (format "%s History" name) :candidates history-get-candidates :fuzzy-match fuzzy :filtered-candidate-transformer (append '((lambda (candidates sources) (cl-loop for i in candidates for cand = (replace-regexp-in-string "\\s\\" "" i) collect cand))) (and hist-fc-transformer (helm-mklist hist-fc-transformer))) :persistent-action persistent-action :persistent-help persistent-help :mode-line mode-line :help-message help-message :action action-fn)) (src (helm-build-sync-source name :candidates get-candidates :filtered-candidate-transformer fc-transformer :requires-pattern requires-pattern :persistent-action persistent-action :persistent-help persistent-help :fuzzy-match fuzzy :mode-line mode-line :help-message help-message :action action-fn :volatile volatile)) (src-1 (helm-build-in-buffer-source name :data get-candidates :filtered-candidate-transformer fc-transformer :requires-pattern requires-pattern :persistent-action persistent-action :fuzzy-match fuzzy :persistent-help persistent-help :mode-line mode-line :help-message help-message :action action-fn)) (src-list (list src-hist (if candidates-in-buffer src-1 src))) (helm-execute-action-at-once-if-one exec-when-only-one) (helm-quit-if-no-candidate quit-when-no-cand) result) (when nomark (setq src-list (cl-loop for src in src-list collect (cons '(nomark) src)))) (when reverse-history (setq src-list (nreverse src-list))) (add-hook 'helm-after-update-hook 'helm-comp-read--move-to-first-real-candidate) (unwind-protect (setq result (helm :sources src-list :input initial-input :default default :preselect preselect :prompt prompt :resume 'noresume :candidate-number-limit candidate-number-limit :case-fold-search case-fold :keymap loc-map :history (and (symbolp input-history) input-history) :buffer buffer)) (remove-hook 'helm-after-update-hook 'helm-comp-read--move-to-first-real-candidate)) (when (and result history del-input) (cond ((and (symbolp history) (not (symbolp (symbol-value history)))) (helm-aif (symbol-value history) (setcar it result))) ((consp history) (setcar history result)) (t (set history (list result))))) (or result (when (and (eq helm-exit-status 0) (eq must-match 'confirm)) (if (and (string= helm-pattern "") default) default (identity helm-pattern))) (unless (or (eq helm-exit-status 1) must-match) default) (helm-mode--keyboard-quit)))))

(cl-defun helm-read-file-name (prompt &key (name "Read File Name") (initial-input default-directory) (buffer "*Helm file completions*") test (case-fold helm-file-name-case-fold-search) preselect history must-match default marked-candidates (candidate-number-limit helm-ff-candidate-number-limit) nomark (alistp t) (persistent-action 'helm-find-files-persistent-action) (persistent-help "Hit1 Expand Candidate, Hit2 or (C-u) Find file") (mode-line helm-read-file-name-mode-line-string)) "Read a file name with helm completion.\nIt is helm `read-file-name' emulation.\n\nArgument PROMPT is the default prompt to use.\n\nKeys description:\n\n- NAME: Source name, default to \"Read File Name\".\n\n- INITIAL-INPUT: Where to start read file name, default to `default-directory'.\n\n- BUFFER: `helm-buffer' name default to \"*Helm Completions*\".\n\n- TEST: A predicate called with one arg 'candidate'.\n\n- CASE-FOLD: Same as `helm-case-fold-search'.\n\n- PRESELECT: helm preselection.\n\n- HISTORY: Display HISTORY in a special source.\n\n- MUST-MATCH: Can be 'confirm, nil, or t.\n\n- MARKED-CANDIDATES: When non--nil return a list of marked candidates.\n\n- NOMARK: When non--nil don't allow marking candidates.\n\n- ALISTP: Don't use `all-completions' in history (take effect only on history).\n\n- PERSISTENT-ACTION: a persistent action function.\n\n- PERSISTENT-HELP: persistent help message.\n\n- MODE-LINE: A mode line message, default is `helm-read-file-name-mode-line-string'." (when (get-buffer helm-action-buffer) (kill-buffer helm-action-buffer)) (when (eq must-match 'confirm-after-completion) (setq must-match 'confirm)) (mapc (lambda (hook) (add-hook 'helm-after-update-hook hook)) '(helm-ff-move-to-first-real-candidate helm-ff-update-when-only-one-matched helm-ff-auto-expand-to-home-or-root)) (let* ((action-fn `(("Sole action (Identity)" lambda (candidate) (if ,marked-candidates (helm-marked-candidates :with-wildcard t) (identity candidate))))) (helm-ff-auto-update-initial-value (and helm-ff-auto-update-initial-value (not (minibuffer-window-active-p (minibuffer-window))))) helm-full-frame (hist (and history (helm-comp-read-get-candidates history nil nil alistp))) (minibuffer-completion-confirm must-match) (must-match-map (when must-match helm-comp-read-must-match-map)) (cmap (if must-match-map (make-composed-keymap must-match-map helm-read-file-map) helm-read-file-map)) (minibuffer-completion-predicate test) (minibuffer-completing-file-name t) (helm-read-file-name-mode-line-string (replace-regexp-in-string "helm-maybe-exit-minibuffer" "helm-confirm-and-exit-minibuffer" helm-read-file-name-mode-line-string)) (src-list (list (helm-build-sync-source (format "%s History" name) :header-name (lambda (name) (concat name (substitute-command-keys helm-find-files-doc-header))) :mode-line mode-line :candidates hist :nohighlight t :persistent-action persistent-action :persistent-help persistent-help :nomark nomark :action action-fn) (helm-build-sync-source name :header-name (lambda (name) (concat name (substitute-command-keys helm-find-files-doc-header))) :init (lambda nil (setq helm-ff-auto-update-flag helm-ff-auto-update-initial-value) (setq helm-ff--auto-update-state helm-ff-auto-update-flag)) :mode-line mode-line :help-message 'helm-read-file-name-help-message :nohighlight t :candidates (lambda nil (append (and (not (file-exists-p helm-pattern)) (list helm-pattern)) (if test (cl-loop with hn = (helm-ff-tramp-hostnames) for i in (helm-find-files-get-candidates must-match) when (or (member i hn) (funcall test i)) collect i) (helm-find-files-get-candidates must-match)))) :filtered-candidate-transformer 'helm-ff-sort-candidates :filter-one-by-one 'helm-ff-filter-candidate-one-by-one :persistent-action persistent-action :persistent-help persistent-help :volatile t :cleanup 'helm-find-files-cleanup :nomark nomark :action action-fn))) (result (helm :sources src-list :input (expand-file-name initial-input) :prompt prompt :keymap cmap :candidate-number-limit candidate-number-limit :resume 'noresume :case-fold-search case-fold :default default :buffer buffer :preselect preselect))) (or (cond ((and result (stringp result) (string= result "") "")) ((and result (stringp result) (file-equal-p result initial-input) default) (if (listp default) (car default) default)) ((and result (stringp result)) (expand-file-name result)) ((and result (listp result)) (mapcar #'expand-file-name result)) (t result)) (when (and (not (string= helm-pattern "")) (eq helm-exit-status 0) (eq must-match 'confirm)) (identity helm-pattern)) (helm-mode--keyboard-quit))))

(defvar helm-mode nil "\
Non-nil if Helm mode is enabled.
See the command `helm-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-mode'.")

(custom-autoload 'helm-mode "helm-mode" nil)

(autoload 'helm-mode "helm-mode" "\
Toggle generic helm completion.

All functions in Emacs that use `completing-read'
or `read-file-name' and friends will use helm interface
when this mode is turned on.
However you can modify this behavior for functions of your choice
with `helm-completing-read-handlers-alist'.

Called with a positive arg, turn on unconditionally, with a
negative arg turn off.
You can turn it on with `helm-mode'.

Some crap emacs functions may not be supported,
e.g `ffap-alternate-file' and maybe others
You can add such functions to `helm-completing-read-handlers-alist'
with a nil value.

Note: This mode is incompatible with Emacs23.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (helm-wikipedia-suggest helm-google-suggest helm-surfraw)
;;;;;;  "helm-net" "helm-net.el" (22331 28205))
;;; Generated autoloads from helm-net.el

(autoload 'helm-surfraw "helm-net" "\
Preconfigured `helm' to search PATTERN with search ENGINE.

\(fn PATTERN ENGINE)" t nil)

(autoload 'helm-google-suggest "helm-net" "\
Preconfigured `helm' for google search with google suggest.

\(fn)" t nil)

(autoload 'helm-wikipedia-suggest "helm-net" "\
Preconfigured `helm' for Wikipedia lookup with Wikipedia suggest.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-org-completing-read-tags helm-org-capture-templates
;;;;;;  helm-org-parent-headings helm-org-in-buffer-headings helm-org-agenda-files-headings)
;;;;;;  "helm-org" "helm-org.el" (22331 28205))
;;; Generated autoloads from helm-org.el

(autoload 'helm-org-agenda-files-headings "helm-org" "\
Preconfigured helm for org files headings.

\(fn)" t nil)

(autoload 'helm-org-in-buffer-headings "helm-org" "\
Preconfigured helm for org buffer headings.

\(fn)" t nil)

(autoload 'helm-org-parent-headings "helm-org" "\
Preconfigured helm for org headings that are parents of the
current heading.

\(fn)" t nil)

(autoload 'helm-org-capture-templates "helm-org" "\
Preconfigured helm for org templates.

\(fn)" t nil)

(autoload 'helm-org-completing-read-tags "helm-org" "\
Not documented

\(fn PROMPT COLLECTION PRED REQ INITIAL HIST DEF INHERIT-INPUT-METHOD _NAME _BUFFER)" nil nil)

;;;***

;;;### (autoloads (helm-multi-occur-from-isearch helm-occur-from-isearch
;;;;;;  helm-occur helm-regexp helm-moccur-mode) "helm-regexp" "helm-regexp.el"
;;;;;;  (22331 28205))
;;; Generated autoloads from helm-regexp.el

(autoload 'helm-moccur-mode "helm-regexp" "\
Major mode to provide actions in helm moccur saved buffer.

Special commands:
\\{helm-moccur-mode-map}

\(fn)" t nil)

(autoload 'helm-regexp "helm-regexp" "\
Preconfigured helm to build regexps.
`query-replace-regexp' can be run from there against found regexp.

\(fn)" t nil)

(autoload 'helm-occur "helm-regexp" "\
Preconfigured helm for Occur.

\(fn)" t nil)

(autoload 'helm-occur-from-isearch "helm-regexp" "\
Invoke `helm-occur' from isearch.

\(fn)" t nil)

(autoload 'helm-multi-occur-from-isearch "helm-regexp" "\
Invoke `helm-multi-occur' from isearch.

With a prefix arg, reverse the behavior of
`helm-moccur-always-search-in-current'.
The prefix arg can be set before calling
`helm-multi-occur-from-isearch' or during the buffer selection.

\(fn &optional _ARG)" t nil)

;;;***

;;;### (autoloads (helm-execute-kmacro helm-show-kill-ring helm-register
;;;;;;  helm-all-mark-rings helm-global-mark-ring helm-mark-ring
;;;;;;  helm-push-mark-mode) "helm-ring" "helm-ring.el" (22331 28205))
;;; Generated autoloads from helm-ring.el

(defvar helm-push-mark-mode nil "\
Non-nil if Helm-Push-Mark mode is enabled.
See the command `helm-push-mark-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-push-mark-mode'.")

(custom-autoload 'helm-push-mark-mode "helm-ring" nil)

(autoload 'helm-push-mark-mode "helm-ring" "\
Provide an improved version of `push-mark'.
Modify the behavior of `push-mark' to update
the `global-mark-ring' after each new visit.

\(fn &optional ARG)" t nil)

(autoload 'helm-mark-ring "helm-ring" "\
Preconfigured `helm' for `helm-source-mark-ring'.

\(fn)" t nil)

(autoload 'helm-global-mark-ring "helm-ring" "\
Preconfigured `helm' for `helm-source-global-mark-ring'.

\(fn)" t nil)

(autoload 'helm-all-mark-rings "helm-ring" "\
Preconfigured `helm' for `helm-source-global-mark-ring' and `helm-source-mark-ring'.

\(fn)" t nil)

(autoload 'helm-register "helm-ring" "\
Preconfigured `helm' for Emacs registers.

\(fn)" t nil)

(autoload 'helm-show-kill-ring "helm-ring" "\
Preconfigured `helm' for `kill-ring'.
It is drop-in replacement of `yank-pop'.

First call open the kill-ring browser, next calls move to next line.

\(fn)" t nil)

(autoload 'helm-execute-kmacro "helm-ring" "\
Preconfigured helm for keyboard macros.
Define your macros with `f3' and `f4'.
See (info \"(emacs) Keyboard Macros\") for detailed infos.
This command is useful when used with persistent action.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-semantic-or-imenu helm-semantic) "helm-semantic"
;;;;;;  "helm-semantic.el" (22331 28205))
;;; Generated autoloads from helm-semantic.el

(autoload 'helm-semantic "helm-semantic" "\
Preconfigured `helm' for `semantic'.
If ARG is supplied, pre-select symbol at point instead of current

\(fn ARG)" t nil)

(autoload 'helm-semantic-or-imenu "helm-semantic" "\
Preconfigured helm for `semantic' or `imenu'.
If ARG is supplied, pre-select symbol at point instead of current
semantic tag in scope.

If `semantic-mode' is active in the current buffer, then use
semantic for generating tags, otherwise fall back to `imenu'.
Fill in the symbol at point by default.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (helm-xrandr-set helm-list-emacs-process helm-top)
;;;;;;  "helm-sys" "helm-sys.el" (22331 28205))
;;; Generated autoloads from helm-sys.el

(autoload 'helm-top "helm-sys" "\
Preconfigured `helm' for top command.

\(fn)" t nil)

(autoload 'helm-list-emacs-process "helm-sys" "\
Preconfigured `helm' for emacs process.

\(fn)" t nil)

(autoload 'helm-xrandr-set "helm-sys" "\
Preconfigured helm for xrandr.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-etags-select) "helm-tags" "helm-tags.el"
;;;;;;  (22331 28205))
;;; Generated autoloads from helm-tags.el

(autoload 'helm-etags-select "helm-tags" "\
Preconfigured helm for etags.
If called with a prefix argument REINIT
or if any of the tag files have been modified, reinitialize cache.

This function aggregates three sources of tag files:

  1) An automatically located file in the parent directories,
     by `helm-etags-get-tag-file'.
  2) `tags-file-name', which is commonly set by `find-tag' command.
  3) `tags-table-list' which is commonly set by `visit-tags-table' command.

\(fn REINIT)" t nil)

;;;***

;;;### (autoloads (helm-popup-tip-mode) "helm-utils" "helm-utils.el"
;;;;;;  (22331 28205))
;;; Generated autoloads from helm-utils.el

(defvar helm-popup-tip-mode nil "\
Non-nil if Helm-Popup-Tip mode is enabled.
See the command `helm-popup-tip-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-popup-tip-mode'.")

(custom-autoload 'helm-popup-tip-mode "helm-utils" nil)

(autoload 'helm-popup-tip-mode "helm-utils" "\
Show help-echo informations in a popup tip at end of line.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("helm-core-pkg.el" "helm-easymenu.el"
;;;;;;  "helm-lib.el" "helm-multi-match.el" "helm-pkg.el" "helm-plugin.el"
;;;;;;  "helm-source.el" "helm-types.el") (22331 33059 828587))

;;;***

(provide 'helm-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-autoloads.el ends here
