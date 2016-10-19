## helm-grepint.el
*Generic helm interface to grep*

---
[![License GPLv3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.html)
[![MELPA](https://melpa.org/packages/helm-grepint-badge.svg)](https://melpa.org/#/helm-grepint)

### Description

This package solves the following problems for me:
- A single function call interface to grep and therefore keybinding.
- Selects the grep based on context: Inside a git-repository, runs
  `git-grep`, otherwise runs `ag`.
- Uses helm to select candidates and jumps to the given line with RET.

And the following additional problems (as of version 1.2.0):
- A second interactive function `helm-grepint-grep-root`.  This runs the
  grepping inside a root directory.  By default this has been defined for
  the git-grep where it greps from the git root directory.
- Inside a huge git repository one can create a file defined in the
  variable `helm-grepint-default-config-ag-presearch-marker-file` and it
  will set that directory as the root directory for grepping.  It uses `ag`
  instead of `git-grep` as the grep.
- The grepping is case-insensitive by default, but if an upper-case letter
  is given case-sensitive grepping is done.

The following enables the aforementioned:

        (require 'helm-grepint)
        (helm-grepint-set-default-config-latest)
        (global-set-key (kbd "C-c g") #'helm-grepint-grep)
        (global-set-key (kbd "C-c G") #'helm-grepint-grep-root)

The original configuration (i.e. without the above additional features) is
available with the following:

        (helm-grepint-set-default-config)

### Key bindings within helm

- `RET`/`F1` selects an item and closes the helm session.
- `F2` displays the grep results in a `grep-mode` buffer.
- `Right-arrow` selects the item, but does not close the helm session.  This
  is similar as `helm-occur`.  Default helmkeybindings for this feature are
  also available (`C-j` and `C-z`).
- `M-c` cycles case sensitiveness.

### Customization

Look into the function `helm-grepint-set-default-config` to see how the default
cases are configured.  Also look into `helm-grepint-add-grep-config` for more
details on what is required for a new grep to be defined.

### Changes

Version 1.2.0

- Obsoleted `helm-grepint-get-grep-config` in favor of
  `helm-grepint-grep-config`.
- Make the ignore-case a separate argument in the grep configuration.  This
  way it can be toggled on and off easily.
- Add case-fold-search support (case-(in)sensitiveness).  Add Helm
  keybinding `M-c` to control it.
- Add smart case-sensitiveness checking.
- Add a new configuration `helm-grepint-set-default-config-v1.2.0` which
  makes the smart cases-sensitiveness as the default.  The configuration is
  now the `helm-grepint-set-default-config-latest`.

Version 1.1.1

- Add `--ignore-case` argument for `git-grep` to make it consistent with
  `ag` in the `helm-grepint-set-default-config`.

Version 1.1.0

- Fix incompatibilities with recent helm versions.
- Add `helm-grepint-candidate-number-limit` variable to control the number
  of candidates instead of hard-coding 500.
- Create a new example configuration which adds the ag-presearch
  functionality.  The example configurations are now versioned:
  `helm-grepint-set-default-config-v1.0.0` and
  `helm-grepint-set-default-config-v1.1.0`.
- Change the `helm-grepint-set-default-config` function to an alias of
  `helm-grepint-set-default-config-v1.0.0`.  Add new alias
  `helm-grepint-set-default-config-latest` which points to
  `helm-grepint-set-default-config-v1.1.0`.

Version 1.0.0

- Add action to create a `grep-mode` buffer from the helm-buffer.
- Add universal-argument to manually ask the used grep configuration.

Version 0.5.5

- Fix swooping into multiple files within a helm session.  Previously it
  would change default-directory every swoop.
- Add action to open the helm buffer in grep-mode.  This enables the use of
  e.g. `wgrep`.
- Add `helm-grepint-grep-ask-root` and set it as default for ag.

### Function Documentation


#### `(helm-grepint-add-grep-config NAME &rest CONFIGURATION)`

Add configuration NAME with properties from CONFIGURATION.

The configuration can have the following items:

:command
 - A command string to run.

:arguments
 - Arguments provided for the command when it is run.  This
   and :command is provided for the ‘helm-grepint-run-command’ function.

:enable-function
 - A function that returns non-nil if this grep can be used.  If
   this is nil, the grep can be used always.

:root-directory-function
 - Function that returns a string of a directory that is regarded
   as the root directory when running ‘helm-grepint-grep-root’.  If
   this is nil, ‘helm-grepint-grep-root’ behaves exactly as ‘helm-grepint-grep’.

:ignore-case-argument
 - The argument for the grep command that makes grepping ignore
   character case.  Traditionally this is ‘--ignore-case’ for a
   number of different greps.  This needs to be defined or the
   ‘helm-grepint-cycle-character-case’ function has no effect.

#### `(helm-grepint-grep-config NAME &optional NEW-CONFIG)`

Get a grep configuration with NAME or set it to NEW-CONFIG.

#### `(helm-grepint-grep-config-property NAME PROPERTY &rest NEW-VALUE)`

Get a config NAME’s PROPERTY or set it to NEW-VALUE.
The config NAME has been added with ‘helm-grepint-add-grep-config’.
Returns the current value of the property or nil if either name
or property was not found.

#### `(helm-grepint-run-command &rest PLIST)`

Run a grep command from PLIST.

The command line is constructed with the following PLIST items:

:command :arguments :extra-arguments.

The :arguments is split on whitespace, but :extra-arguments are
used as is.

#### `(helm-grepint-select-grep ASK-GREP)`

Select the grep based on :enable-function from ‘helm-grepint-grep-configs’.

If ASK-GREP is non-nil, select the grep by asking with
‘completing-read’.  The greps are compared in order of
‘helm-grepint-grep-list’.  If the grep does not
have :enable-function property, select it automatically.

#### `(helm-grepint-grep-default-root)`

Get the default root directory if :root-directory-function isn’t defined.

#### `(helm-grepint-grep-ask-root)`

Ask the root directory from user.

#### `(helm-grepint-grep-parse-line LINE)`

Parse a LINE of output from grep-compatible programs.

Returns a list of (file line contents) or nil if the line could not be parsed.

#### `(helm-grepint-grep-action-jump CANDIDATE)`

Jump to line in a file described by a grep -line CANDIDATE.

#### `(helm-grepint-grep-action-mode CANDIDATE)`

Open a copy of the helm buffer in ‘grep-mode’.

CANDIDATE is ignored.

#### `(helm-grepint-grep-process)`

This is the candidates-process for ‘helm-grepint-helm-source’.

#### `(helm-grepint-grep-filter-one-by-one CANDIDATE)`

Propertize each CANDIDATE provided by ‘helm-grepint-helm-source’.

Uses ‘helm-grep-highlight-match’ from helm-grep to provide line highlight.

#### `(helm-grepint-cycle-character-case)`

Select the next one from the ‘helm-grepint-character-cases’ list.

#### `(helm-grepint-grep &optional ARG)`

Run grep in the current directory.

See the usage for ARG in ‘helm-grepint--grep’.

The grep function is determined by the contents of
‘helm-grepint-grep-configs’ and the order of ‘helm-grepint-grep-list’.

#### `(helm-grepint-grep-root &optional ARG)`

Function ‘helm-grepint-grep’ is run in a root directory.

See the usage for ARG in ‘helm-grepint--grep’.

#### `(helm-grepint-set-default-config-v1\.0\.0)`

Set the default grep configuration into ‘helm-grepint-grep-configs’ and ‘helm-grepint-grep-list’.

#### `(helm-grepint-set-default-config-v1\.1\.0)`

Set default grep configuration.

Run ‘helm-grepint-set-default-config-v1.0.0’ and then this function.

Adds configuration for running ag if file set in
‘helm-grepint-default-config-ag-presearch-marker-file’ is found
in a git repository before the git root.  The use case is running
this in huge git repositories and wanting to limit the searching
to a subdirectory.

#### `(helm-grepint-set-default-config-v1\.2\.0)`

Set default grep configuration.

Run ‘helm-grepint-set-default-config-v1.1.0’ and then this function.

Makes the ‘smart’ character-case as the default.  Changes the
order of cycling the character-cases.  After the ‘smart’ comes
case-sensitive.

-----
<div style="padding-top:15px;color: #d0d0d0;">
Markdown README file generated by
<a href="https://github.com/mgalgs/make-readme-markdown">make-readme-markdown.el</a>
</div>
