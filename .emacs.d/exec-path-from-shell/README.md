[![Melpa Status](http://melpa.org/packages/exec-path-from-shell-badge.svg)](http://melpa.milkbox.net/#/exec-path-from-shell)
[![Melpa Stable Status](http://stable.melpa.org/packages/exec-path-from-shell-badge.svg)](http://stable.melpa.org/#/exec-path-from-shell)

exec-path-from-shell
=====================

A GNU Emacs library to ensure environment variables inside Emacs look
the same as in the user's shell.

Motivation
----------

Ever find that a command works in your shell, but not in Emacs?

This happens a lot on OS X, where an Emacs instance started from the GUI inherits a
default set of environment variables.

This library works solves this problem by copying important environment
variables from the user's shell: it works by asking your shell to print out the
variables of interest, then copying them into the Emacs environment.

Compatibility
-------------

If you use a non-POSIX-standard shell such as `tcsh` or `fish`, your
shell will be asked to execute `sh` as a subshell in order to print
out the variables in a format which can be reliably parsed. `sh` must
be a POSIX-compliant shell in this case.

Note that shell variables which have not been exported as environment
variables (e.g. using the "export" keyword) may not be visible to
`exec-path-from-shell'.

Installation
------------

ELPA packages are available on Marmalade and MELPA.  Alternatively, [download][]
the latest release or clone the repository, and install
`exec-path-from-shell.el` with `M-x package-install-from-file`.

Usage
-----

Add the following to your `init.el` (after calling `package-initialize`):

```el
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
```

This sets `$MANPATH`, `$PATH` and `exec-path` from your shell, but only on OS X.

You can copy values of other environment variables by customizing
`exec-path-from-shell-variables` before invoking
`exec-path-from-shell-initialize`, or by calling
`exec-path-from-shell-copy-env`, e.g.:

```el
(exec-path-from-shell-copy-env "PYTHONPATH")
```

This function may also be called interactively.

Further help
------------

* `C-h f exec-path-from-shell-initialize`
* `C-h f exec-path-from-shell-copy-env`

License
-------

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
Street, Fifth Floor, Boston, MA 02110-1301, USA.

[download]: https://github.com/purcell/exec-path-from-shell/tags
