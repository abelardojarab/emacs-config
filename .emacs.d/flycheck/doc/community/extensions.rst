.. _flycheck-extensions:

========================
 Recommended extensions
========================

The Emacs community has produced a number of extensions to Flycheck.  This page
lists all that we know of and can safely recommend to our users.

*Official* extensions are (co-)maintained by the :ref:`Flycheck maintainers
<flycheck-maintainers>` who will take care to update official extensions in case
of breaking changes in Flycheck and work to provide extra API for extensions if
needed.  If you'd like to make your extension an *official* one and move it into
the `Flycheck Github organisation`_ please contact a :ref:`maintainer
<flycheck-maintainers>`.

If you do know extensions not in this list, or would like to see your own
extension here, please feel free to `add it`_.

We would like to thank all people who created and contributed to Flycheck
extensions for their awesome work.  Without your help and support Flycheck would
not be what it is today.

.. _add it: https://github.com/flycheck/flycheck/edit/master/doc/community/extensions.rst
.. _Flycheck Github organisation: https://github.com/flycheck

User interface
==============

These extensions change Flycheck’s user interface:

* :flyc:`flycheck-color-mode-line` (*official*) colors the mode line according
  to the Flycheck status.
* :flyc:`flycheck-pos-tip` (*official*) shows Flycheck error messages in a
  graphical popup.
* :gh:`liblit/flycheck-status-emoji` adds cute emoji (e.g. 😱 for errors) to
  Flycheck’s mode line status.

Language support
================

These extensions add support for new languages, or improve support for built-in
languages.  They are grouped by the corresponding language so you can jump
directly to the languages that interest you:

.. contents:: Languages
   :local:

Cadence
-------

* :gh:`cmarqu/flycheck-hdl-irun` adds a syntax checker for hardware description
  languages supported by `Cadence IES/irun`_.

.. _Cadence IES/irun: http://www.cadence.com/products/fv/enterprise_simulator/pages/default.aspx

Clojure
-------

* :gh:`clojure-emacs/squiggly-clojure` adds syntax checking for Clojure.

C/C++/Objective C
-----------------

* :gh:`Wilfred/flycheck-pkg-config` configures Flycheck to use settings from
  `pkg-config`_ when checking C/C++.
* :flyc:`flycheck-google-cpplint` (*official*) adds a syntax checker for
  Google's C++ style checker.
* :gh:`Sarcasm/flycheck-irony` adds a Flycheck syntax checker for C, C++ and
  Objective C using :gh:`Irony Mode <Sarcasm/irony-mode>`.

.. _pkg-config: https://www.freedesktop.org/wiki/Software/pkg-config/

D
-

* :flyc:`flycheck-d-unittest` (*official*) adds a Flycheck checker to run unit
  tests for D programs on the fly.

Elixir
------

* :gh:`tomekowal/flycheck-mix` adds an Elixir syntax checker using the ``mix``
  build tool.

Emacs Lisp
----------

* :flyc:`flycheck-cask` (*official*) makes Flycheck use Cask packages for Emacs
  Lisp syntax checking in Cask_ projects.
* :gh:`purcell/flycheck-package` checks Emacs Lisp packages for common problems
  with package metadata.

.. _Cask: https://github.com/cask/cask

Haskell
-------

* :flyc:`flycheck-haskell` (*official*) configures Flycheck from the Cabal
  settings and sandbox in Haskell projects.

Ledger
------

* :gh:`purcell/flycheck-ledger` adds a syntax checker for the Ledger_ accounting
  tool.

.. _Ledger: http://ledger-cli.org/

Mercury
-------

* :flyc:`flycheck-mercury` (*official*) adds a syntax checker for the Mercury_
  language.

.. _Mercury: http://mercurylang.org/

OCaml
-----

* :flyc:`flycheck-ocaml` (*official*) adds a syntax checker for OCaml using the
  :gh:`Merlin <the-lambda-church/merlin>` backend.

Python
------

* :gh:`Wilfred/flycheck-pyflakes` adds a Python syntax checker using Pyflakes.

.. _Pyflakes: https://github.com/pyflakes/pyflakes

Rust
----

* :flyc:`flycheck-rust` (*official*) configures Flycheck according to the Cargo
  settings and layouts of the current Rust project.

Shell scripts
-------------

* :gh:`Gnouc/flycheck-checkbashisms` adds a shell script syntax checker using
  ``checkbashisms`` which is part of `Debian devscripts`_ and checks for common
  Bash constructs in POSIX shell scripts.

.. _Debian devscripts: https://anonscm.debian.org/cgit/collab-maint/devscripts.git
