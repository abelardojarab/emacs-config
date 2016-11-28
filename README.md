---
author: 'Abelardo Jara-Berrocal'
title: 'Emacs configuration files from Abelardo Jara-Berrocal'
...

Target Emacs' versions:
=======================

-   Emacs 24.4-24.x
-   Emacs 25.-
-   Emacs 26 (snapshots)

Packages included (differences and improvements over prelude and spacemacs):
============================================================================

-   use-package
-   company
-   flycheck
-   yasnippet
-   **Fixed ECB such it works with Emacs 25 and above** (fixed issues so
    ecb works with Emacs 25 and above)
-   **Adaptive usage of company-jedi** (wrote function to check that
    'jedi' module is an installed Python module or not)
-   **Adaptive usage of company-tern** (wrote function to check that
    'tern' module is an installed node.js module or not)
-   **Fixed ergoemacs-mode**, such that it does not
    invoke (package-initialize) in case there is no Internet connection,
    in addition to not invoke add-hooks to (execute-command)
    and (execute-command-extended)
-   Wrote function helm-fonts, to enable fontset selection from a helm
    source
-   Multiple themes (zenburn, monokai, ample, zerodark, etc); available
    through easy selection by helm-themes
-   adviced the (load-theme ') function such that fringe,
    modeline (powerline) and tabbar colorset is also refreshed
    (improvement over prelude and spacemacs)
-   **Tested** to properly work on Windows (cygwin is required), Mac OS
    X and -of course- Linux.
-   **Included ox-pandoc** to export Org documents to markdow
-   **Fixed htmlize** library so it is possible to export code or text
    segments as formatted HTML
-   Included org-insert-screenshot (Windows, Mac OS X and -of course,
    again- Linux are included)

Screenshot
==========

![](file:README.png)
