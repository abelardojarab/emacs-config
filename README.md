
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
-   wunderlust, sx, mu4e and elfeed
-   **Fixed htmlize** library so it is possible to export code or text
    segments as formatted HTML
-   **Fixed ECB ** - such it works with Emacs 25 and above
-   **Adaptive loading of company-jedi** (check that
    'jedi' module is an installed Python module or not)
-   **Adaptive usage of company-tern** (check that
    'tern' module is an installed node.js module or not)
-   **Fixed ergoemacs-mode**, such that it does not
    invoke (package-initialize) in case there is no Internet connection
-   Added helm-fonts, to enable fontset selection from a helm
    source
-   Multiple themes (zenburn, monokai, ample, zerodark, etc); available
    for selection by helm-themes
-   adviced (load-theme ') function such that fringe,
    modeline (powerline) and tabbar colorsets are also refreshed
    (improvement over prelude and spacemacs)
-   Included org-insert-screenshot (Windows, Mac OS X and -of course,
    again- Linux are included)
-   **Tested** to properly work on Windows (cygwin is required), Mac OS
    X and Linux.

Screenshot
==========

![](file:https://github.com/abelardojarab/emacs-config/edit/master/README.png)
