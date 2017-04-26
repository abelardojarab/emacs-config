Emacs configuration files from Abelardo Jara-Berrocal
=====================================================

Target Emacs' versions:
-----------------------

-   Emacs 24.4-24.x
-   Emacs 25.-
-   Emacs 26 (snapshots)

Packages included (differences and improvements over prelude and spacemacs):
----------------------------------------------------------------------------

-   use-package, company, flycheck, yasnippet
-   wunderlust, sx, mu4e and elfeed

-   *Fixed htmlize* library so it is possible to export code or text
    segments as formatted HTML
-   *Fixed ECB* - works with Emacs 25 and above
-   *Fixed ergoemacs-mode*, such that it does not
    invoke (package-initialize) in case there is no Internet connection

-   **Adaptive loading of company-jedi** (check that
    'jedi' module is an installed Python module or not)
-   **Adaptive usage of company-tern** (check that
    'tern' module is an installed node.js module or not)

-   Added helm-fonts, to enable fontset selection from a helm
    source
-   adviced (load-theme ') function such that fringe,
    modeline (powerline) and tabbar colorsets are refreshed after a theme change
    (improvement over prelude and spacemacs)
-   Included org-insert-screenshot (Windows, Mac OS X and -of course,
    again- Linux are included)

-   **Tested** to properly work on Windows (cygwin is required), Mac OS
    X and Linux.

Screenshot
----------
![](https://raw.githubusercontent.com/abelardojarab/emacs-config/master/README.png)
