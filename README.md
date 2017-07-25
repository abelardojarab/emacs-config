# Emacs configuration files from Abelardo Jara-Berrocal #

## Supported emacs versions: ##

-   Emacs 24.4-24.x
-   Emacs 25.x
-   Emacs 26 (snapshots)

## Packages included ##

-   **Package auto-loading and deferral:** use-package
-   **Development:** company, flycheck, yasnippet, auctex, cmake-ide
-   **Network utilities:** wunderlust, sx, mu4e, elfeed

## Fixes with respect to Spacemacs and Prelude ##

-   **Fixed htmlize** library so it does not fail to export code or text
    segments as formatted HTML
-   **Fixed and enabled ECB** - fix ECB to work with Emacs 25 and above
-   **Fixed and enabled ergoemacs-mode**, such that it does not
    invoke (package-initialize) in case there is no Internet connection
-   **Fixed usage orgtbl-mode** under markdown-mode so Org tables can be used in
    markdown documents.
-   *adviced* (load-theme ') function such that fringe,
    modeline (powerline) and tabbar colorsets are refreshed after a theme change
    (improvement over prelude and spacemacs)

## New features ##

-   **Adaptive loading of company-jedi** (by checking that
    'jedi' module is an installed Python module or not)
-   **Adaptive usage of company-tern** (by checking that
    'tern' module is an installed node.js module or not)
-   Added helm-fonts, to enable fontset selection through helm.
-   Included org-insert-screenshot (Windows, Mac OS X and Linux versions are included)

## Supported platforms ##

-   **Tested** to properly work on Windows (cygwin is required), Mac OS
    X and Linux.

# Keybindings: #

![Enhanced ergoemacs keybindings](https://raw.githubusercontent.com/abelardojarab/emacs-config/master/.emacs.d/ergoemacs-extras/saved/standard.png)

# Screenshot #

![ECB-based emacs layout](https://raw.githubusercontent.com/abelardojarab/emacs-config/master/README.png)
