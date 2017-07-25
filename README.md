# Emacs configuration files from Abelardo Jara-Berrocal #

## Supported Emacs versions: ##

-   Emacs 24.4-24.x
-   Emacs 25.x
-   Emacs 26 (snapshots)

## Packages included ##

-   **Package auto-loading and deferral:** use-package
-   **Development:** company, flycheck, yasnippet, auctex, cmake-ide
-   **Network utilities:** wunderlust, sx, mu4e, elfeed

## Fixes with respect to Spacemacs and Prelude ##

-   *Fixed htmlize* library so it is possible to export code or text
    segments as formatted HTML
-   *Fixed and enabled ECB* - works with Emacs 25 and above
-   *Fixed and enabled ergoemacs-mode*, such that it does not
    invoke (package-initialize) in case there is no Internet connection
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

![](https://raw.githubusercontent.com/abelardojarab/emacs-config/master/.emacs.d/ergoemacs-extras/bindings/lvl1.png)

# Screenshot #

![](https://raw.githubusercontent.com/abelardojarab/emacs-config/master/README.png)
