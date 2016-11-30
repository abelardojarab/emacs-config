Danneskjold Colorscheme for Emacs
===============================

Beautiful high-contrast emacs theme.

Screenshots
-----------

#### Dired and org-mode ####

![dired and org-mode](https://github.com/rails-to-cosmos/danneskjold-theme/raw/master/screenshots/dired-and-org.png)

#### Magit ####
![magit](https://github.com/rails-to-cosmos/danneskjold-theme/raw/master/screenshots/magit.png)

### Bug Reporting

Here are some things to keep in mind when submitting a bug report:

*   include the output of `M-x version` in your report,
*   mention whether you’re using color-theme or the Emacs 24 theme,
*   include the names of Emacs faces that you have a problem with (`M-: (face-at-point)` and `M-x describe-face` will tell you the name of the face at point),
*   include the output of `M-: (display-color-cells)` (that lets us know which set of colors your Emacs is using),
*   screenshots are very helpful (before and after if you made a change),
*   if you’re using a terminal, the name of the terminal and (if you can find out) the number of colors the terminal app is using,
*   also if you’re using a terminal, try running Emacs in GUI mode, and see if the problem exists there (if it does, report the bug that way, if not, just mention that it’s a terminal-only bug),
*  `M-x customize-apropos-faces` can help you find all the relevant faces if you are submitting faces for a mode.