orgtbl-ascii-plot
=================

News
----

- This package will be integrated in the next release of Org mode.
  It will no longer need to be a Melpa package.
- Key-binding will change from current `C-c p` to `C-c " a`.
  This is reflected in this package.
- A new key-binding will be available:
  `C-c " g` will launch GnuPlot on a table.
  The key-binding is made available by this package.

Bar plot
--------

Draws bar plots in Emacs OrgMode tables.

- Put the cursor in a column containing numerical values
  of an Org-Mode table,
- type `C-c " a`,
  or use the menu entry `Tbl > Column > Ascii plot`
  a new column is added with a bar plot.
- When the table is refreshed
  (`C-u C-c *` or menu `Tbl > Calculate > Recalculate all`),
  the plot is updated to reflect the new values.

Detailed documentation is here:
http://orgmode.org/worg/org-contrib/orgtbl-ascii-plot.html

Example
-------

    | ! |  x |    sin(x/4) |              |
    |---+----+-------------+--------------|
    | # |  0 |           0 | WWWWWW       |
    | # |  1 |  0.32719470 | WWWWWWWW     |
    | # |  2 |  0.61836980 | WWWWWWWWWh   |
    | # |  3 |  0.84147098 | WWWWWWWWWWW  |
    | # |  4 |  0.97193790 | WWWWWWWWWWWV |
    | # |  5 |  0.99540796 | WWWWWWWWWWWW |
    | # |  6 |  0.90929743 | WWWWWWWWWWWu |
    | # |  7 |  0.72308588 | WWWWWWWWWW-  |
    | # |  8 |  0.45727263 | WWWWWWWWh    |
    | # |  9 |  0.14112001 | WWWWWWV      |
    | # | 10 | -0.19056796 | WWWWH        |
    | # | 11 | -0.50127705 | WWW          |
    | # | 12 | -0.75680250 | Wu           |
    | # | 13 | -0.92901450 | ;            |
    | # | 14 | -0.99895492 |              |
    | # | 15 | -0.95892427 | :            |
    | # | 16 | -0.81332939 | W.           |
    | # | 17 | -0.57819824 | WWu          |
    | # | 18 | -0.27941550 | WWWW-        |
    | # | 19 | 0.050127010 | WWWWWW-      |
    | # | 20 |  0.37415123 | WWWWWWWW:    |
    | # | 21 |  0.65698660 | WWWWWWWWWH   |
    | # | 22 |  0.86749687 | WWWWWWWWWWW: |
    | # | 23 |  0.98250779 | WWWWWWWWWWWH |
    | # | 24 |  0.98935825 | WWWWWWWWWWWH |
    | # | 25 |  0.88729411 | WWWWWWWWWWW- |
    | # | 26 |  0.68755122 | WWWWWWWWWW.  |
    | # | 27 |  0.41211849 | WWWWWWWWu    |
    | # | 28 | 0.091317236 | WWWWWWu      |
    | # | 29 | -0.23953677 | WWWWl        |
    | # | 30 | -0.54402111 | WWh          |
    | # | 31 | -0.78861628 | W-           |
    #+TBLFM: $3=sin($x/3);R::$4='(orgtbl-ascii-draw $3 -1 1)

Author
------

Thierry Banel, tbanelwebmin at free dot fr

Change Log
----------

- Make `C-c p` key-binding autoloaded
- Added menu entry `Tbl > Column > Ascii plot`
- Change key-binding from `C-c p` to `C-c " a`
