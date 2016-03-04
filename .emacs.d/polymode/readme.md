[![unstable](http://badges.github.io/stability-badges/dist/unstable.svg)](http://github.com/badges/stability-badges)

## Overview

Polymode is an emacs package that offers generic support for multiple major
modes inside a single emacs buffer. It is lightweight, object oriented and
highly extensible. Creating new polymodes typically takes a
[few](modes#multiple-automatically-detected-innermodes) lines of code.

Polymode also provides extensible facilities for external literate programming
tools for exporting, weaving and tangling.

- [Installation](#installation)
- [Polymodes Activation](#activation-of-polymodes)
- [Basic Usage](#basic-usage)
- [Warnings](#warnings)
- [Development](modes)
- [Screenshots](#screenshots)

## Installation

*Note: Oldest supported Emacs version is 24.4*

### From [MELPA](https://github.com/milkypostman/melpa)

<kbd>M-x</kbd> `package-install` `polymode`.

### Manually

```sh
git clone https://github.com/vitoshka/polymode.git
```

Add "polymode" directory and "polymode/modes" to your emacs path:

```lisp
(setq load-path
      (append '("path/to/polymode/"  "path/to/polymode/modes")
              load-path))
```

Require any polymode bundles that you are interested in. For example:

```lisp
(require 'poly-R)
(require 'poly-markdown)
```

## Activation of Polymodes

Polymodes are functions, just like ordinary emacs modes. The can be used in
place of emacs major or minor modes alike. There are two main ways to
automatically activate emacs (poly)modes:

 1. _By registering a file extension by adding modes to `auto-mode-alist`_:

    ```lisp
    ;;; MARKDOWN
    (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

    ;;; R modes
    (add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
    (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
    (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
    ```
    See [polymode-configuration.el](polymode-configuration.el) for more
    examples.

 2. _By setting local mode variable in you file_:

   ```c++
   // -*- mode: poly-C++R -*-
   ```
    or

   ```sh
   ## -*- mode: poly-brew+R; -*-
   ```

## Basic Usage

All polymode keys start with the prefix defined by `polymode-prefix-key`,
default is <kbd>M-n</kbd>. The `polymode-mode-map` is the parent of all
polymodes' maps:

* BACKENDS

     <kbd>e</kbd> `polymode-export`

     <kbd>E</kbd> `polymode-set-exporter`

     <kbd>w</kbd> `polymode-weave`

     <kbd>W</kbd> `polymode-set-weaver`

     <kbd>t</kbd> `polymode-tangle` ;; not implemented yet

     <kbd>T</kbd> `polymode-set-tangler` ;; not implemented yet

     <kbd>$</kbd> `polymode-show-process-buffer`

* NAVIGATION

    <kbd>C-n</kbd> `polymode-next-chunk`

    <kbd>C-p</kbd> `polymode-previous-chunk`

    <kbd>C-M-n</kbd> `polymode-next-chunk-same-type`

    <kbd>C-M-p</kbd> `polymode-previous-chunk-same-type`

* MANIPULATION

    <kbd>M-k</kbd> `polymode-kill-chunk`

    <kbd>M-m</kbd> `polymode-mark-or-extend-chunk`

    <kbd>C-t</kbd> `polymode-toggle-chunk-narrowing`

    <kbd>M-i</kbd> `polymode-insert-new-chunk`


## Warnings

  * Tested with Emacs 24.3.1 and 24.4.5.

Some things still don't work as expected. For example:

   * To kill a polymode buffer you will have position the cursor in the host
     mode buffer.
   * Customization interface is not working as expected (an eieio bug) and is
     not even tested.
   * Indentation and font-lock is not always right and requires some more
     tweaking. This is especially true for complex modes like `c-mode`.

## Developing with Polymode

For the relevant terminology and development info see these [docs](modes).

## Screenshots

### slim

<img src="img/slim.png" width="400px"/>

### markdown+R

<img src="img/Rmd.png" width="400px"/>

### markdown+R+YAML

<img src="img/rapport.png" width="400px"/>

### org mode

<img src="img/org.png" width="400px"/>

### Ess-help buffer

<img src="img/ess-help.png" width="400px"/>

### C++R
<img src="img/cppR.png" width="400px"/>

