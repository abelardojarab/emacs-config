# helm-bm.el

[helm] sources for [bm.el][bm].

## Requirements

- [helm]
- [bm]
- [cl-lib]
- [s]

## Installation

If you're an Emacs 24 user or you have a recent version of package.el
you can install `helm-bm.el` from the [MELPA](http://melpa.milkbox.net/) repository.

## Configuration

Add the following to your emacs init file.

    (require 'helm-bm) ;; Not necessary if using ELPA package
    (global-set-key (kbd "C-c b") 'helm-bm)


## Basic usage

#### <kbd>M-x</kbd> `helm-bm`

Show bookmarks of [bm].el with `helm`.


[helm]:https://github.com/emacs-helm/helm
[bm]:https://github.com/joodland/bm
[cl-lib]:http://elpa.gnu.org/packages/cl-lib.html
[s]:https://github.com/magnars/s.el
