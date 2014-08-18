emacsfull
=========

Emacs configuration files including compiled binaries.

Compilation
===========

emacs -batch -L ~/.emacs.d/epc -L ~/.emacs.d/deferred -L ~/.emacs.d/mocker -L ~/.emacs.d/ctable -L ~/.emacs.d/auto-complete -L ~/.emacs.d/popup-el -L ~/.emacs.d/jedi -L ~/.emacs.d/js2-mode -L ~/.emacs.d/python-environment -L ~/.emacs.d/yasnippet ~/.emacs.d/tabbar -L ~/.emacs.d/magit -L ~/.emacs.d/icicles -L ~/.emacs.d/cedet-bzr -L ~/.emacs.d/elisp -L ~/.emacs.d/git-modes -L ~/.emacs.d/git-emacs -L ~/.emacs.d/s ~/.emacs.d/dash -L ~/.emacs.d/diff-hl -L ~/.emacs.d/ecb -L ~/.emacs.d/projectile -L ~/.emacs.d/use-package -L ~/.emacs.d/helm -L ~/.emacs.d/popwin -L ~/.emacs.d/popup -L ~/.emacs.d/flycheck -L ~/.emacs.d/flycheck-tip -L ~/.emacs.d/perspective -L ~/.emacs.d/epl -L ~/.emacs.d/pkg-info -L . -f batch-byte-compile ~/.emacs.d/**/*.el
