<<<<<<< HEAD
emacsfull
=========

Emacs configuration files including compiled binaries.

Compilation
===========

emacs -batch -L ~/.emacs.d/epc -L ~/.emacs.d/deferred -L ~/.emacs.d/mocker -L ~/.emacs.d/ctable -L ~/.emacs.d/auto-complete -L ~/.emacs.d/popup-el -L ~/.emacs.d/jedi -L ~/.emacs.d/js2-mode -L ~/.emacs.d/python-environment -L ~/.emacs.d/yasnippet ~/.emacs.d/tabbar -L ~/.emacs.d/magit -L ~/.emacs.d/icicles -L ~/.emacs.d/cedet -L ~/.emacs.d/elisp -L ~/.emacs.d/git-modes -L ~/.emacs.d/git-emacs -L ~/.emacs.d/s ~/.emacs.d/dash -L ~/.emacs.d/diff-hl -L ~/.emacs.d/ecb -L ~/.emacs.d/projectile -L ~/.emacs.d/use-package -L ~/.emacs.d/helm -L ~/.emacs.d/popwin -L ~/.emacs.d/popup -L ~/.emacs.d/flycheck -L ~/.emacs.d/flycheck-tip -L ~/.emacs.d/perspective -L ~/.emacs.d/epl -L ~/.emacs.d/pkg-info -L ~/.emacs.d/tern/emacs -L ~/.emacs.d/multiple-cursors -L ~/.emacs.d/git-messenger -L ~/.emacs.d/noflet -L ~/.emacs.d/fakir -L ~/.emacs.d/web -L ~/.emacs.d/elnode -L ~/.emacs.d/w3m -L ~/.emacs.d/db -L ~/.emacs.d/kv -L ~/.emacs.d/ergoemacs-mode -L ~/.emacs.d/whitespace-cleanup-mode -L ~/.emacs.d/skewer-mode -L ~/.emacs.d/emacs-web-server -L ~/.emacs.d/json-mode -L ~/.emacs.d/json-snatcher -L ~/.emacs.d/json-reformat -L ~/.emacs.d/js2-refactor -L ~/.emacs.d/highlight-symbol -L ~/.emacs.d/diff-hl -L ~/.emacs.d/guide-key -L ~/.emacs.d/git-gutter-plus -L ~/.emacs.d/git-gutter-fringe-plus -L ~/.emacs.d/tern/emacs -L ~/.emacs.d/benchmark-init -L ~/.emacs.d/flx -L ~/.emacs.d/f -L ~/.emacs.d/autopair -L ~/.emacs.d/pos-tip -L ~/.emacs.d/exec-path-from-shell -L ~/.emacs.d/tabbar-ruler -L ~/.emacs.d/markdown-mode -L ~/.emacs.d/ack-and-a-half -L ~/.emacs.d/pretty-symbols -L ~/.emacs.d/pretty-mode -L ~/.emacs.d/rich-minority -L ~/.emacs.d/yaml-mode -L ~/.emacs.d/etags-select -L ~/.emacs.d/org-mode/lisp -L ~/.emacs.d/auto-indent-mode -L ~/.emacs.d/rainbow-delimiters -L ~/.emacs.d/smooth-scrolling -L ~/.emacs.d/python-mode -L ~/.emacs.d/smart-mode-line -L ~/.emacs.d/smex -L ~/.emacs.d/git-timemachine -L ~/.emacs.d/highlight-symbol -L ~/.emacs.d/nyan-mode -L ~/.emacs.d/indent-guide -L ~/.emacs.d/guide-key-tip -L ~/.emacs.d/auto-complete-etags -L ~/.emacs.d/auto-complete-clang -L ~/.emacs.d/Highlight-Indentation-for-Emacs -L ~/.emacs.d/bm -L ~/.emacs.d/fringe-helper -L . -f batch-byte-compile ~/.emacs.d/**/*.el
=======
# yafolding - Yet another folding extension for Emacs

Folding code blocks based on indentation.

## ScreenShot
![PrtSc](https://raw.github.com/zenozeng/yafolding.el/master/psc.png)


## Config Example

### Hook into prog-mode-hook

```emacs-lisp
(add-hook 'prog-mode-hook
          (lambda () (yafolding-mode)))
```

### Modify keybindings

```
(require 'yafolding)
(define-key yafolding-mode-map (kbd "<C-S-return>") nil)
(define-key yafolding-mode-map (kbd "<C-return>") nil)
(define-key yafolding-mode-map (kbd "C-c <C-S-return>") 'yafolding-toggle-all)
(define-key yafolding-mode-map (kbd "C-c <C-return>") 'yafolding-toggle-element)
```


## Licensing

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

>>>>>>> b87219fe4dc7e601e112873d5f91a0843fde7d27

