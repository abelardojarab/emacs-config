Overview
========
latex-pretty-symbols.el makes emacs display unicode characters instead of latex
commands for a lot of the most usual symbols.

It basically contains a list of many latex commands, and their unicode
counterparts, and makes emacs display the unicode symbol instead of the latex
code. So \vDash, \Sigma, \leq etc displays nicely. It also converts some
sub/super-scripts into equivalent unicode symbols, so e.g $a_1$ becomes $a₁$. It
is highly reminiscent, and based on, some of the pretty-lambda code that is
floating around.

image: http://imageshack.us/photo/my-images/3/exampleyp.png/

Installation
============
Put latex-pretty-symbols.el  in your load-path, and put 
(require 'latex-pretty-symbols) in your init-file.
