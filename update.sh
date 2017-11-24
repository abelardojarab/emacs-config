#!/bin/bash

# Forked packages

git subtree pull --prefix=.emacs.d/yasnippet  --squash https://github.com/abelardojarab/yasnippet.git master
git subtree pull --prefix=.emacs.d/ergoemacs-mode  --squash https://github.com/abelardojarab/ergoemacs-mode.git master
git subtree pull --prefix=.emacs.d/rtags  --squash https://github.com/abelardojarab/rtags.git master
git subtree pull --prefix .emacs.d/rtags/src/rct --squash https://github.com/Andersbakken/rct master
git subtree pull --prefix .emacs.d/rtags/src/selene --squash https://github.com/jeremyong/selene master
git subtree pull --prefix .emacs.d/rtags/src/lua --squash https://github.com/LuaDist/lua master
