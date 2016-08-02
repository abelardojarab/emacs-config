export CLICOLOR=1
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
export TERM=linux
export TERM=xterm-256color

if [ -f /opt/local/etc/profile.d/bash_completion.sh ]; then
      . /opt/local/etc/profile.d/bash_completion.sh
fi

# Your previous /Users/abelardojara/.profile file was backed up as /Users/abelardojara/.profile.macports-saved_2014-10-22_at_06:18:06

# Macports
export PATH="/opt/local/bin:/opt/local/sbin:${PATH}"

# Make MacTeX override MacPorts
export PATH=/usr/texbin:/usr/local/texlive/2014/bin/x86_64-darwin:$PATH

# Better compilation
export CFLAGS="-I/opt/local/include -I/opt/X11/include ${CFLAGS}"
export LDFLAGS="-L/opt/local/lib ${LDFLAGS}"
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig:${PKG_CONFIG_PATH}"

# Python PATH
export PYTHONPATH=/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages:/Library/Python/2.7/site-packages:/Users/abelardojara/workspace/pythonlibs/lib/python2.7/site-packages

# Setting PATH for Python 2.7
export PATH="/Library/Frameworks/Python.framework/Versions/2.7/bin":$PATH

# Java options
export _JAVA_OPTIONS="-Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel -Djava.net.preferIPv4Stack=true -Dawt.useSystemAAFontSettings=on -Dswing.aatext=true"

# Emacs options
export EMACS_SERVER_FILE="${HOME}/.emacs.cache/server/server"

# Aliases
alias gedit='/Applications/gedit.app/Contents/MacOS/Gedit'
alias ls='ls -lsF'

