# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
        # We have color support; assume it's compliant with Ecma-48
        # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
        # a case would tend to support setf rather than setaf.)
        color_prompt=yes
    else
        color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
    xterm*|rxvt*)
        PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
        ;;
    *)
        ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

alias lmli='/opt/cadence/installs/IC615/tools/bin/lmgrd -c /opt/cadence/installs/IC615/share/license/license.dat'

# export OA_UNSUPPORTED_PLAT=linux_rhel40
export OA_UNSUPPORTED_PLAT=linux_rhel40_gcc44x
# export OA_HOME=/opt/Ansys/Totem14.1.b2/lib/totem2/oa
export OA_HOME=/opt/cadence/installs/IC615/oa_v22.41.029
export PYTHONPATH=~/workspace/pythonlibs/lib/python2.7/site-packages
export PYTHONPATH=${PYTHONPATH}:/opt/oaScript/python2
export CDSROOT=/opt/cadence/installs/IC615
export CDSHOME=/opt/cadence/installs/IC615
export CDS_ROOT=/opt/cadence/installs/IC615
export CDS_LOG_PATH=/tmp
export CDS_LOG_VERSION=pid
export CDS_AUTO_CKOUT=all
export CDS_LOAD_ENV=CWD
export CDS_Netlisting_Mode=Analog
export KEEP_CDS_LIB_MGR=1
export MMSIM_ROOT=/opt/cadence/installs/MMSIM121
export APACHEDA_LICENSE_FILE=$HOME/flexlm/totem.lic
export APACHEROOT=/opt/Ansys/Totem14.1.b2
export CALIBRE_HOME=/opt/mentor/calibre/2007.1_17.18
export MGC_HOME=/opt/mentor/calibre/2007.1_17.18
export MGLS_LICENSE_FILE=$HOME/flexlm/calibre.dat
export LM_LICENSE_FILE=$HOME/flexlm/license.dat
export LM_LICENSE_FILE=$LM_LICENSE_FILE:$CDS_ROOT/share/license/license.dat:$HOME/flexlm/cadence.dat:$HOME/flexlm/synopsys.dat:$HOME/flexlm/modelsim.dat:$HOME/flexlm/matlab.dat
export PATH=$MMSIM_ROOT/tools/bin:$MMSIM_ROOT/tools/spectre/bin:$CDS_ROOT/tools/bin:$CDS_ROOT/tools/dfII/bin:/opt/cds/soc710/bin:/opt/synopsys/starrc/2012.12/bin:/opt/synopsys/hspice/H-2013.03-SP2/hspice/bin:/opt/synopsys/cosmosscope/H-2012.12/ai_bin:/opt/mentor/modelsim/6.5f/modeltech/bin:/opt/mentor/calibre/2007.1_17.18/bin:/opt/PyCharm/4.0.6/bin:/opt/IBM/SPSS/Statistics/22/bin:/opt/synopsys/icweb/G-2012.06-SP1/bin/amd64:/opt/synopsys/icvalidator/H-2013.06/bin/SUSE.64:$PYTHONPATH/bin:$PATH:~/workspace/frametools/bin
export CDK_DIR=/opt/ncsu-cdk-1.6.0
export PDK_DIR=/opt/FreePDK45
export PDK_DIR_SC=/opt/FreePDK45StandardCells
export _JAVA_OPTIONS="-Xmx4g -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel -Djava.net.preferIPv4Stack=true -Dawt.useSystemAAFontSettings=on -Dswing.aatext=true"
export GDK_NATIVE_WINDOWS=1
