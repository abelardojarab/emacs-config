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

# General settings
export EMACS_SERVER_FILE=$HOME/.emacs.cache/server/server
export PYTHONPATH=~/workspace/pythonlibs/lib/python2.7/site-packages
export PYTHONPATH=${PYTHONPATH}:/opt/oaScript/python2
export LM_LICENSE_FILE=$HOME/flexlm/license.dat
export _JAVA_OPTIONS=" -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel -Djava.net.preferIPv4Stack=true -Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Xms512m -Xmx1024m"
export theHost=`hostname`
alias lmlicense='/opt/cadence/installs/IC616/tools.lnx86/bin/lmgrd -c'

# OpenAccess
export OA_UNSUPPORTED_PLAT=linux_rhel50_gcc44x
export OA_HOME=/opt/cadence/installs/IC616/oa_v22.43.018

# Python settings
export PYTHONPATH=~/workspace/pythonlibs/lib/python2.7/site-packages
export PYTHONPATH=${PYTHONPATH}:/opt/oaScript/python2

# PDKs
export CDK_DIR=/opt/ncsu-cdk-1.6.0
export PDK_DIR=/opt/FreePDK45
export PDK_DIR_SC=/opt/FreePDK45StandardCells

# Cadence settings
export CDS_AUTO_64BIT=NONE
export CDSROOT=/opt/cadence/installs/IC615
export CDSHOME=/opt/cadence/installs/IC615
export CDS_ROOT=/opt/cadence/installs/IC615
export BASIC_LIB_PATH=$CDSROOT/tools/dfII/etc/cdslib/basic
export ANALOG_LIB_PATH=$CDSROOT/tools/dfII/etc/cdslib/artist/analogLib/
export CDS_LIC_FILE=27000@ubuntu
export CDS_LOG_PATH=/tmp
export CDS_LOG_VERSION=pid
export CDS_AUTO_CKOUT=all
export CDS_LOAD_ENV=CWD
export CDS_Netlisting_Mode=Analog
export EDI_ROOT=/opt/cadence/installs/EDI131
export MMSIM_ROOT=/opt/cadence/installs/MMSIM121
export PATH=$MMSIM_ROOT/tools/bin:$MMSIM_ROOT/tools/spectre/bin:$CDS_ROOT/tools/bin:$CDS_ROOT/tools/dfII/bin:$EDI_ROOT/bin:$PATH
export LM_LICENSE_FILE=$LM_LICENSE_FILE:$HOME/flexlm/cadence.dat

# Apache settings
export APACHEDA_LICENSE_FILE=$HOME/flexlm/apache.dat
export APACHEROOT=/opt/ansys/Totem14.1.b2
export PATH=$APACHEROOT/bin:$PATH
export LM_LICENSE_FILE=$LM_LICENSE_FILE:$HOME/flexlm/apache.dat

# Synopsys settings
export SYNOPSYS=/opt/synopsys
export SNPSLMD_LICENSE_FILE=$HOME/flexlm/synopsys.dat
export SNPS_DC_ROOT=/opt/synopsys/designcompiler/B-2008.09
export SNPS_HSPICE_ROOT=/opt/synopsys/hspice/F-2011.09-SP2
export SNPS_STARRC_ROOT=/opt/synopsys/starrc/H-2012.12
export SNPS_COSMOSSCOPE_ROOT=/opt/synopsys/cosmosscope/H-2012.12
export SNPS_ICWB_ROOT=/opt/synopsys/icweb/G-2012.06-SP1
export SNPS_ICV_ROOT=/opt/synopsys/icvalidator/H-2013.06
export PATH=$SNPS_HSPICE_ROOT/hspice/linux:$SNPS_DC_ROOT/bin:$SNPS_STARRC_ROOT/bin:$SNPS_COSMOSSCOPE_ROOT/ai_bin:$PATH
export PATH=$SNPS_ICWB_ROOT/bin/amd64:$SNPS_ICV_ROOT/bin/SUSE.64:$PATH
export LM_LICENSE_FILE=$LM_LICENSE_FILE:$HOME/flexlm/synopsys.dat

# Mentor settings
export MODELSIMROOT=/opt/mentor/modelsim/10.1c
export CALIBRE_HOME=/opt/mentor/calibre/2013.3_28.19
export MGC_HOME=/opt/mentor/calibre/2013.3_28.19
export MGLS_LICENSE_FILE=$HOME/flexlm/calibre.dat:$HOME/flexlm/modelsim.dat
export PATH=$MODELSIMROOT/modeltech/bin:$CALIBRE_HOME/bin:$PATH
export LM_LICENSE_FILE=$LM_LICENSE_FILE:$HOME/flexlm/calibre.dat

# Aldec settings
export ALDEC_LICENSE_FILE=$HOME/flexlm/aldec.dat

# Matlab settings
export LM_LICENSE_FILE=$LM_LICENSE_FILE:$HOME/flexlm/matlab.dat
export PATH=/opt/PyCharm/4.0.6/bin:/opt/IBM/SPSS/Statistics/22/bin:$PYTHONPATH/bin:$PATH:$HOME/workspace/frametools/bin

# Sublime Text settings
export PATH=/opt/sublime_text:$PATH

# Set up general GTAGS location
export GTAGSLIBPATH=$HOME/.gtags/

# Regular Colors
Black="\[\033[0;30m\]"        # Black
Red="\[\033[0;31m\]"          # Red
BRed="\[\033[1;31m\]"         # Bold red
Green="\[\033[0;32m\]"        # Green
BGreen="\[\033[1;32m\]"       # Bold green
Yellow="\[\033[0;33m\]"       # Yellow
BYellow="\[\033[1;33m\]"      # Bold yellow
Cyan="\[\033[0;36m\]"         # Cyan
Gray="\[\033[1;30m\]"         # Gray
White="\[\033[0;37m\]"        # White

# Prompt separator
#  This will go between the git indicators and the dollar sign.  It's empty by default,
#  but something I commonly do in the shell is to assign a newline to it, so you have
#  a status line, and then the prompt where you type your command is on the next line.
#
# Example:
#     [12:38:13] user@hostname example_repo (master)*$ SEP="
#     > "
#     [12:41:54] user@hostname example_repo (master)*
#     $

SEP=""

export PS1="\
$Gray[\t] \
$Green\u@\h \
$Yellow\W \
\$(\
    # get the reference description
    if refname=\$(git name-rev --name-only HEAD 2> /dev/null); then\
        # on a branch
        if curbranch=\$(git symbolic-ref HEAD 2> /dev/null); then\
            echo -n '$Cyan('\${curbranch##refs/heads/}')';\
        # detached head
        else\
            # unreachable
            if [ \$refname = 'undefined' ]; then\
                echo -n '$BRed(Unreachable detached HEAD: '\$(git rev-parse HEAD | head -c7)')';\
            # reachable
            else\
                echo -n '$White('\$(git rev-parse HEAD | head -c7)': '\$refname')';\
            fi;\
        fi;\
        git diff --quiet --cached &> /dev/null \
            || echo -n '$BGreen*';\
        git diff --quiet &> /dev/null \
            || echo -n '$BRed*';\
  git status --porcelain 2> /dev/null | grep -q ^?? \
        && echo -n '$Gray*';\
        echo -n ' ';\
    fi\
)\
$White\$SEP\
\$(\
    if [ \$USER = 'root' ]; then\
        echo -n '$Yellow#';\
    else\
        echo -n '$Green$';\
    fi;\
)\
$White "
