alias ls       'ls -l --color -sF'
alias cd 'cd \!*; set prompt = "%{\033[34m%}[%n@%{\033[35m%}%m%{\033[33m%} %b%{\033[0m%}%c]$ "'
alias emacs '$HOME/workspace/emacspkgs/emacs-24.5/src/emacs'
alias emacsclient 'emacsclient -n'

setenv JAVA_HOME /usr/intel/pkgs/java/1.8.0
if ( -f /etc/SuSE-release && `grep --count 11 /etc/SuSE-release` > 0 ) then
   setenv PATH ${HOME}/bin:${HOME}/bin/bin:${HOME}/workspace/frametools/bin:${JAVA_HOME}/bin:${PATH}:/p/advda/utils/bin
else
   setenv PATH ${HOME}/workspace/builds/emacs-24.4/src:/p/advda/utils/bin:${HOME}/bin:${HOME}/workspace/frametools/bin:${HOME}/builds/nodejs/bin:${PATH}:${HOME}/bin:${HOME}/bin/bin
endif

unsetenv PYTHONPATH

setenv EC_DISABLE_VAL 1
setenv CAMDEXGT_CAD_ROOT ${HOME}/camdex/data
setenv PYTHONPATH ${HOME}/workspace/pythonlibs/lib/python2.7/site-packages:${HOME}/workspace/frametools/extratools/ctk/tools/oascript/v3.1_oa22.43p028/linux_rhel55_gcc44x_64/python
setenv _JAVA_OPTIONS '-Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel -Djava.net.preferIPv4Stack=true -Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Xmx750m'
setenv http_proxy http://proxy-chain.intel.com:911
setenv https_proxy http://proxy-chain.intel.com:911
setenv socks_proxy http://proxy-socks.fm.intel.com:1080
setenv no_proxy intel.com,.intel.com,10.0.0.0/8,192.168.0.0/16,localhost,127.0.0.0/8,134.134.0.0/16
setenv GIT_PROXY_COMMAND /nfs/pdx/home/ajaraber/bin/gitproxy
setenv GDK_NATIVE_WINDOWS 1
setenv INTEL_CTK $HOME/workspace/frametools/extratools/ctk
setenv EMACS_SERVER_FILE $HOME/.emacs.cache/server/server

# Libraries for OpenAccess
set oa_version=22.43p028
set oascript_version=v3.1_oa${oa_version}
set oaxpop_version=v1.2_oasv3.1_oa${oa_version}
set oaxctk_version=v0.1_oasv3.1_oa${oa_version}
set platform=linux_rhel55_gcc44x_64
set ctk_tool_root=${HOME}/workspace/frametools/extratools/ctk/tools
set oascript_root=${ctk_tool_root}/oascript/$oascript_version/$platform
set oaxpop_root=${ctk_tool_root}/oaxpop/$oaxpop_version/$platform
set oa_libs=${ctk_tool_root}/oa/$oa_version/$platform/lib/linux_rhel50_gcc44x_64/opt
set oa_libs=${oa_libs}:${oascript_root}/extras/x86_64
set oaxpop_libs=${oaxpop_root}/lib/linux_rhel50_gcc44x_64/opt
set add_libs=${HOME}/bin/lib:${oa_libs}:${oaxpop_libs}:${ctk_tool_root}/libs:/usr/intel/pkgs/pixman/0.19.2/lib:/usr/intel/pkgs/openssl/0.9.8o/lib
set add_libs_post=/usr/lib64:/usr/lib

# Front-load LD_LIBRARY_PATH
alias python_ctk_load 'setenv LD_LIBRARY_PATH ${add_libs}:${add_libs_post}'
alias python_ctk '${INTEL_CTK}/bin/python_ctk'
