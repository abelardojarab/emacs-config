alias ls       'ls -l --color -sF'
alias cd 'cd \!*; set prompt = "%{\033[34m%}[%n@%{\033[35m%}%m%{\033[33m%} %b%{\033[0m%}%c]$ "'
alias emacs '/nfs/pdx/home/ajaraber/utils/emacs-24.5/src/emacs'
alias emacsclient 'emacsclient -n'

setenv JAVA_HOME /usr/intel/pkgs/java/1.8.0
setenv PATH ${HOME}/bin:${HOME}/bin/bin:$HOME/workspace/pythonlibs/bin:${JAVA_HOME}/bin:${PATH}

unsetenv PYTHONPATH
setenv EC_DISABLE_VAL 1
setenv CAMDEXGT_CAD_ROOT ${HOME}/camdex/data
setenv PYTHONPATH ${HOME}/workspace/pythonlibs/lib/python2.7/site-packages:${HOME}/workspace/frametools/extratools/ctktools/tools/oascript/v3.1_oa22.43p028/linux_rhel55_gcc44x_64/python
setenv _JAVA_OPTIONS '-Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel -Djava.net.preferIPv4Stack=true -Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Xmx750m'
setenv http_proxy http://proxy-chain.intel.com:911
setenv https_proxy http://proxy-chain.intel.com:911
setenv socks_proxy http://proxy-socks.fm.intel.com:1080
setenv no_proxy intel.com,.intel.com,10.0.0.0/8,192.168.0.0/16,localhost,127.0.0.0/8,134.134.0.0/16
setenv GIT_PROXY_COMMAND /nfs/pdx/home/ajaraber/bin/gitproxy
setenv GDK_NATIVE_WINDOWS 1
setenv EMACS_SERVER_FILE $HOME/.emacs.cache/server/server
setenv DICTPATH $HOME/workspace/emacsfull/.emacs.d/dictionaries
setenv GTAGSLIBPATH $HOME/workspace/dcg:$HOME/workspace/ClionProjects

# Libraries for OpenAccess
set oa_version=22.43p028
set oascript_version=v3.1_oa${oa_version}
set oaxpop_version=v1.2_oasv3.1_oa${oa_version}
set oaxctk_version=v0.1_oasv3.1_oa${oa_version}
set platform=linux_rhel55_gcc44x_64
set ctk_tool_root=${HOME}/workspace/frametools/extratools/ctktools/tools
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

# Altera options
# setenv QUARTUS_HOME /nfs/site/eda/data/tools.6/altera/quartusprime/15.1.0.185/common/quartus
setenv QUARTUS_HOME /p/atp/tools/altera/quartus/Pro_15.1.2.Patch-2.20/quartus
setenv QUARTUS_ROOTDIR $QUARTUS_HOME
setenv QUARTUS_64BIT 1
setenv QUARTUS_ROOTDIR_OVERRIDE $QUARTUS_HOME
setenv PATH  "${QUARTUS_HOME}/bin:${PATH}"

# Tue Apr 21 13:12:39 PDT 2015 - new License servers for Altera
if (!($?LM_LICENSE_FILE)) then
setenv LM_LICENSE_FILE ""
endif
setenv LM_LICENSE_FILE "1800@altera02p.elic.intel.com:1800@fmylic7001.fm.intel.com:1800@fmylic7008.fm.intel.com"

#Altera Quartus
setenv LM_LICENSE_FILE "1800@altera02p.elic.intel.com"
setenv LM_LICENSE_FILE "${LM_LICENSE_FILE}:1800@fmylic7001.fm.intel.com"
setenv LM_LICENSE_FILE "${LM_LICENSE_FILE}:1800@fmylic7008.fm.intel.com"
setenv LM_LICENSE_FILE "${LM_LICENSE_FILE}:1800@plxs0402.pdx.intel.com"
#setenv LM_LICENSE_FILE "${LM_LICENSE_FILE}:2100@fmylic40a.fm.intel.com"

# set license for VCS
setenv SNPSLMD_LICENSE_FILE "26586@plxs0402.pdx.intel.com"
setenv SNPSLMD_LICENSE_FILE "${SNPSLMD_LICENSE_FILE}:26586@plxs0405.pdx.intel.com"
setenv SNPSLMD_LICENSE_FILE "${SNPSLMD_LICENSE_FILE}:26586@plxs0406.pdx.intel.com"
setenv SNPSLMD_LICENSE_FILE "${SNPSLMD_LICENSE_FILE}:26586@plxs0408.pdx.intel.com"
setenv SNPSLMD_LICENSE_FILE "${SNPSLMD_LICENSE_FILE}:26586@plxs0414.pdx.intel.com"
setenv SNPSLMD_LICENSE_FILE "${SNPSLMD_LICENSE_FILE}:26586@plxs0415.pdx.intel.com"
setenv SNPSLMD_LICENSE_FILE "${SNPSLMD_LICENSE_FILE}:26586@plxs0416.pdx.intel.com"
setenv SNPSLMD_LICENSE_FILE "${SNPSLMD_LICENSE_FILE}:26586@plxs0418.pdx.intel.com"
setenv SNPS_FPGA /p/atp/tools/synopsys/fpga/linux/fpga_e201009sp3
setenv PATH ${PATH}:$SNPS_FPGA/bin

# set OVM_HOME to OVM installation directory
setenv OVM_HOME /p/atp/ovm/ovm-2.1.2

# set VCS_HOME to VCS installation directory
#setenv VCS_HOME /p/atp/tools/synopsys/D-2009.12-5
#setenv VCS_HOME /p/atp/tools/synopsys/vcs-mx/I-2014.03-2
setenv VCS_HOME /p/atp/tools/synopsys/vcs/G-2012.09
setenv PATH ${PATH}:$VCS_HOME/bin

if (-e /etc/redhat-release) then
    setenv VCS_PLATFORM amd64
    setenv VCS_ARCH_OVERRIDE linux
else
    setenv VCS_PLATFORM suse64
endif

#if !($?SC_INSTALL_DIR) then
    setenv SC_INSTALL_DIR /p/atp/systemc/systemc-2.2.0
#endif
#if !($?CSI_TOP) then
    setenv CSI_TOP /p/atp/qpi_bfm/csibfm-QPI1.1-OSCI2.2-gcc410-3.35
#endif

# set XILINX_HOME and XILINX
#setenv XILINX_HOME /p/atp/tools/xilinx/13.4
#setenv XILINX $XILINX_HOME/ISE
#setenv XILINX_EDK $XILINX_HOME/EDK
#setenv XILINX_DSP $XILINX_HOME/ISE
#setenv XILINX_PLANAHEAD $XILINX_HOME/PlanAhead

# Boost
setenv BOOST_ROOT /usr/intel/pkgs/boost/1.59.0

# set LD_LIBRARY_PATH
setenv LD_LIBRARY_PATH "${HOME}/bin/lib"
setenv LD_LIBRARY_PATH "${LD_LIBRARY_PATH}"
setenv LD_LIBRARY_PATH "${LD_LIBRARY_PATH}:/usr/lib64:/usr/lib:/lib"
setenv LD_LIBRARY_PATH "${LD_LIBRARY_PATH}:${VCS_HOME}/amd64/lib"
setenv LD_LIBRARY_PATH "${LD_LIBRARY_PATH}:${SC_INSTALL_DIR}/lib-linux64"
setenv LD_LIBRARY_PATH "${LD_LIBRARY_PATH}:${CSI_TOP}/sysverilog/Linux_x86_64/lib:${CSI_TOP}/lib/Linux_x86_64:${CSI_TOP}/csi/crm_1_1"
setenv PERL5LIB /p/atp/tools/common

# PKG_CONFIG
setenv PKG_CONFIG_PATH /nfs/site/itools/em64t_SLES11/pkgs/gtk+/2.24.20/lib/pkgconfig:/nfs/pdx/home/ajaraber/bin/lib/pkgconfig:/usr/lib64/pkgconfig
