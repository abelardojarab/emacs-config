(defun start-skill ()
  "Starts a local SKILL 'interpretter' that talks to skill over
   a socket. Note that this socket has to exist a priori. This 
   socket is automatically created on cadence startup so long as
   the TCC_START_SERVER  environment variable is set (this is done
   in $CDS_SITE/.cdsinit.testchip"
  (interactive)
  ;;point ilisp to the "interpretter"
  (run-lisp "/p/tccdev/work_areas/vrayapp/tccdev/tcc_collateral/core/tcl/skill-repl.tcl"))
