$!> SSTG_SETUP.COM - Set up commands for using SSTG and SSTG_TO_C.
$!-----------------------------------------------------------------------------
$! This requires that the dcl symbol SCM already be set up as a 
$! DCL foreign command that runs SCM.
$!-----------------------------------------------------------------------------
$ proc = f$environment ("PROCEDURE")
$ procdev = f$parse (proc,,, "DEVICE")
$ procdir = f$parse (proc,,, "DIRECTORY")
$ sstg_dir = procdev + procdir
$ write sys$output "sstg_dir: ", sstg_dir
$ if f$type (scm) .eqs. "" then @com:scm_setup ! make sure SCM is defined.
$ sstg :== 'scm' 'sstg_dir'sstg.scm
$ sstg_to_c :== 'scm' 'sstg_dir'sstg_to_c.scm
