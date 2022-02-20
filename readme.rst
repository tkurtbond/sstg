SSTG -- Simple Scanner Table Generator
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

Documentation at the bottom of SSTG.SCM and SSTG.LSP.  

The .LSP files are for David Betz's XLISP_ 1.7 (circa 1986).

The .SCM files are Scheme (R4RS) source code and are a little newer,
and probably more portable.  They run in SCM version 4c0.

SSTG was originally written around 1986 or 1987.  It was used to
generate the scanners for a couple of projects I wrote around that
time, SDCL_ and FRG.  SDCL_ is a Structured DCL preprocessor, a
complete rewrite from scratch of Sohail Aslam's SDCL, and added
if-then-else statements, loops, and compound statements to DCL.  (DCL
only added multi-statement IF-THEN and ELSE statements somewhat
later.)  FRG was a compiler (written in VAX BASIC and VAX MACRO and
producing VAX BASIC) for a a report generation language that used
POISE DMS files as input to produce spreadsheet-like summaries.

In early 2022 I completed porting SSTG_TO_C to R4RS using SCM_ 4c0
(still available here_), the Scheme available on the VAX/VMS v5.5-2
system that I maintain, in order to recompile SDCL_ in preparation for
putting it up on GitHub.


.. _XLISP: http://www.softwarepreservation.org/projects/LISP/xlisp
.. _SDCL: https://github.com/tkurtbond/sdcl
.. _SCM: https://people.csail.mit.edu/jaffer/SCM.html
.. _here: http://groups.csail.mit.edu/mac/ftpdir/scm/OLD/
