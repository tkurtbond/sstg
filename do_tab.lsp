;;; DO_TAB.LSP -- create the SDCL scanner tables.
;;; Execute this from main SDCL directory.
(load "[.sstg]sstg.lsp" t)		;load the necessary lisp definitions
(gen-scantab "sdcl.scn" "sdcl.table")	;process scanner data and make tables
(exit)					;exit the lisp
