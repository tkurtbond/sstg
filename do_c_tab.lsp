;;; DO_C_TAB.LSP -- Create C tables for SDCL scanner.
;;; Execute this from main sdcl directory.
(load "[.sstg]sstg_to_c.lsp" t)
(sstg-to-c "sdcl.table" "codes.h" "lextab.h")
(exit)
