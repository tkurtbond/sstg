;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; sstg_to_bas.lsp -- convert SSTG output to VAX BASIC and VAX MACRO
;;;;                    SSTG == simple scanner table generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Author:   Thomas Kurt Bond
;;;
;;; Address:  AdminiSoft, Inc.                 Rt. 1, Box 74     
;;;           P.O. Box 415              or     Jane Lew, WV 26378
;;;           Dellslow, WV  26531
;;;
;;; Description:
;;;
;;; This program converts the intermediate file produced by SSTG.LSP
;;; into a VAX BASIC source code file and a VAX MACRO source code
;;; file.  The VAX MACRO file, the `table file', defines the constants
;;; and the initialized tables for driving a lexical analyser (since
;;; VAX BASIC doesn't have any way to do this conviently and
;;; efficently).  The VAX BASIC file, the `code file', declares the
;;; constants as EXTERNAL, so their values in the VAX MACRO file are
;;; picked up at link time; it is intended to be used as an include
;;; file.
;;;
;;; To use the program, load it into XLISP and give the following
;;; command:
;;;        (sstg-to-bas "inputfilename" "codefilename" "tablefilename")
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Here is the format of the intermediate file.  Items in angle
;;; brackets are data; items starting with a colon are directives.
;;; Alternate forms are preceded by a single upright bar, and refer to
;;; the entire line.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; :creator <string>			;program that created this file
;;;
;;; :version <string>			;of the creator
;;;
;;; :source <string>			;source file that produced this output
;;;
;;; :classes                            ;character class constant definitions
;;;     <classname> <value>
;;;
;;; :chartoclass <length>               ;character to character class mapping
;;;     <classname> <char-int-value>
;;;    
;;; :actions                            ;action constant definitions
;;;     <action> <value>
;;;
;;; :tokens                             ;token constant definitions
;;;     <token> <value>
;;; 
;;; :returns <number-of-states>         ;mapping of end state to return token 
;;;     <token> <state-number>
;;; 
;;; :errors <number-of-states>          ;mapping of error messages to state
;;;     <error-message> <state-number>
;;; 
;;; :start                              ;start state constant definition
;;;     <start-name> <state-num>
;;;     | :none
;;; 
;;; :accept                             ;accept state constant definition
;;;     <accept-name> <state-num>
;;;     | :none
;;;
;;; ;table of state transitions for each (state, character class) pair
;;; :goto-matrix <rows number-of-states> <columns number-of-character-classes>
;;;     <goto>				;1st row, 2nd row, etc.
;;;
;;; ;table of actions for each (state, character class) pair
;;; :action-matrix <rows num-of-states> <columns num-of-character-classes>
;;;     <action>			;1st row, 2nd row, etc.
;;; 
;;;---------------------------------------------------------------------------
;;;
;;; This program is *very* sensitive to data format.  The input *must*
;;; correct or it runs in an infinite loop.  Since the input file is
;;; supposed to be produced *only* by SSTG.LSP (function
;;; gen-scantab), this should not be a problem.
;;;
;;; This program was written while I was learning LISP, so some of the
;;; code is not ideal.  It was written using XLISP, but shouldn't be to
;;; hard to port to another LISP.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Support routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pr (files &rest values)		;print on FILES the VALUES 
  (if (not (listp files))
      (setq files (list files)))
  (dolist (f files)
    (dolist (v values)
      (princ v f))))

(defun prt (files &rest values)		;print on FILES the VALUES and newline
  (if (not (listp files))
      (setq files (list files)))
  (dolist (f files)
    (dolist (v values)
      (princ v f))
    (terpri f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;output routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq first-tc-value 10)		;token code values start with this

(defun sstg-to-bas (input-file-name code-file-name table-file-name)
  (let* (inf tabl-f tabl-f both)
    (setq inf    (openi input-file-name))
    (setq code-f (openo code-file-name))
    (setq tabl-f (openo table-file-name))
    (setq both   (list tabl-f code-f))

    (process-creator inf tabl-f code-f both)
    (process-version inf tabl-f code-f both)
    (process-source  inf tabl-f code-f both
		     input-file-name code-file-name table-file-name)
    (process-classes inf tabl-f code-f both)
    (process-chartoclass inf tabl-f code-f both)
    (process-actions inf tabl-f code-f both)
    (process-tokens  inf tabl-f code-f both)
    (process-returns inf tabl-f code-f both)
    (process-errors  inf tabl-f code-f both)
    (process-start   inf tabl-f code-f both)
    (process-accept  inf tabl-f code-f both)
    (process-goto-mat inf tabl-f code-f both)
    (process-act-mat inf tabl-f code-f both)

    ;;close files
    (close inf)
    (close tabl-f)
    (close code-f)))


;;process :creator
(defun process-creator (inf tabl-f code-f both)
  (let (item item2)
    (princ "Processing :CREATOR...")
    (read inf)				;get rid of :creator
    (read inf)				;get rid of string
    (princ "done\n")))

;;process :version
(defun process-version (inf tabl-f code-f both)
  (let (item item2)
    (princ "Processing :VERSION...")
    (read inf)				;get rid of :version
    (read inf)				;get rid of string
    (princ "done\n")))


;;process :source
(defun process-source (inf tabl-f code-f both
			   input-file-name code-file-name table-file-name)
  (let (item item2)
    (princ "Processing :SOURCE...")
    (read inf)				;get rid of :source
    (pr tabl-f ";!> " table-file-name)
    (pr code-f "\t!> " code-file-name)
    (prt both " -- made from the file " (read inf)
	 " (by way of " input-file-name ")")
    (terpri tabl-f)
    (terpri code-f)
    (princ "done\n")))

;;process :classes
(defun process-classes (inf tabl-f code-f both)
  (let (item item2)
    (princ "Processing :CLASSES...")
    (read inf)				;ignore :classes
    (pr tabl-f "; ")
    (pr code-f "\t! ")
    (prt both "character classes")
    (prt code-f "\texternal byte constant &")
    (setq item (read inf))		;first classname
    (do ((i 0 (1+ i))) ((eq item :chartoclass))
      (setq item2 (read inf))
      (pr tabl-f item " == " item2 "\n")
      (pr code-f (if (/= i 0) ", &\n\t\t" "\t\t") item)
      (setq item (read inf)))
    (terpri tabl-f)
    (terpri code-f)
    (terpri code-f)
    (princ "done\n")))


;;process :chartoclass
(defun process-chartoclass (inf tabl-f code-f both)
  (let (item item2)
    (princ "Processing :CHARTOCLASS...")
    (setq item2 (read inf))		;length of :chartoclass
    (pr tabl-f "; ")
    (pr code-f "\t! ")
    (prt both "Array for translating characters to character classes")
    (prt tabl-f "\t.psect char_to_class pic,usr,ovr,rel,gbl,"
	 "shr,noexe,rd,wrt,novec,long")
    (prt code-f "\tcommon (char_to_class) byte char_to_class(0 to "
	 (1- item2) ")") 
    (terpri code-f)
    (do ((i 0 (1+ i))) ((>= i item2))
      (setq item (read inf))
      (read inf)			;get rid of integer
      (prt tabl-f "\t.byte " item))
    (terpri tabl-f)))

;;process :actions
(defun process-actions (inf tabl-f code-f both)
  (let (item item2)
    (read inf)				;ignore :actions
    (pr tabl-f "; ")
    (pr code-f "\t! ")
    (prt both "Action Codes")
    (prt code-f "\texternal byte constant &")
    (setq item (read inf))		;first action
    (do ((i 0 (1+ i))) ((eq item :tokens))
      (setq item2 (read inf))
      (prt tabl-f item " == " item2)
      (pr code-f (if (/= i 0) ", &\n\t\t" "\t\t") item)
      (setq item (read inf)))
    (terpri tabl-f)
    (terpri code-f)
    (terpri code-f)    
    (princ "done\n")))

;;process :tokens
;; tokens must be in increasing order, numbered from 0,
;; and sequential without gaps.
(defun process-tokens (inf tabl-f code-f both)
  (let (item item2 token-list)
    (princ "Processing :TOKENS...")
    (pr tabl-f "; ")
    (pr code-f "\t! ")
    (prt both "Token Codes")
    (prt code-f "\texternal byte constant &")
    (prt tabl-f "first_tc_value == " first-tc-value)
    (setq item (read inf))		;first token
    (do ((i 0 (1+ i))) ((eq item :returns))
      (setq item2 (read inf))
      (setq token-list (append token-list (list (cons item item2))))
      (prt tabl-f item " == " (+ first-tc-value item2))
      (pr code-f (if (/= i 0) ", &\n\t\t" "\t\t") item)
      (setq item (read inf)))
    (terpri tabl-f)
    (terpri code-f)
    (terpri code-f)
    (prt tabl-f "tc_str_max == " (+ (1- first-tc-value) (length token-list)))
    (dotimes (i (length token-list))
      (prt tabl-f "TCS_" i ":\t.ascid /" (car (nth i token-list)) "/"))
    (prt tabl-f "TCS_" (length token-list) ":\t.ascid /erroneous token/")
    (prt tabl-f "tc_str_array::")
    (dotimes (i (1+ (length token-list)))
      (prt tabl-f "\t.long\tTCS_" i))
    (terpri tabl-f)
    (terpri tabl-f)
    (princ "done\n")))

;;process :returns
(defun process-returns (inf tabl-f code-f both)
  (let (item item2)
    (princ "Processing :RETURNS...")
    (setq item2 (read inf))		;length of :returns
    (pr tabl-f "; ")
    (pr code-f "\t! ")
    (prt both "Array for translating states to return token codes")
    (prt tabl-f "\t.psect return_codes pic,usr,ovr,rel,gbl,"
	 "shr,noexe,rd,wrt,novec,long")
    (prt code-f "\tcommon (return_codes) byte return_codes(0 to "
	 (1- item2) ")")
    (terpri code-f)
    (do ((i 0 (1+ i))) ((>= i item2))
      (setq item (read inf))
      (read inf)			;get rid of integer
      (prt tabl-f "\t.byte " item))
    (terpri tabl-f)
    (princ "done\n")))

;;process :errors
(defun process-errors (inf tabl-f code-f both)
  (let (item item2)
    (princ "Processing :ERRORS...")
    (read inf)				;get rid of :errors
    (setq item (read inf))		;length of errors table
    (prt tabl-f "; Lexical error message strings")
    (do ((i 0 (1+ i))) ((>= i item))
      (prt tabl-f "LXM_" i ":\t.ascid /" (read inf) "/")
      (read inf))			;get rid of state number
    (prt tabl-f
	 "LXM_" item
	 ":\t.ascid /internal error: invalid lexical error message number/")
    (prt tabl-f "lex_mess_max == " item)
    (terpri tabl-f)
    (prt tabl-f "; Lexical error message vector table")
    (prt tabl-f "lex_messages::")
    (do ((i 0 (1+ i))) ((> i item))
      (prt tabl-f "\t.long LXM_" i))
    (terpri tabl-f)
    (princ "done\n")))

;;process :start
(defun process-start (inf tabl-f code-f both)
  (let (item item2)
    (princ "Processing :START...")
    (read inf)				;get rid of :start
    (pr tabl-f "; ")
    (pr code-f "\t! ")
    (prt both "Start state")
    (setq item (read inf))
    (cond  ((not (eq item :none))
	    (princ "(exists)...")
	    (prt tabl-f item " == " (read inf))
	    (prt code-f "\texternal byte constant &\n\t\t" item))
	   (t
	    (princ "(does not exist)...")))
    (terpri tabl-f)
    (terpri code-f)
    (princ "done\n")))

;;process :accept if it exists
(defun process-accept (inf tabl-f code-f both)
  (let (item item2)
    (princ "Processing :ACCEPT...")
    (setq item (read inf))		;get rid of :accept
    (pr tabl-f "; ")
    (pr code-f "\t! ")
    (prt both "Accept State")
    (setq item (read inf))
    (cond ((not (eq item :none))
	   (princ "(exists)...")
	   (prt tabl-f item " == " (read inf))
	   (prt code-f "\texternal byte constant &\n\t\t" item))
	  (t
	   (princ "(does not exist)...")))
    (terpri tabl-f)
    (terpri code-f)
    (princ "done\n")))

;;process :goto-matrix
(defun process-goto-mat (inf tabl-f code-f both)
  (let (item item2)
    (princ "Processing :GOTO-MATRIX...")
    (pr tabl-f "; ")
    (pr code-f "\t! ")
    (prt both "Goto matrix (next state transitions matrix)")
    (read inf)				;get rid of :goto-matrix
    (setq item (read inf))		;number of states (rows)
    (setq item2 (read inf))		;number of character classes (columns)
    (prt tabl-f "\t.psect next_state pic,usr,ovr,rel,gbl,"
	 "shr,noexe,rd,wrt,novec,long")
    (prt code-f "\tcommon (next_state) byte next_state(0 to "
	 (1- item) ", 0 to " (1- item2) ")")
    (dotimes (i item)
      (dotimes (j item2)
	(prt tabl-f "\t.byte " (read inf)))
      (terpri tabl-f))
    (terpri tabl-f)
    (terpri code-f)
    (princ "done\n")))
    
;;process :action-matrix
(defun process-act-mat (inf tabl-f code-f both)
  (let (item item2)
    (princ "Processing :ACTION-MATRIX...")
    (read inf)				;get rid of :action-matrix
    (pr tabl-f "; ")
    (pr code-f "\t! ")
    (prt both "action matrix (next action transitions matrix)")
    (setq item (read inf))		;number of states (rows)
    (setq item2 (read inf))		;number of character classes (columns)
    (prt tabl-f "\t.psect next_action pic,usr,ovr,rel,gbl,"
	 "shr,noexe,rd,wrt,novec,long")
    (prt code-f "\tcommon (next_action) byte next_action(0 to "
	 (1- item) ", 0 to " (1- item2) ")")
    (dotimes (i item)
      (dotimes (j item2)
	(prt tabl-f "\t.byte " (read inf)))
      (terpri tabl-f))
    (terpri tabl-f)
    (terpri code-f)
    (prt tabl-f "\t.end")
    (princ "done\n")))
