;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; sstg_to_c.lsp -- convert SSTG (simple scanner table generator) output to C
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Author:   T. Kurt Bond
;;; Date:     1986 or 1987?
;;;
;;; Description:
;;;
;;; This program converts the intermediate file produced by SSTG.LSP
;;; into two C source files, one which contains #define'd constants
;;; and one which contains initialized tables for driving a lexical
;;; analyser.
;;;
;;; To use the program, load it into XLISP and give the following
;;; command:
;;;        (sstg-to-c "inputfilename" "codefilename" "tablefilename")
;;;
;;; A terse description of the format of the intermediate file
;;; follows.  Items in angle brackets are data; items starting with a
;;; colon are directives.  Alternate forms are preceded by a single
;;; upright bar, and refer to the entire line.
;;;
;;;---------------------------------------------------------------------------
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
;;; This program is *very* sensitive to data format.  The input *must*
;;; correct or it runs in an infinite loop.  Since the input file is
;;; supposed to be produced *only* by SSTG.LSP (function gen-scantab),
;;; this should not be a problem.
;;;
;;; This program was written while I was learning LISP, so some of the
;;; code is not ideal.  It was written using XLISP 1.7, but shouldn't be to
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

(defun sstg-to-c (input-file-name code-file-name table-file-name)
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
    (pr tabl-f "/* " table-file-name)
    (pr code-f "/* " code-file-name)
    (prt both " -- made from the file " (read inf)
	 " (by way of " input-file-name ") */")
    (terpri tabl-f)
    (terpri code-f)
    (princ "done\n")))

;;process :classes
(defun process-classes (inf tabl-f code-f both)
  (let (item item2)
    (princ "Processing :CLASSES...")
    (read inf)				;ignore :classes
    (prt code-f "/* Character Classes */")
    (setq item (read inf))		;first classname
    (do ((i 0 (1+ i))) ((eq item :chartoclass))
      (setq item2 (read inf))
      (pr code-f "#define " item " (" item2 ")\n")
      (setq item (read inf)))
    (terpri code-f)
    (princ "done\n")))


;;process :chartoclass
(defun process-chartoclass (inf tabl-f code-f both)
  (let (item item2)
    (princ "Processing :CHARTOCLASS...")
    (setq item2 (read inf))		;length of :chartoclass
    (prt tabl-f "/* Array for translating characters to character classes */")
    (prt tabl-f "short char_to_class[" item2 "] = {") 
    (terpri code-f)
    (do ((i 0 (1+ i))) ((>= i item2))
      (setq item (read inf))
      (read inf)			;get rid of integer
      (prt tabl-f "    " item ","))
    (prt tabl-f "};")
    (terpri tabl-f)))

;;process :actions
(defun process-actions (inf tabl-f code-f both)
  (let (item item2)
    (read inf)				;ignore :actions
    (prt code-f "/* Action Codes */")
    (setq item (read inf))		;first action
    (do ((i 0 (1+ i))) ((eq item :tokens))
      (setq item2 (read inf))
      (prt code-f "#define " item " (" item2 ")")
      (setq item (read inf)))
    (terpri code-f)    
    (princ "done\n")))

;;process :tokens
(defun process-tokens (inf tabl-f code-f both)
  (let (item item2)
    (princ "Processing :TOKENS...")
    (prt code-f "/* Token Codes */")
    (setq item (read inf))		;first token
    (do ((i 0 (1+ i))) ((eq item :returns))
      (setq item2 (read inf))
      (prt code-f "#define " item " (" item2 ")")
      (setq item (read inf)))
    (terpri code-f)
    (princ "done\n")))

;;process :returns
(defun process-returns (inf tabl-f code-f both)
  (let (item item2)
    (princ "Processing :RETURNS...")
    (setq item2 (read inf))		;length of :returns
    (prt tabl-f "/* Array for translating states to return token codes */")
    (prt tabl-f "short state_to_tc[" item2 "] = {")
    (do ((i 0 (1+ i))) ((>= i item2))
      (setq item (read inf))
      (read inf)			;get rid of integer
      (prt tabl-f "    " item ","))
    (prt tabl-f "};")
    (terpri tabl-f)
    (princ "done\n")))

;;process :errors
(defun process-errors (inf tabl-f code-f both)
  (let (item item2)
    (princ "Processing :ERRORS...")
    (read inf)				;get rid of :errors
    (setq item (read inf))		;length of errors table
    (prt tabl-f "/* Lexical error message strings */")
    (prt tabl-f "char *state_error[" item "] = {")
    (do ((i 0 (1+ i))) ((>= i item))
      (princ "    " tabl-f)
      (prin1 (read inf) tabl-f)
      (princ ",\n" tabl-f)
      (read inf))			;get rid of state number
    (prt tabl-f "};")
    (terpri tabl-f)
    (princ "done\n")))

;;process :start
(defun process-start (inf tabl-f code-f both)
  (let (item item2)
    (princ "Processing :START...")
    (read inf)				;get rid of :start
    (prt code-f "/* Start state */")
    (setq item (read inf))
    (cond  ((not (eq item :none))
	    (princ "(exists)...")
	    (prt code-f "#define " item " (" (read inf) ")"))
	   (t
	    (princ "(does not exist)...")))
    (terpri code-f)
    (princ "done\n")))

;;process :accept if it exists
(defun process-accept (inf tabl-f code-f both)
  (let (item item2)
    (princ "Processing :ACCEPT...")
    (setq item (read inf))		;get rid of :accept
    (prt code-f "/* Accept State */")
    (setq item (read inf))
    (cond ((not (eq item :none))
	   (princ "(exists)...")
	   (prt code-f "#define " item " (" (read inf) ")"))
	  (t
	   (princ "(does not exist)...")))
    (terpri tabl-f)
    (terpri code-f)
    (princ "done\n")))

;;process :goto-matrix
(defun process-goto-mat (inf tabl-f code-f both)
  (let (item item2)
    (princ "Processing :GOTO-MATRIX...")
    (prt tabl-f "/* Goto matrix (next state transitions matrix) */")
    (read inf)				;get rid of :goto-matrix
    (setq item (read inf))		;number of states (rows)
    (setq item2 (read inf))		;number of character classes (columns)
    (prt tabl-f "short nextstate[" item "][" item2 "] = {")
    (dotimes (i item)
      (dotimes (j item2)
	(prt tabl-f "    " (read inf) ","))
      (terpri tabl-f))
    (prt tabl-f "};")
    (terpri tabl-f)
    (princ "done\n")))
    
;;process :action-matrix
(defun process-act-mat (inf tabl-f code-f both)
  (let (item item2)
    (princ "Processing :ACTION-MATRIX...")
    (read inf)				;get rid of :action-matrix
    (prt tabl-f "/* Action Matrix (next action transitions matrix) */")
    (setq item (read inf))		;number of states (rows)
    (setq item2 (read inf))		;number of character classes (columns)
    (prt tabl-f "short nextaction[" item "][" item2 "] = {")
    (dotimes (i item)
      (dotimes (j item2)
	(prt tabl-f "    " (read inf) ","))
      (terpri tabl-f))
    (prt tabl-f "};")
    (terpri tabl-f)
    (princ "done\n")))
