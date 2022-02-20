;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; sstg-to-bas.scm -- convert SSTG output to VAX BASIC and VAX MACRO
;;;;                    SSTG == simple scanner table generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Author:   Thomas Kurt Bond
;;;
;;; Address:  AdminiSoft, Inc.                 Rt. 1, Box 74     
;;;           P.O. Box 789              or     Jane Lew, WV 26378
;;;           Buckhannon, WV 26201
;;;
;;; Description:
;;;
;;; This program converts the intermediate file produced by SSTG.SCM
;;; into a VAX BASIC source code file and a VAX MACRO source code
;;; file.  The VAX MACRO file, the `table file', defines the constants
;;; and the initialized tables for driving a lexical analyser (since
;;; VAX BASIC doesn't have any way to do this conviently and
;;; efficently).  The VAX BASIC file, the `code file', declares the
;;; constants as EXTERNAL, so their values in the VAX MACRO file are
;;; picked up at link time; it is intended to be used as an include
;;; file.
;;;
;;; To use the program, load it into Scheme and give the following
;;; command:
;;;        (sstg-to-bas "inputfilename" "codefilename" "tablefilename")
;;;
;;; A description of the input language follows at the end.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; global constants
(define tab (integer->char 9))
(define nl #\newline)
(define first-tc-value 25)		;token code values start with this

;;; global variables
(define creator "")
(define version "")
(define source "")

(define input-file-name "")
(define table-file-name "")
(define code-file-name "")

(define input-file #f)
(define table-file #f)
(define code-file #f)



;;; support procedures
(define (dt . args)
  (for-each (lambda (obj) (display obj table-file)) args))

(define (dc . args)
  (for-each (lambda (obj) (display obj code-file)) args))

(define (db . args)
  (for-each (lambda (obj)
	      (display obj table-file)
	      (display obj code-file))
	    args))

(define (rd)
  (read input-file))

;; do a function N times, passing an index that ranges from 0 to N-1 to FUN.
(define (dotimes n f)
  (letrec ((iter (lambda (i)
		   (cond ((< i n)
			  (f i)
			  (iter (+ i 1)))))))
    (iter 0)))



;;; The Programme
(define (sstg-to-bas input-fn code-fn table-fn)
  (set! input-file-name input-fn)
  (set! table-file-name table-fn)
  (set! code-file-name code-fn)

  (set! input-file (open-input-file input-fn))
  (set! table-file (open-output-file table-fn))
  (set! code-file  (open-output-file code-fn))

  (main-iter (list process-creator process-version process-source
		   process-classes process-chartoclass process-actions
		   process-tokens process-returns process-errors process-start
		   process-accept process-goto-matrix process-action-matrix))
  (close-input-port input-file)
  (close-output-port table-file)
  (close-output-port code-file)
  'done)

(define (main-iter divisions)
  (cond ((null? divisions)
	 (dt tab ".end") #t)
	(((car divisions))
	 (main-iter (cdr divisions)))
	(else
	 (error "boom.  A division didn't work: " (car divisions))
	 #f)))

(define (process-creator)
  (let ((tag (rd)))
    (cond ((not (eq? tag ':creator))
	   (error tag " when :creator expected")
	   #f)
	  (else
	   (set! creator (rd))
	   #t))))

(define (process-version)
  (let ((tag (rd)))
    (cond ((not (eq? tag ':version))
	   (error tag " when :version expected")
	   #f)
	  (else
	   (set! version (rd))
	   #t))))

(define (process-source)
  (let ((tag (rd)))
    (cond ((not (eq? tag ':source))
	   (error tag " when :source expected")
	   #f)
	  (else
	   (set! source (rd))
	   (dt ";!> " table-file-name)
	   (dc tab "!> " code-file-name)
	   (db " -- made by " creator " " version " from " source
	       " by way of " input-file-name nl)
	   #t))))

(define (dc-cont i)
  (if (= i 0)
      (dc tab tab)
      (dc ", &" nl tab tab)))

(define (process-classes)
  (let ((tag (rd))
	(len 0))
    (cond
     ((not (eq? tag ':classes))
      (error tag " when :classes expected")
      #f)
     (else
      (set! len (rd))
      (dt "; ")
      (dc tab "! ")
      (db "character classes" nl nl)
      (dc tab "external byte constant &" nl)
      (dotimes len (lambda (i)
		     (let* ((name (rd))
			    (value (rd)))
		       (dt name " == " value nl)
		       (dc-cont i)
		       (dc name))))
      (db nl)
      (dc nl)
      #t))))

(define (process-chartoclass)
  (let ((tag (rd))
	(len 0))
    (cond
     ((not (eq? tag ':chartoclass))
      (error tag " when :chartoclass expected")
      #f)
     (else
      (set! len (rd))
      (dt "; ")
      (dc tab "! ")
      (db "Array for translating characters to character classes" nl)
      (dt tab ".psect char_to_class pic,usr,ovr,rel,gbl,"
	  "shr,noexe,rd,wrt,novec,long" nl)
      (dc tab "common (char_to_class) byte char_to_class(0 to "
	  (- len 1) ")" nl nl)
      (dotimes len (lambda (i)
		     (let* ((name (rd))
			    (value (rd)))
		       (dt tab ".byte " name nl))))
      (db nl)
      #t))))

(define (process-actions)
  (let ((tag (rd))
	(len 0))
    (cond
     ((not (eq? tag ':actions))
      (error tag " where expected :actions")
      #f)
     (else
      (set! len (rd))
      (dt "; ")
      (dc tab "! ")
      (db "Action Codes" nl)
      (dc tab "external byte constant &" nl)
      (dotimes len (lambda (i)
		     (let* ((name (rd))
			    (value (rd)))
		       (dt name " == " value nl)
		       (dc-cont i)
		       (dc name))))
      (db nl)
      (dc nl)
      #t))))

;; Tokens must be in increasing order, numbered from 0,
;; and sequential without gaps.
(define (process-tokens)
  (let ((tag (rd))
	(len 0)
	(token-list '()))
    (cond
     ((not (eq? tag ':tokens))
      (error tag " when expecting :tokens")
      #f)
     (else
      (set! len (rd))
      (dt "; ")
      (dc tab "! ")
      (db "Token Codes" nl)
      (dc tab "external byte constant &" nl)
      (dt "first_tc_value == " first-tc-value nl)
      (dotimes len (lambda (i)
		     (let* ((name (rd))
			    (value (rd)))
		       (set! token-list
			     (append token-list (list (cons name value))))
		       (dt name " == " (+ first-tc-value value) nl)
		       (dc-cont i)
		       (dc name))))
      (db nl)
      (dc nl)
      (dt "tc_str_max == " (+ (- first-tc-value 1) len) nl)
      (dotimes len (lambda (i) (dt "TCS_" i ":" tab ".ascid /"
				   (car (list-ref token-list i)) "/" nl)))
      (dt "TCS_" len ":" tab ".ascid /erroneous token/" nl)
      (dt "tc_str_array::" nl)
      (dotimes (+ 1 len) (lambda (i) (dt tab ".long" tab "TCS_" i nl)))
      (dt nl nl)
      #t))))

(define (process-returns)
  (let ((tag (rd))
	(len 0))
    (cond
     ((not (eq? tag ':returns))
      (error tag " when expecting :returns")
      #f)
     (else
      (set! len (rd))
      (dt "; ")
      (dc tab "! ")
      (db "Array for translating states to return token codes" nl)
      (dt tab ".psect return_codes pic,usr,ovr,rel,gbl,"
	  "shr,noexe,rd,wrt,novec,long" nl)
      (dc tab "common (return_codes) byte return_codes(0 to "
	  (- len 1) ")" nl)
      (dotimes len (lambda (i)
		     (let* ((name (rd))
			    (value (rd)))
		       (dt tab ".byte " name nl))))
      (db nl)
      #t))))

(define (process-errors)
  (let ((tag (rd))
	(len 0))
    (cond
     ((not (eq? tag ':errors))
      (error tag " when expecting :errors")
      #f)
     (else
      (set! len (rd))
      (dt "; Lexical error message strings" nl)
      (dotimes len (lambda (i)
		     (let* ((name (rd)) (value (rd)))
		       (dt "LXM_" i ":" tab ".ascid /" name "/" nl))))
      (dt "LXM_" len ":" tab
	  ".ascid /internal error: invalid leical error message number/" nl)
      (dt "lex_mess_max == " len nl)
      (dt nl)
      (dt ";Lexical error message vector table" nl)
      (dt "lex_messages::" nl)
      (dotimes (+ 1 len) (lambda (i) (dt tab ".long LXM_" i nl)))
      (dt nl)
      #t))))

(define (process-start)
  (let ((tag (rd)))
    (cond
     ((not (eq? tag ':start))
      (error tag " when expecting :start")
      #f)
     (else
      (dt "; ")
      (dc tab "! ")
      (db "Start State" nl)
      (let* ((name (rd))
	     (value (rd)))
	(dt name " == " value nl)
	(dc tab "external byte constant &" nl tab tab name nl)
	(db nl))
      #t))))

(define (process-accept)
  (let ((tag (rd)))
    (cond
     ((not (eq? tag ':accept))
      (error tag " when expecting :accept")
      #f)
     (else
      (dt "; ")
      (dc tab "! ")
      (db "Accept State" nl)
      (let* ((name (rd))
	     (value (rd)))
	(dt name " == " value nl)
	(dc tab "external byte constant &" nl tab tab name nl)
	(db nl))
      #t))))

(define (process-goto-matrix)
  (let ((tag (rd)))
    (cond
     ((not (eq? tag ':goto-matrix))
      (error tag " when expecting :goto-matrix"))
     (else
      (let* ((num-rows (rd))
	     (num-cols (rd)))
	(dt "; ")
	(dc tab "! ")
	(db "Goto matrix -- matrix of state transitions" nl)
	(dt tab ".psect next_state pic,usr,ovr,rel,gbl,"
	    "shr,noexe,rd,wrt,novec,long" nl)
	(dc tab "common (next_state) byte next_state(0 to "
	    (- num-rows 1) ", 0 to " (- num-cols 1) ")" nl)
	(dotimes num-rows
		 (lambda (i)
		   (dotimes num-cols (lambda (j)
				       (let ((value (rd)))
					 (dt tab ".byte " value nl))))
		   (dt nl)))
	(db nl)
	(dt nl)
	#t)))))

(define (process-action-matrix)
  (let ((tag (rd)))
    (cond
     ((not (eq? tag ':action-matrix))
      (error tag " when expecting :action-matrix"))
     (else
      (let* ((num-rows (rd))
	     (num-cols (rd)))
	(dt "; ")
	(dc tab "! ")
	(db "Action matrix -- matrix of actions for (state, class) pairs" nl)
	(dt tab ".psect next_action pic,usr,ovr,rel,gbl,"
	    "shr,noexe,rd,wrt,novec,long" nl)
	(dc tab "common (next_action) byte next_action(0 to "
	    (- num-rows 1) ", 0 to " (- num-cols 1) ")" nl)
	(dotimes num-rows
		 (lambda (i)
		   (dotimes num-cols (lambda (j)
				       (let ((value (rd)))
					 (dt tab ".byte " value nl))))
		   (dt nl)))
	(db nl)
	#t)))))
      
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input Language Description
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 0. INTRODUCTION
;;;
;;; Items in angle brackets are data; items starting with a colon are
;;; directives.  Alternate forms are preceded by a single upright bar,
;;; and refer to the entire line.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 1.  LANGUAGE DESCRIPTION
;;;
;;; :creator <string>			;program that created this file
;;;
;;; :version <string>			;of the creator
;;;
;;; :source <string>			;source file that produced this output
;;;
;;; :classes <length>                   ;character class constant definitions
;;;     <classname> <value>
;;;
;;; :chartoclass <length>               ;character to character class mapping
;;;     <classname> <char-int-value>
;;;    
;;; :actions <length>                   ;action constant definitions
;;;     <action> <value>
;;;
;;; :tokens <length>                            ;token constant definitions
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
;;; 
;;; :accept                             ;accept state constant definition
;;;     <accept-name> <state-num>
;;;
;;; ;table of state transitions for each (state, character class) pair
;;; :goto-matrix <rows number-of-states> <columns number-of-character-classes>
;;;     <goto>				;1st row, 2nd row, etc.
;;;
;;; ;table of actions for each (state, character class) pair
;;; :action-matrix <rows num-of-states> <columns num-of-character-classes>
;;;     <action>			;1st row, 2nd row, etc.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This program is *very* sensitive to data format.  The input *must*
;;; correct or it runs in an infinite loop.  Since the input file is
;;; supposed to be produced *only* by SSTG.SCM (function
;;; gen-scantab), this should not be a problem.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
