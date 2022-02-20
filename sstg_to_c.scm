;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; sstg_to_c.scm -- convert SSTG (simple scanner table generator) output to C
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Author:   T. Kurt Bond
;;; Date:     Started 1991?  Finished 2022.
;;; Description:
;;;
;;; This program converts the intermediate file produced by SSTG.SCM
;;; into two C source files, one which contains #define'd constants
;;; and one which contains initialized tables for driving a lexical
;;; analyser.
;;;
;;; To use the program, load it into SCM and give the following
;;; command:
;;;     (sstg-to-c "inputtablefile" "codeheaderfile" "tableheaderfile")
;;; Alternately, define a DCL foreign command like:
;;;     $ sstg_to_c :== 'scm' 'sstg_dir'sstg_to_c.scm
;;; where SCM is a DCL foriegn command that runs SCM and SSTG_DIR is a 
;;; logical name defined as the directory where sstg_to_c.scm resides. 
;;; Then run:
;;;     $ sstg_to_c inputtablefile codeheaderfile tableheaderfile
;;;
;;; A terse description of the format of the input file is found at
;;; the end of this file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; global constants
(define program-name "sstg_to_c")       ; Name: simple scanner table generator
(define program-version "2.0")		; Version number of scanner

(define tab (integer->char 9))
(define nl #\newline)

;;; global variables
;; Info about creator of the input table file.
(define creator "")
(define version "")
(define source "")

(define input-table-filename "")
(define output-codes-filename "")
(define output-lextab-filename "")

(define input-table-file #f)
(define output-codes-file #f)
(define output-lextab-file #f)


;;; support procedures
(define (upcase s)
  (let* ((s (if (symbol? s) (symbol->string s) s))
         (l (string-length s))
         (t (make-string l)))
    (do ((i 0 (+ i 1)))
        ((= i l) t)
      (let* ((c (string-ref s i))
             (c (if (char-lower-case? c) (char-upcase c) c)))
        (string-set! t i c)))))

(define (dc . args)			; Display to codes file.
  (for-each (lambda (obj) (display obj output-codes-file)) args))

(define (wc . args)			; Write to codes file.
  (for-each (lambda (obj) (write obj output-codes-file)) args))

(define (dl . args)                     ; Display to lextab file.
  (for-each (lambda (obj) (display obj output-lextab-file)) args))

(define (wl . args)                     ; Write to lextab file.
  (for-each (lambda (obj) (write obj output-lextab-file)) args))

(define (rd)
  (read input-table-file))

;; do a function N times, passing an index that ranges from 0 to N-1 to FUN.
(define (dotimes n f)
  (letrec ((iter (lambda (i)
		   (cond ((< i n)
			  (f i)
			  (iter (+ i 1)))))))
    (iter 0)))



;;; The Programme
(define (sstg-to-c input-table-fn output-codes-fn output-lextab-fn)
  (set! input-table-filename input-table-fn)
  (set! output-codes-filename output-codes-fn)
  (set! output-lextab-filename output-lextab-fn)

  (set! input-table-file (open-input-file input-table-filename))
  (set! output-codes-file (open-output-file output-codes-filename))
  (set! output-lextab-file (open-output-file output-lextab-filename))

  (main-iter (list process-creator process-version process-source
		   process-classes process-chartoclass process-actions
		   process-tokens process-returns process-errors process-start
		   process-accept process-goto-matrix process-action-matrix))
  (close-input-port input-table-file)
  (close-output-port output-codes-file)
  (close-output-port output-lextab-file)
  'done)

(define (main-iter divisions)
  (cond ((null? divisions)
	 #t)
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
	   (dc "/* " output-codes-filename " -- made by " program-name " " program-version
	       " from " source " (by way of " input-table-filename " created by " creator " " version ") */" nl nl)
           (dl "/* " output-lextab-filename " -- made by " program-name " " program-version 
               " from " source " (by way of " input-table-filename " created by " creator " " version ") */" nl nl)
	   #t))))

(define (process-classes)
  (let ((tag (rd))
	(len 0))
    (cond
     ((not (eq? tag ':classes))
      (error tag " when :classes expected")
      #f)
     (else
      (set! len (rd))
      (dc "/* Character Classes */" nl)
      (dotimes len (lambda (i)
		     (let* ((name (rd))
			    (value (rd)))
		       (dc "#define " (upcase name) " (" value ")" nl))))
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
      (dl "/* Array for translating characters to character classes */" nl)
      (dl "short char_to_class[" len "] = {" nl)
      (dotimes len (lambda (i)
		     (let* ((name (rd))
			    (value (rd)))
		       (dl "    " (upcase name) "," nl))))
      (dl "};" nl nl)
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
      (dc "/* Action Codes */" nl)
      (dotimes len (lambda (i)
		     (let* ((name (rd))
			    (value (rd)))
		       (dc "#define " (upcase name) " (" value ")" nl))))
      (dc nl)
      #t))))

;; Tokens must be in increasing order, numbered from 0,
;; and sequential without gaps.
(define (process-tokens)
  (let ((tag (rd))
	(len 0))
    (cond
     ((not (eq? tag ':tokens))
      (error tag " when expecting :tokens")
      #f)
     (else
      (set! len (rd))
      (dc "/* Token Codes */" nl)
      (dotimes len (lambda (i)
		     (let* ((name (rd))
			    (value (rd)))
		       (dc "#define " (upcase name) " (" value ")" nl))))
      (dc nl)
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
      (dl "/* Array for translating states to return token codes */" nl)
      (dl "short state_to_tc[" len "] = {" nl)
      (dotimes len (lambda (i)
		     (let* ((name (rd))
			    (value (rd)))
		       (dl "    " (upcase name) "," nl))))
      (dl "};" nl)
      (dl nl)
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
      (dl "/* Lexical error message strings */" nl)
      (dl "char *state_error[" len "] = {" nl) 
      (dotimes len (lambda (i)
		     (let* ((string (rd)) (value (rd)))
		       (dl "    ")
                       (wl string)
                       (dl "," nl))))
      (dl "};" nl)
      (dl nl)
      #t))))

(define (process-start)
  (let ((tag (rd)))
    (cond
     ((not (eq? tag ':start))
      (error tag " when expecting :start")
      #f)
     (else
      (dc "/* Start state */" nl)
      (let* ((name (rd))
	     (value (rd)))
	(dc "#define " (upcase name) " (" value ")" nl))
      (dc nl)
      #t))))

(define (process-accept)
  (let ((tag (rd)))
    (cond
     ((not (eq? tag ':accept))
      (error tag " when expecting :accept")
      #f)
     (else
      (dc "/* Accept State */" nl)
      (let* ((name (rd))
	     (value (rd)))
	(dc "#define " (upcase name) " (" value ")" nl)
	(dc nl))
      #t))))

(define (process-goto-matrix)
  (let ((tag (rd)))
    (cond
     ((not (eq? tag ':goto-matrix))
      (error tag " when expecting :goto-matrix"))
     (else
      (let* ((num-rows (rd))
	     (num-cols (rd)))
	(dl "/* Goto matrix -- matrix of state transitions */" nl)
        (dl "short nextstate[" num-rows "][" num-cols "] = {" nl)
	(dotimes num-rows
		 (lambda (i)
		   (dotimes num-cols (lambda (j)
				       (let ((value (rd)))
					 (dl "    " value ","  nl))))
		   (dl nl)))
	(dl "};" nl)
	(dl nl)
	#t)))))

(define (process-action-matrix)
  (let ((tag (rd)))
    (cond
     ((not (eq? tag ':action-matrix))
      (error tag " when expecting :action-matrix"))
     (else
      (let* ((num-rows (rd))
	     (num-cols (rd)))
	(dl "/* Action matrix -- matrix of actions for (state, class) pairs */" nl)
        (dl "short nextaction[" num-rows "][" num-cols "] = {" nl)
	(dotimes num-rows
		 (lambda (i)
		   (dotimes num-cols (lambda (j)
				       (let ((value (rd)))
					 (dl "    " (upcase value) "," nl))))
		   (dl nl)))
        (dl "};")
	(dl nl)
	#t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; If arguments were specified to SCM on the command line, assume
;;; they are the input table file and the output codes file and
;;; lexical tables file and run sstg-to-c on them, then exit.
;;; Otherwise, just let the user do things interactively.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (= 5 (length (program-arguments)))
    (begin
      ;; We've got the input scanner file and the output table file.
      (let ((tabfile (caddr (program-arguments)))
            (codesfile (cadddr (program-arguments)))
            (lextabfile (car (cddddr (program-arguments)))))
        (display "tabfile: ")
        (write tabfile)
        (display " codesfile: ")
        (write codesfile)
        (display " lextabfile: ")
        (write lextabfile)
        (newline)
        (sstg-to-c tabfile codesfile lextabfile)
        (quit))))      
    

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
