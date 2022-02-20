;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; sstg.scm -- simple scanner table generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Author:   T. Kurt Bond
;;; Date:     1991?
;;;
;;; Description:
;;;
;;; This program reads a description of a Finite State Machine and
;;; constructs from it the tables needed to drive a lexical analyser.
;;; The tables are written to a file in a form that another program
;;; can read easily and turn into code in a particular language, such
;;; as C.  It is intended for use if a more sophisticated lexical
;;; analyser generator is not available.
;;;
;;; To use the program, load this file into a Scheme and issue the
;;; command:
;;;     (gen-scantab "inputscannerfile" "outputtablefile")
;;; which creates the file OUTPUTTABLEFILE, which contains the scanner
;;; table in a format that can be easily read by another program and
;;; converted to a particular language.
;;;
;;; Alternately, define a command like:
;;;     $ sstg :== 'scm' 'sstg_dir'sstg.scm
;;; where SCM is a DCL symbol for the foriegn command that runs SCM and 
;;; SSTG_DIR is a logical name defined as the directory where sstg.scm
;;; resides.  Then run:
;;;     $ sstg inputscannerfile outputtablefile
;;;
;;; The original version of this program was written using XLISP 1.7.
;;; This version was completely rewritten in Scheme.  This program
;;; assumes the underlying character scheme is ASCII.
;;; 
;;; The one of the main reasons it was written in LISP is so it could
;;; use the LISP reader instead of writing a scanner and parser for
;;; the description language (after all, *it* was written to help
;;; write scanners), which also explains the reasons for the
;;; syntax of the description language.  Of course, this also means
;;; that error reporting is rather primitive.
;;;
;;; A terse description of the input and output formats follows at the end
;;; of this file.  The scanner table generator generates a generic output
;;; file rather than code in a specific language; that way it can be easily
;;; adapted for any language using a post-processor.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous Global variables 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define program-name "sstg")		;name: simple scanner table generator
(define program-version "2.0")		;version number of scanner
(define tab (integer->char 9))		;since #\tab isn't portable



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (message . args)
  (for-each display args)
  (newline))

(define debugging-message #f)

(define (dbg . args) #f)

(if debugging-message
    (set! dbg
	  (lambda args
	    (display "debug: ")
	    (for-each display args)
	    (newline))))

(define (print-error . args)
  (apply message (cons "continuable error: " args)))

(define (expand-from-to start end)
  (define (iter acc start end)
    (if (<= start end)
	(iter (cons start acc) (+ start 1) end)
	acc))
  (iter '() start end))

(define (append-at-end values newval)
  (append values (list newval)))

;; 2d matrix using lists, so the number of rows can increase
(define (m-ref matrix row col)
  (dbg "m-ref: row " row " col " col ;" matrix " matrix
       )
  (vector-ref (list-ref matrix row) col))

(define (m-set! matrix row col val)
  (dbg "m-set!: row " row " col " col ;" matrix " matrix
       " val " val)
  (vector-set! (list-ref matrix row) col val))

;(if (not (bound? 'error))
;    (set! error (lambda args
;		  (display "fatal error: ")
;		  (for-each display args)
;		  (newline)
;		  (car 1))))

;; do a function N times, passing an index that ranges from 0 to N-1 to FUN.
(define (dotimes n f)
  (letrec ((iter (lambda (i)
		(cond
		 ((< i n)
		  (f i)
		  (iter (+ i 1)))))))
    (iter 0)))
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The Programme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gen-scantab in-name out-name)
  (dbg "in: " in-name)
  (dbg "out: " out-name)
  (let ((in-port (open-input-file in-name)))
    (if (process-dfsm in-name in-port)
	(let ((out-port (open-output-file out-name)))
	  (output-dfsm out-name out-port in-name)
	  (close-output-port out-port)
	  (close-input-port in-port)))))

   
(define (process-dfsm in-name in-port)
  (define (process-dfsm-iter proglist obj)
    (dbg "proglist: " proglist)
    (dbg "obj: " obj)
    (cond
     ((null? proglist)
      #t)				;all sections processed
     ((eof-object? obj)
      (error "Unexpected end of file")
      #f)				;error, unexpected end-of-file
     (((car proglist) obj)
      ;;section processed, do next
      (process-dfsm-iter (cdr proglist) (read in-port)))
     (else
      ;;error, unable to process section
      (print-error "Unable to process " obj)
      #f)))

  (dbg "in-name: " in-name)
  (dbg "in-port: " in-port)
  (process-dfsm-iter
   (list lex-classes lex-actions lex-tokens lex-states lex-start lex-accept)
   (read in-port)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Character Class Routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define char-set-length 128)
(define character-classes (make-vector char-set-length #f))
(define classes-list '())
(define number-of-classes 0)

(define (lex-classes obj)
  (dbg "lex-classes: " obj)
  (set! character-classes (make-vector char-set-length #f))
  (set! classes-list '())
  (set! number-of-classes 0)
  (for-each process-class (cdr obj))
  (set! classes-list (reverse classes-list))
  #t)

(define (process-class class)
  (let ((name (car class)))
    (define (iter b e)
      (cond ((<= b e)
	     (if (not (vector-ref character-classes b))
		 (vector-set! character-classes b name))
	     (iter (+ b 1) e))))
    (define (process-specifier spec)
      (cond ((char? spec)		;deal with single char
	     (vector-set! character-classes (char->integer spec) name))
	    ((integer? spec)		;deal with single integer char code
	     (vector-set! character-classes spec name))
	    ((eq? spec 'special)	;for EOF and the like
	     )				;do nothing
	    ((eq? spec 'others)		;for all unset characters
	     (iter 0 (- char-set-length 1))) ;zero-based
	    ((list? spec)		;(#\a #\z)
	     (iter (if (char? (car spec))
		       (char->integer (car spec))
		       (car spec))
		   (if (char? (cadr spec))
		       (char->integer (cadr spec))
		       (cadr spec))))
	    (else
	     (error "unable to understand character specifier " spec))))

    (set! classes-list (cons (list name number-of-classes) classes-list))
    (set! number-of-classes (+ 1 number-of-classes))
    (for-each process-specifier (cdr class))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Action and Token Routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define actions-list '())
(define number-of-actions 0)

(define (lex-actions obj)
  (dbg "lex-actions: " obj)
  (set! actions-list '())
  (set! number-of-actions 0)
  (for-each process-action (cdr obj))
  (set! actions-list (reverse actions-list))
  #t)

(define (process-action action)
  (set! actions-list (cons (list action number-of-actions) actions-list))
  (set! number-of-actions (+ 1 number-of-actions)))


(define tokens-list '())
(define number-of-tokens 0)

(define (lex-tokens obj)
  (dbg "lex-tokens: " obj)
  (set! tokens-list '())
  (set! number-of-tokens 0)
  (for-each process-token (cdr obj))
  (set! tokens-list (reverse tokens-list))
  #t)

(define (process-token token)
  (set! tokens-list (cons (list token number-of-tokens) tokens-list))
  (set! number-of-tokens (+ 1 number-of-tokens)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; State Routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define states-list '())		;list of (state-name state-num) pairs
(define number-of-states 0)		;value of state-name for next state
(define states-goto '())		;matrix of goto transitions
(define states-action '())		;matrix of actions
(define states-return '())		;list of token codes to return
(define states-error '())		;list of (state-name err-message) pairs
(define states-default-error-message	;default error message
  "improper action")

(define (lex-states obj)
  (dbg "lex-states: obj " obj)
  (set! states-list '())
  (set! number-of-states 0)
  (set! states-goto '())
  (set! states-action '())
  (set! states-error '())
  (for-each process-state (cdr obj))
  #t)

(define (check-state-name state-name)
  (dbg "check-state-name: state-name " state-name)
  (cond ((not (or (symbol? state-name) (number? state-name)))
	 (print-error "state name not number of symbol: " state-name)
	 #f)
	((assoc state-name states-list)
	 (print-error "state name already defined: " state-name)
	 #f)
	(else #t)))

(define (check-return-name return-name)
  (dbg "check-return-name: return-name " return-name)
  (cond ((assoc return-name tokens-list) #t)
	(else
	 (print-error "unknown return name: " return-name)
	 #f)))

(define (check-err-message msg)
  (dbg "check-err-message: msg " msg)
  (cond ((or (null? msg) (string? msg) (eq? msg 'nil)) #t)
	(else
	 (print-error "illegal error message: " msg)
	 #f)))
      

(define (process-state state)
  (dbg "process-state: state " state)
  (let ((state-name (car state))
	(return-name (cadr state))
	(err-message (caddr state))
	(transition-list (cdddr state))
	(state-num -1))
    (cond
     ((not (check-state-name state-name))
      (print-error state-name " is not a valid state name"))
     (else
      (set! states-list (append-at-end states-list
				       (list state-name number-of-states)))
      (set! state-num number-of-states)
      (set! number-of-states (+ 1 number-of-states))
      (set! states-goto (append-at-end states-goto
				       (make-vector number-of-classes #f)))
      (set! states-action (append-at-end states-action
					 (make-vector number-of-classes #f)))
      (cond
       ((check-return-name return-name)
	(set! states-return (append-at-end states-return
					   (list state-num return-name))))
       (else
	(print-error return-name " is not a valid return name")))
      (cond
       ((check-err-message err-message)
	(set! states-error (append-at-end
			    states-error
			    (list
			     state-num
			     (if (or (null? err-message)
				     (not err-message)
				     (eq? err-message 'nil))
				 states-default-error-message
				 err-message)))))
       (else
	(print-error err-message " is not a valid error message")))
      (process-transitions state-name state-num transition-list)))))

(define (process-transitions st-name st-num trans-list)
  (define (do-tran tran)
    (let ((cc (car tran))
	  (ac (cadr tran))
	  (gt (if (<= 3 (length tran)) (caddr tran) '())))
      (define (do-item item)
	(if (eq? item 'others)
	    (set-other-trans st-name st-num ac gt)
	    (set-state-trans st-name st-num ac gt item)))
      (if (not (list? cc))
	  (set! cc (list cc)))
      (for-each do-item cc)))

  (dbg "process-transitions: st-name " st-name
       " st-num " st-num
       " trans-list " trans-list)
  (for-each do-tran trans-list))

(define (set-other-trans st-name st-num ac gt)
  (dbg "set-other-trans: st-name " st-name
       " st-num " st-num
       " ac " ac " gt " gt)
  (dotimes number-of-classes
	   (lambda (i)
	     (cond
	      ((not (m-ref states-goto st-num i))
	       (m-set! states-goto st-num i gt)
	       (m-set! states-action st-num i ac))))))

(define (set-state-trans st-name st-num ac gt item)
  (dbg "set-state-trans: st-name " st-name
       " st-num " st-num
       " ac " ac " gt " gt " item " item)
  (let ((character-class (assoc item classes-list)))
    (dbg "character-class " character-class)
    (cond
     ((not character-class)
      (print-error item " is not a valid character class (state " st-name ")"))
     (else
      (let ((cc-num (cadr character-class)))
	(dbg "cc-num " cc-num)
	(if (not (m-ref states-goto st-num cc-num))
	    (m-set! states-goto st-num cc-num gt)
	    (print-error item " already has a goto in state " st-name))
	(if (not (m-ref states-action st-num cc-num))
	    (m-set! states-action st-num cc-num ac)
	    (print-error item " already has an action in state " st-name)))))))


(define start-state '())

(define (lex-start obj)
  (dbg "lex-start: " obj)
  (let ((entries (cdr obj)))
    (set! start-state (list
		       (if (and (= (length entries) 2)
				(symbol? (cadr entries)))
			   (cadr entries)
			   'start_state)
		       (cadr (assoc (car entries) states-list)))))
  #t)


(define accept-state '())

(define (lex-accept obj)
  (dbg "lex-accept: " obj)
  (let ((entries (cdr obj)))
    (set! accept-state (list
		       (if (and (= (length entries) 2)
				(symbol? (cadr entries)))
			   (cadr entries)
			   'accept_state)
		       (cadr (assoc (car entries) states-list)))))
  #t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Process output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (output-dfsm out-name out-port in-name) 
  (define (writeln . args)
    (for-each (lambda (obj) (display obj out-port)) args)
    (newline out-port))
  (dbg "out-name " out-name " out-port " out-port " in-name "  in-name)
  (message "writing output...")

  (display "creator...")
  (writeln ":creator  " #\" program-name #\")
  (writeln)
  (message "done")

  (display "version...")
  (writeln ":version  " program-version)
  (writeln)
  (message "done")

  (display "source...")
  (writeln ":source  " #\" in-name #\")
  (writeln)
  (message "done")

  (display "classes...")
  (writeln ":classes  " number-of-classes)
  (for-each (lambda (class)
	      (writeln tab (car class) "  " (cadr class)))
	    classes-list)
  (writeln)
  (message "done")

  (display "chartoclass...")
  (writeln ":chartoclass  " char-set-length)
  (dotimes char-set-length
	   (lambda (i) (writeln tab (vector-ref character-classes i) "  " i)))
  (writeln)
  (message "done")

  (display "actions...")
  (writeln ":actions  " number-of-actions)
  (for-each (lambda (action)
	      (writeln tab (car action) "  " (cadr action)))
	    actions-list)
  (writeln)
  (message "done")

  (display "tokens...")
  (writeln ":tokens  " number-of-tokens)
  (for-each (lambda (token)
	      (writeln tab (car token) "  " (cadr token)))
	    tokens-list)
  (writeln)
  (message "done")

  (display "returns...")
  (writeln ":returns  " number-of-states)
  (dotimes number-of-states (lambda (i)
			      (writeln tab
				       (cadr (assoc i states-return))
				       "  "
				       i)))
  (writeln)
  (message "done")

  (display "errors...")
  (writeln ":errors  " number-of-states)
  (dotimes number-of-states (lambda (i)
			      (writeln tab
				       #\"
				       (cadr (assoc i states-error))
				       #\"
				       "  "
				       i)))
  (writeln)
  (message "done")

  (display "start...")
  (writeln ":start  "  (car start-state) "  "  (cadr start-state))
  (writeln)
  (message "done")

  (display "accept...")
  (writeln ":accept  " (car accept-state) "  " (cadr accept-state))
  (writeln)
  (message "done")

  (display "goto-matrix...")
  (writeln ":goto-matrix  " number-of-states "  " number-of-classes)
  (for-each
   (lambda (state)
     (dotimes number-of-classes
	      (lambda (i)
		(writeln tab
			 (cadr (assoc (vector-ref state i) states-list)))))
     (newline out-port))
   states-goto)
  (writeln)
  (message "done")

  (display "action-matrix...")
  (writeln ":action-matrix  " number-of-states "  " number-of-classes)
  (for-each
   (lambda (actions)
     (dotimes number-of-classes
	      (lambda (i)
		(writeln tab
			 (vector-ref actions i))))
     (newline out-port))
   states-action)
  (writeln)
  (message "done")

  (display "Output written."))
				      


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; If arguments were specified to SCM on the command line, assume
;;; they are the input scanner file and the output table file and run
;;; gen-scantab on them, then exit.  Otherwise, just let the user do
;;; things interactively.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (= 4 (length (program-arguments)))
    (begin
      ;; We've got the input scanner file and the output table file.
      (let ((scnfile (caddr (program-arguments)))
            (tabfile (cadddr (program-arguments))))
        (display "scnfile: ")
        (write scnfile)
        (display " tabfile: ")
        (write tabfile)
        (newline)
        (gen-scantab scnfile tabfile)
        (quit))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input Language Description
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 0.  INTRODUCTION
;;;
;;; Input should appear in the same order as it is described.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 1.  CLASSES
;;;
;;; Character Classes are defined as follows:
;;; 
;;;     (classes <class-list>)
;;; 
;;; where <class-list> is one or more <class>s, and
;;; <class> is
;;; 
;;;     (<class-name> <char-list>)
;;; 
;;; where <class-name> is a symbol and <char-list> is either the
;;; symbol `others', the symbol `special', or one or more characters,
;;; small integers (ASCII codes), and/or lists of length two whose car
;;; is the beginning character of a sequence and whose caddr is the
;;; ending character of that sequence.
;;; 
;;; Example:
;;;     (classes
;;;       (LT_LETTER (#\a #\z) (#\A #\Z) #\_)
;;;       (LT_SPACE  32 13 12 9)	;blank, return, formfeed, tab
;;; 	  (LT_UNDEFINED others))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 2.  ACTIONS
;;;
;;; Actions are defined as follows:
;;; 
;;;     (actions <action-list>)
;;; 
;;; where <action-list> is a list of symbols that will name the
;;; various actions.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 3.  TOKEN NAMES
;;;
;;; A token name is a symbol which names the type of token the scanner
;;; has returned to its caller.  SSTG automatically generates a
;;; numeric code for each token name.  Token names are defined as
;;; follows:
;;; 
;;;     (tokens <token-list>)
;;; 
;;; where <token-list> is a list of symbols that will name the various
;;; tokens.
;;;
;;; Example:
;;;     (tokens TC_IF TC_ELSE TC_STRING TC_CHAR TC_INT)
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 4.  STATES
;;; 
;;; States are defined as follow:
;;; 
;;;     (states  <states-list>)
;;; 
;;; where <states-list> is a list of state instructions of the form
;;; 
;;;     (<state-name> <return-name> <error-message> <transition-list>)
;;; 
;;; <state-name> is a symbol or number naming the state, <return-name>
;;; is the name of the value to be returned if an accept happens. It
;;; must be one of the tokens defined in (tokens ...).
;;;
;;; <transition-list> has the form
;;; 
;;;     (<character-classes> <action> <goto>)
;;; 
;;; where <character-classes> is a character class or a list of
;;; character classes, <action> is one of the actions defined earlier
;;; in (actions ...)  and <goto> is omitted, NIL or a <state-name>.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 5.  START
;;;
;;; The START statement defines a state as the start state.  Its form
;;; is
;;;     (start <state-name> [<start-symbol>])
;;; where <start-symbol> is an optional symbol which will name the
;;; start state.  If <start-symbol> is omitted, the symbol START_STATE
;;; will be used.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 5.  ACCEPT
;;;
;;; The ACCEPT statement defines a state as the accept state.  Its form
;;; is
;;;     (accept <state-name> [<accept-symbol>])
;;; where <accept-symbol> is an optional symbol which will name the
;;; accept state.  If <accept-symbol> is omitted, the symbol ACCEPT_STATE
;;; will be used.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 6.  END
;;;
;;; The END statement tells SSTG that no more input is expected.  It
;;; is required.  Its form is
;;;     end
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Output Language Description
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



