;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sstg.lsp -- simple scanner table generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Author:   T. Kurt Bond
;;; Date:     1986 or 1987?
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
;;; To use the program, load this file into XLISP and issue the
;;; command: 
;;;        (gen-scantab "input.scn" "output.table")
;;; which creates the file OUTPUT.TABLE, which contains the scanner
;;; table in a format that can be easily read by another program and
;;; converted to a particular language.
;;;
;;; This program was written using XLISP 1.7.  If it is ported to a
;;; different LISP, changes will have to be made to at least `stringp'
;;; `pad-on-left', `princ-pad', and calls to `openi' and `openo'.
;;; Also, characters are written using the `#\c' syntax of Common
;;; LISP, but are internally represented as small integers (so #\a is
;;; the same as 97).  It is assumed that the character set being used
;;; is ASCII.
;;; 
;;; The main reason I wrote it in LISP is so I could use the LISP
;;; reader instead of writing a scanner and parser for the description
;;; language (after all, I wrote *it* to help me write a scanner),
;;; which also explains the reasons for the syntax of the description
;;; language.  Of course, this also means that error reporting is rather
;;; primitive. 
;;;
;;; This program was written while I was learning LISP, so some of the
;;; code may not be ideal.
;;;
;;; A terse description of the input and output formats follows.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
;;;     (classes <class-spec-list>)
;;; 
;;; where <class-spec-list> is one or more <class-spec>s, and
;;; <class-spec> is
;;; 
;;;     (<class-name> <char-spec-list>)
;;; 
;;; where <class-name> is a symbol and <char-spec-list> is either the
;;; symbol `others', the symbol `special' or one or more characters,
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misellaneous Global variables 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq scanner-version "1.1")		;version number of scanner
(setq program-name "scanner")		;name of this program/facility


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support Routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perror -- print a list of items, preceeded by "Error: " and folled by EOL
(defun perror (&rest body)
  (princ "Error: ")
  (dolist (item body)
    (princ item))
  (terpri))

;; pad-on-left -- given STR, return a string that is at least WIDTH wide
(defun pad-on-left (str width &optional pad-char)
  (if (null pad-char) (setq pad-char #\ ))
  (do ((i (flatc str) (1+ i)) (res str) (pad-str (string pad-char)))
      ((>= i width) res)
    (setq res (strcat pad-str res))))

;; princ-pad -- given ITEM, print it right justified in a field WIDTH wide
(defun princ-pad (item width &optional pad-char sink)
  (if (null pad-char) (setq pad-char #\ ))
  (if (null sink) (setq sink *standard-output*))
  (princ
   (do ((i (flatc item) (1+ i)) (res "") (pad-str (string pad-char)))
       ((>= i width) res)
     (setq res (strcat pad-str res)))
   sink)
  (princ item sink))

;; stringp -- Is the item a string?
(defun stringp (item)
  (eq (type-of item) :string))

;; add-at-end -- append a value to the end of a list and return the new list
(defun add-at-end (values newval)
  (append values (list newval)))

;; make-code-list -- given a list of codes, make an a-list of those codes and
;; internally assigned code numbers
(defun make-code-list (codes)
  (let (res (i 0))
    (dolist (item codes res)
      (cond ((assoc item res)
	     (perror item " has already been defined"))
	    ((not (symbolp item))
	     (perror item " is not a symbol"))
	    (t
	     (setq res (append res (list (list item i))))
	     (setq i (1+ i)))))))

;; expand-from-to -- Given two numbers, produce a list containing all the
;; numbers between those two numbers, inclusive.
(defun expand-from-to (from to)
  (do (res (i to (1- i))) ((< i from) res)
    (setq res (cons i res))))

;; fix-up-sequence -- given a list of integers and (from to) pairs, return list
;; containing the integers and the expansions of the (from to) pairs.
(defun fix-up-sequence (values)
  (let (res)
    (dolist (item values res)
      (cond ((numberp item)
	     (setq res (append res (list item))))
	    ((and (listp item) (= (length item) 2)
		  (numberp (car item))
		  (numberp (cadr item))
		  (<= (car item) (cadr item)))
	     (setq res (append res (expand-from-to (car item) (cadr item)))))
	    (t
	     (perror "not a sequence specification: " item))))))

;; make-list -- make a list N elements long.  Optional FILL tells what elements
;; start as.
(defun make-list (n &optional fill)
  (do (res (i 0 (1+ i)))
      ((>= i n) res)
    (setq res (cons fill res))))

;; make-matrix -- make an array ROW by COL, using lists.  
;; (need this because XLISP 1.7 doesn't have 2-d arrays)
(defun make-matrix (row col &optional fill)
  (do (res (i 0 (1+ i)))
      ((>= i row) res)
    (setq res (cons (make-list col fill) res))))

;; mref -- get the value of an element of a matrix produced by `make-matrix'.
(defmacro mref (matrix row col)
  `(nth ,col (nth ,row ,matrix)))

;; mset -- set the value of an element of a matrix produced by `make-matrix'.
(defmacro mset (matrix row col value)
  `(setf (nth ,col (nth ,row ,matrix)) ,value)) 




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Character Class Routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq char-set-length 128)		;number of characters in character set
;;array telling what class a character is in
(setq char-classes-vector (make-array char-set-length))
(setq char-classes-list '())		;list of (char-class code) pairs
(setq char-classes-next 0)		;char-class code of next class


(defun process-classes (spec-list)
  (let (name spec)
    (dolist (item spec-list)
      (setq name (car item))
      (setq spec (cdr item))
      (cond ((assoc name char-classes-list)
	     (perror "character class " name " already defined"))
	    (t
	     (setq char-classes-list
		   (add-at-end char-classes-list
			       (list name char-classes-next)))
	     (setq char-classes-next (1+ char-classes-next))
	     (cond ((eq (car spec) 'others)
		    (set-other-chars name))
		   ;;ignore special lex code names, because they aren't
		   ;;in the character array.
		   ((eq (car spec) 'special)
		    t)
		   (t
		    (set-chars name spec))))))))

(defun set-other-chars (name)
  (dotimes (i char-set-length)
    (if (not (aref char-classes-vector i))
	(setf (aref char-classes-vector i) name))))

(defun set-chars (name spec)
  (setq spec (fix-up-sequence spec))
  (dolist (i spec)
    (cond ((aref char-classes-vector i)
	   (perror "character " i " already defined as "
		   (aref char-classes-vector i)))
	  (t
	   (setf (aref char-classes-vector i) name)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Action and Token Routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq actions-list '())			;list of (action code) pairs
(setq actions-number 0)			;number of actions

(defun process-actions (actions)
  (setq actions-list (make-code-list actions))
  (setq actions-number (length actions-list)))



(setq tokens-list '())			;list of (token code) pairs
(setq tokens-number 0)			;number of token codes
(defun process-tokens (tokens)
  (setq tokens-list (make-code-list tokens))
  (setq tokens-number (length tokens-list)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; State Routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq states-list '())			;list of (state-name state-num) pairs
(setq states-next 0)			;value of state-name for next state
(setq states-goto '())			;matrix of goto transitions
(setq states-act '())			;matrix of actions
(setq states-return '())		;list of token codes to return 
(setq states-error '())			;list of (state-name err-message) pairs
(setq states-err-mess "improper action");default error message

(defun process-states (states)
  (dolist (state states)
    (add-state state)))

(defun check-state-name (state-name)
  (cond ((not (or (symbolp state-name) (numberp state-name)))
	 (perror "state name not number or symbol: " state-name)
	 nil)
	((assoc state-name states-list)
	 (perror "state name already defined: " state-name)
	 nil)
	(t
	 t)))

(defun check-return-name (return-name)
  (cond ((assoc return-name tokens-list) t)
	(t (perror "unknown return name: " return-name)
	   nil)))

(defun check-err-message (message)
  (cond ((or (null message) (stringp message))
	 t)
	(t (perror "illegal error message: " message)
	   nil)))

(defun add-state (state)
  (let ((state-name (car state))
	(return-name (cadr state))
	(err-message (caddr state))
	(transition-list (cdddr state))
	state-num)
    (cond ((not (check-state-name state-name))
	   (perror state-name " is not a valid state name"))
	  (t
	   (setq states-list (add-at-end states-list
					 (list state-name states-next)))
	   (setq state-num states-next)
	   (setq states-next (1+ states-next))
	   (setq states-goto (add-at-end states-goto
					 (make-list char-classes-next)))
	   (setq states-act  (add-at-end states-act
					 (make-list char-classes-next)))
	   (cond ((check-return-name return-name)
		  (setq states-return
			(add-at-end states-return
				    (list state-num return-name))))
		 (t
		  (perror return-name " is not a valid return name")))
	   (cond ((check-err-message err-message)
		  (setq states-error
			(add-at-end states-error
				    (list state-num (or err-message
							states-err-mess)))))
		 (t
		  (perror err-message " is not a valid error message")))
	   (process-transitions state-name state-num transition-list)))))

(defun process-transitions (st-name st-num trans-list) 
  (dolist (trans trans-list)
    (let ((cc (car trans)) (ac (cadr trans))
	  (gt (if (<= 3 (length trans)) (caddr trans) nil)))
      (if (not (listp cc))
	  (setq cc (list cc)))
      (dolist (item cc)
	(cond ((eq item 'others)
	       (set-other-trans st-name st-num ac gt))
	      (t
	       (set-state-trans st-name st-num ac gt item)))))))

(defun set-other-trans (st-name st-num ac gt) 
  (dotimes (i char-classes-next)
    (cond ((not (mref states-goto st-num i))
	   (mset states-goto st-num i gt)
	   (mset states-act  st-num i ac)))))

(defun set-state-trans (st-name st-num ac gt item)
  (let ((cc-num (cadr (assoc item char-classes-list))))
    (cond ((not cc-num)
	   (perror item " is not a valid character class (state " st-name ")"))
	  (t
	   (if (not (mref states-goto st-num cc-num))
	       (mset states-goto st-num cc-num gt)
	     (perror item " already has a goto in state " st-name))
	   (if (not (mref states-act st-num cc-num))
	       (mset states-act  st-num cc-num ac)
	     (perror item " already has a action in state " st-name))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Driver routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gen-scantab (source-file-name output-file-name)
  (let ((inf (openi source-file-name)))
    (catch 'scanner-top
      (do ((sexp (read inf) (read inf))) ((= 1 2))
	(cond ((not sexp)
	       (princ "Nothing to read, exiting...")
	       (terpri)
	       (throw 'scanner-top))	;exit if nothing to read
	      ((eq sexp 'end)		;exit if told by user
	       (throw 'scanner-top))
	      ((and (listp sexp) (directive-p sexp))
	       (process-sexp sexp))
	      (t
	       (perror "unrecognized top level directive " sexp)))))
    (output-plain source-file-name output-file-name)
    (princ "The End.")
    (terpri)))

(defun directive-p (sexp)
  (member (car sexp) '(classes actions tokens states start accept)))

(defun process-sexp (sexp)		;sexp is a list whose car is directive
  (princ "Processing ") (princ (car sexp)) (princ "...")
  (let ((directive (car sexp)) (entries (cdr sexp)))
    (cond ((eq directive 'classes)
	   (process-classes entries))
	  ((eq directive 'actions)
	   (process-actions entries))
	  ((eq directive 'tokens)
	   (process-tokens entries))
	  ((eq directive 'states)
	   (process-states entries))
	  ((eq directive 'start)
	   (process-start entries))
	  ((eq directive 'accept)
	   (process-accept entries))
	  (t
	   (perror "unknown directive in PROCESS-SEXP"))))
  (princ "done\n"))

(defun process-start (entries)
  (cond ((null entries)
	 (perror "missing value in `(START ...)'"))
	(t
	 (setq start-state (list
			    (if (and (cadr entries) (symbolp (cadr entries)))
				(cadr entries)
			      'start_state)
			    (cadr (assoc (car entries) states-list)))))))

(defun process-accept (entries)
  (cond ((null entries)
	 (perror "missing value in `(ACCEPT ...)'"))
	(t
	 (setq accept-state (list
			    (if (and (cadr entries) (symbolp (cadr entries)))
				(cadr entries)
			      'accept_state)
			    (cadr (assoc (car entries) states-list)))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Output Routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-plain (source-file-name output-file-name)
  (princ "Writing output...")
  (let (outf)
    (cond ((not (setq outf (openo output-file-name)))
	   (perror "can't open output file: " output-file-name)
	   nil)
	  (t
	   ;;print creators name
	   (prin1 :creator outf)
	   (princ "  " outf)
	   (print program-name outf)
	   (terpri outf)

	   ;;print version
	   (prin1 :version outf)
	   (princ "  " outf)
	   (print scanner-version outf)
	   (terpri outf)

	   ;;print name of source file
	   (prin1 :source outf)
	   (princ "  " outf)
	   (print source-file-name outf)
	   (terpri outf)

	   ;;print list of character class codes
	   (print :classes outf)
	   (dolist (item char-classes-list)
	     (princ "\t" outf)
	     (prin1 (car item) outf)
	     (princ "  " outf)
	     (print (cadr item) outf))
	   (terpri outf)

	   ;;print list of characters and class they belong to
	   (prin1 :chartoclass outf)
	   (princ "  " outf)
	   (print char-set-length outf)
	   (dotimes (i char-set-length)
	     (princ "\t" outf)
	     (prin1 (aref char-classes-vector i) outf)
	     (princ "  " outf)
	     (print i outf))
	   (terpri outf)

	   ;;print list of action codes
	   (print :actions outf)
	   (dolist (item actions-list)
	     (princ "\t" outf)
	     (prin1 (car item) outf)
	     (princ "  " outf)
	     (print (cadr item) outf))
	   (terpri outf)

	   ;;print list of token codes
	   (print :tokens outf)
	   (dolist (item tokens-list)
	     (princ "\t" outf)
	     (prin1 (car item) outf)
	     (princ "  " outf)
	     (print (cadr item) outf))
	   (terpri outf)

	   ;;print list of return codes
	   (prin1 :returns outf)
	   (princ "  " outf)
	   (print states-next outf)
	   (dotimes (i states-next)
	     (princ "\t" outf)
	     (prin1 (cadr (assoc i states-return)) outf)
	     (princ "  " outf)
	     (print i outf))
	   (terpri outf)

	   ;;print list of error messages
	   (prin1 :errors outf)
	   (princ "  " outf)
	   (print states-next outf)
	   (dotimes (i states-next)
	     (princ "\t" outf)
	     (prin1 (cadr (assoc i states-error)) outf)
	     (princ "  " outf)
	     (print i outf))
	   (terpri outf)

	   ;;print start state 
	   (print :start outf)
	   (princ "\t" outf)
	   (cond ((boundp 'start-state)
		  (prin1 (car start-state) outf)
		  (princ "  " outf)
		  (print (cadr start-state) outf))
		 (t
		  (print :node outf)))
	   (terpri outf)

	   ;;print the accept state
	   (print :accept outf)
	   (princ "\t" outf)
	   (cond ((boundp 'accept-state)
		  (prin1 (car accept-state) outf)
		  (princ "  " outf)
		  (print (cadr accept-state) outf))
		 (t
		  (print :none outf)))
	   (terpri outf)
	   
	   ;;print the goto matrix
	   (prin1 :goto-matrix outf)
	   (princ "  " outf)
	   (prin1 states-next outf)
	   (princ "  " outf)
	   (print char-classes-next outf)
	   (dolist (item states-goto) 
	     (dotimes (i char-classes-next)
	       (princ "\t" outf)
	       (print (cadr (assoc (nth i item) states-list)) outf))
	     (terpri outf))
	   (terpri outf)

	   ;;print the action matrix
	   (prin1 :action-matrix outf)
	   (princ "  " outf)
	   (prin1 states-next outf)
	   (princ "  " outf)
	   (print char-classes-next outf)
	   (dolist (item states-act)
	     (dolist (i item)
	       (princ "\t" outf)
	       (print i outf))
	     (terpri outf))
	   (terpri outf)
	   (close outf))))
  (princ "done")
  (terpri))
