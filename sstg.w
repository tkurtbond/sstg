#-h- 000change_log.txt 853  asc  12-aug-24 15:29:46  tkb ()
Sun Feb 20 12:57:31 2022  T. Kurt Bond  (TKB at MPLVAX)

        * README.RST: Rename from AAAREADME.1ST so GitHub will
        recognize it.   Update with more details about the versions of
        XLISP and SCM used, and list the projects I used it for.

        * SSTG.LSP, SSTG_TO_C.LSP: Update comments slightly in preparation
        for uploading to GitHub.

Fri Feb 18 14:30:30 2022  T. Kurt Bond  (TKB at MPLVAX)

        * SSTG_TO_C.SCM: Finished porting SSTG_TO_C.LSP to Scheme,
        including adding command line argument processing, so it can be
        run non-interactively.

        * SSTG.SCM: Add command line argument processing, so it can be run
        non-interactively. 

        * SSTG_SETUP.COM: add a command procedure to define DCL
        symbols for foreign commands using SCM to run SSTG.SCM and
        SSTG_TO_C.SCM.

#-h- 000todo.org       74  asc  12-aug-24 15:29:46  tkb ()
* Do SSTG_TO_C.SCM and SSTG_TO_BAS.SCM correctly consume SSTG.SCM output?
#-h- do_c_tab.lsp     182  bin  12-aug-24 15:29:47  tkb ()
;;; DO_C_TAB.LSP -- Create C tables for SDCL scanner.
;;; Execute this from main sdcl directory.
(load "[.sstg]sstg_to_c.lsp" t)
(sstg-to-c "sdcl.table" "codes.h" "lextab.h")
(exit)
#-h- do_tab.lsp       260  bin  12-aug-24 15:29:47  tkb ()
;;; DO_TAB.LSP -- create the SDCL scanner tables.
;;; Execute this from main SDCL directory.
(load "[.sstg]sstg.lsp" t)		;load the necessary lisp definitions
(gen-scantab "sdcl.scn" "sdcl.table")	;process scanner data and make tables
(exit)					;exit the lisp
#-h- sstg.lsp       22783  bin  12-aug-24 15:29:47  tkb ()
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
#-h- sstg_to_bas.lsp 13615  bin  12-aug-24 15:29:47  tkb ()
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
#-h- sstg_to_c.lsp  10663  bin  12-aug-24 15:29:48  tkb ()
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
#-h- sstg.scm       22885  bin  12-aug-24 15:29:48  tkb ()
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



#-h- sstg_to_bas.scm 13777  bin  12-aug-24 15:29:49  tkb ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; sstg-to-bas.scm -- convert SSTG output to VAX BASIC and VAX MACRO
;;;;                    SSTG == simple scanner table generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Author:   T. Kurt Bond
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
(define program-name "sstg_to_bas")     ; Name: simple scanner table generator
(define program-version "2.0")		; Version number of scanner

(define tab (integer->char 9))
(define nl #\newline)
(define first-tc-value 10)		;token code values start with this

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
(define (upcase s)
  (let* ((s (if (symbol? s) (symbol->string s) s))
         (l (string-length s))
         (t (make-string l)))
    (do ((i 0 (+ i 1)))
        ((= i l) t)
      (let* ((c (string-ref s i))
             (c (if (char-lower-case? c) (char-upcase c) c)))
        (string-set! t i c)))))

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
	   (db " -- made by " program-name " " program-version " from " source
	       " (by way of " input-file-name " created by " creator " " version ")" nl nl)
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
      (db "character classes" nl)
      (dc tab "external byte constant &" nl)
      (dotimes len (lambda (i)
		     (let* ((name (upcase (rd)))
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
	  (- len 1) ")" nl)
      (dotimes len (lambda (i)
		     (let* ((name (upcase (rd)))
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
		     (let* ((name (upcase (rd)))
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
		     (let* ((name (upcase (rd)))
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
		     (let* ((name (upcase (rd)))
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
	  ".ascid /internal error: invalid lexical error message number/" nl)
      (dt "lex_mess_max == " len nl)
      (dt nl)
      (dt "; Lexical error message vector table" nl)
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
      (db "Start state" nl)
      (let* ((name (upcase (rd)))
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
      (let* ((name (upcase (rd)))
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
				       (let ((value (upcase (rd))))
					 (dt tab ".byte " value nl))))
		   (dt nl)))
	(db nl)
	#t)))))
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; If arguments were specified to SCM on the command line, assume
;;; they are the input table file and the output codes file and
;;; lexical tables file and run sstg-to-bas on them, then exit.
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
        (sstg-to-bas tabfile codesfile lextabfile)
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
#-h- sstg_to_c.scm  12467  bin  12-aug-24 15:29:50  tkb ()
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
#-h- readme.rst      1378  bin  12-aug-24 15:29:50  tkb ()
SSTG -- Simple Scanner Table Generator
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

Documentation is at the bottom of SSTG.SCM and SSTG.LSP.  

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
#-h- sstg_setup.com   728  asc  12-aug-24 15:29:51  tkb ()
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
$ sstg_to_bas :== 'scm' 'sstg_dir'sstg_to_bas.scm
#-h- listsrcs.sh      135  asc  15-aug-24 09:29:28  tkb ()
lr *.txt *.org *.lsp *.scm *.rst *.com *.sh descrip.mms | tee ~usr/sstg.lis | sedit 's/%{?+}$/ar uv sstg.w $1/' | tee ~usr/initsstg.sh
#-h- descrip.mms      567  asc  15-aug-24 09:29:28  tkb ()
!+++
! Make an archival backup with today's date.
!---
zipit : 
	today = f$cvtime (,, "DATE")
        note = "$(NOTE)"
        set def [-]
	zipfile = "[.project_backups]sstg_" + today
        if note .nes. "" then zipfile = zipfile + "_" + note
        zipfile = zipfile + ".zip"
        write sys$output "Zipfile is ", zipfile
	! It's not really accurate unless we start from scratch.
	if f$search (zipfile) .nes. "" then delete 'zipfile';*/log
	zip -r 'zipfile' sstg.dir -x *.dsc *.key *.obj *.exe *.hlb *.lis *.map *.log
        write sys$output "Done zipiting!"

