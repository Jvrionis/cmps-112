#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; James Vrionis  jvrionis
;; Luke Tanner    latanner
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an 
;;    SBIR program, which is the executed.  Currently it is only 
;;    printed.
;;--------------------------------------------------------------------

;; Standard Error
(define *stderr* (current-error-port))

;; Gets the file name
(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)
;;--------------------------------------------------------------------

;; Print list content to stderr, append newline, quit program.
(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)
;;--------------------------------------------------------------------

;; Incorrect use of file 
(define (usage-exit)
  (die `("Usage: " ,*run-file* " filename"))
)
;;--------------------------------------------------------------------

;; Return file input as Linked List for each line
(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program)))
)
;;--------------------------------------------------------------------

;; Symbol, Label, and Function Tables:
(define *symbol-hash* (make-hash))   
(define *label-hash* (make-hash))    
(define *func-hash* (make-hash))     
;;--------------------------------------------------------------------

;; Sets value and key for Symbol Table:
(define (set-symbol! key value)
  (hash-set! *symbol-hash* key value))
;;--------------------------------------------------------------------

;; Sets value and key for Label Table:
(define (set-label! key value)
  (hash-set! *label-hash* key value))
;;--------------------------------------------------------------------

;; Set value and key for Function Table:
(define (set-func! key value)
  (hash-set! *func-hash* key value))
;;--------------------------------------------------------------------


;; Initialize the symbol table w/ extra ASG1 functions:
(for-each (lambda (pair) (set-symbol! (car pair) (cadr pair)))
  `(  (log10_2 0.301029995663981195213738894724493026768189881)
      (sqrt_2  1.414213562373095048801688724209698078569671875)
      (e       2.718281828459045235360287471352662497757247093)
      (pi      3.141592653589793238462643383279502884197169399)
      (div     ,(lambda (x y) (floor (/ x y))))
      (log10   ,(lambda (x) (/ (log x) (log 10.0))))
      (mod     ,(lambda (x y) (- x (* (div x y) y))))
      (quot    ,(lambda (x y) (truncate (/ x y))))
      (rem     ,(lambda (x y) (- x (* (quot x y) y))))
      (<>      ,(lambda (x y) (not (= x y))))
      (+ ,+) (- ,-) (* ,*) (/ ,/) (abs ,abs) 
      (<= ,<=) (>= ,>=) (= ,=) (> ,>) (tan ,tan)
      (< ,<) (^ ,expt) (atan ,atan) (sin ,sin) 
      (asin ,asin) (acos ,acos) (round ,round)
      (ceil ,ceiling) (exp ,exp) (floor ,floor)
      (log ,log) (sqrt ,sqrt) (cos ,cos)
  )
)
;;--------------------------------------------------------------------

;; Input File Handling
(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n")
    (set! prog-len (length program))
    (make-label program)
    (line-eval program 0)
)
;;--------------------------------------------------------------------

;; Prints each item in list str
(define (s-print str)
  (map (lambda (x) (display (exp-eval x))) str)
  (newline))
;;--------------------------------------------------------------------

;; Array for name and values of expression
(define (s-dim exp)
  (if (pair? exp)
    (let ((array (car exp)))
      (set-symbol! (car array) (make-vector (exp-eval (cadr array)))))
    (die "Invalid Array Declaration :" exp)))
;;--------------------------------------------------------------------

;; Assigns Symbol table values
(define (s-let value)
    (set-symbol! (car value) (exp-eval (cadr value))))
;;--------------------------------------------------------------------

;; Assigns values in str to input from read, only reads in numbers
(define (s-input str)
  (define (s-input. str count)
      (if (null? (car str)) 
          (set-symbol! 'icount count)
          (let ((token (read)))
            (cond ((eof-object? token)
                (set-symbol! 'icount (- 1)))
              (else
                (cond ((number? token) 
                  (set! count (+ count 1))
                  (set-symbol! (car str) token) 
                  (set-symbol! 'icount count))
                (else 
                  (printf "Error: Input Token not a number")))
      (when (not (null? (cdr str))) (s-input. (cdr str) count)))))))
  (s-input. str 0))
;;--------------------------------------------------------------------

;; Fill Function table with our functions. 
(for-each (lambda (pair)(hash-set! *func-hash* (car pair)(cadr pair)))
    `(  (print ,s-print) (dim   ,s-dim)
        (let   ,s-let)   (input ,s-input)
        (if    (void))   (goto  (void)))
     )
;;--------------------------------------------------------------------


;; Fill Label Table
(define (make-label program)
    (for-each (lambda (line)
        (cond
            ((= (length line) 3)
                (set-label! (cadr line) (- (car line)1)))
            ((= (length line) 2)
                (when (not(pair? (cadr line)))
                    (set-label! (cadr line) (- (car line) 1))))))
        program))
;;--------------------------------------------------------------------

;; Line evaluatEvaulates a line of the program given by number 
;; Find function to pass to eval-func
(define (line-eval program number)
  (when (< number prog-len)
    (let* ((line (list-ref program number)) (Len (length line)))
      (cond
        ((= Len 3)
          (fun-eval program (caddr line) number))
        ((and(= Len 2)(not(hash-has-key? *label-hash* (cadr line))))
          (fun-eval program (cadr line) number))
        (else
          (line-eval program (+ number 1))))))
)
;;--------------------------------------------------------------------

;; Evaluate and Check for the function in the function table 
;; Expressions go to exp-eval returning to line-evalto  
;; 'Die' (quit) if function doesn't exist in table
(define (fun-eval program intable number)  
  (when (not (hash-has-key? *func-hash* (car intable))) 
    (die '((car intable) " is not a valid instruction.")))
  (cond
    ((eq? (car intable) 'goto)
      (if(hash-has-key? *label-hash* (cadr intable)) 
        (line-eval program (hash-ref *label-hash* (cadr intable)))
        (die '("Invalid Label : " (cadr intable)))))
    ((eq? (car intable) 'if)
      (if (exp-eval (cadr intable))
        (line-eval program (hash-ref *label-hash* (caddr intable)))
        (line-eval program (+ number 1))))
    (else
      ((hash-ref *func-hash* (car intable)) (cdr intable))
      (line-eval program (+ number 1)))))
;;--------------------------------------------------------------------

;; If in symbole table evaluate expression
;; Else return input value
(define (exp-eval intable)
  (cond 
    ((number? intable)(+ intable 0.0))
    ((string? intable) intable)((hash-has-key? *symbol-hash* intable)
    (hash-ref *symbol-hash* intable))
    ((pair? intable)(define op (car intable))
      (if (hash-has-key? *symbol-hash* op)
        (let ((real-op (hash-ref *symbol-hash* op)))
          (cond ((vector? real-op)
            (when (not(null? (cdr intable)))
             (define index (exp-eval (cadr intable)))
              (if (and( < index (vector-length real-op))(number? index))
                  (vector-ref real-op index)
                  (die '("Index Error: " op "(" index ")")))))
                  ((procedure? real-op)
                      (apply real-op (map exp-eval (cdr intable))))
                (else
                    (die '("Undefined Expression : " intable)))))
          (die '("Undefined Symbol: " op))))
        (else
          (die '("Invalide Instuction" intable))))
)
;;--------------------------------------------------------------------
 
;; Main():
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (write-program-by-line sbprogfile program)))
)
;;--------------------------------------------------------------------

;; Command line Arguments added to main
(main (vector->list (current-command-line-arguments)))
;;--------------------------------------------------------------------
