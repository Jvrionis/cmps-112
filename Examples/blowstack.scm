#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: blowstack.scm,v 1.4 2017-10-05 15:44:50-07 - - $

;;
;; Blow the stack by infinite recursion.  This function is not
;; a tail call.
;;

(define modulus 1000000)

(define (memory-use)
        (truncate (/ (current-memory-use) (* 1024 1024))))

(define (recur count)
        (when (= (remainder count modulus) 0)
              (printf "count = ~a, memory = ~a M~n"
                      count (memory-use)))
        (+ 1 (recur (+ 1 count))))

(recur 0)


