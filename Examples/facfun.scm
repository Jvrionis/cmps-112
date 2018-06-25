#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: facfun.scm,v 1.3 2014-10-31 17:35:08-07 - - $

(define-syntax µ
        (syntax-rules ()
            ((_ formals body ...) (lambda formals body ...))))

(define (foldl fn unit lis)
        (if (null? lis) unit
            (foldl fn (fn unit (car lis)) (cdr lis))))

(define (foldr fn unit lis)
        (if (null? lis) unit
            (fn (car lis) (foldr fn unit (cdr lis)))))

(define (grep ok? lis)
        (define (test hd tl) (if (ok? hd) (cons hd tl) tl))
        (foldr test '() lis))

(define (map fn lis)
        (foldr (lambda (hd tl) (cons (fn hd) tl)) '() lis))

(define (unfold done? fcar fcdr seed)
        (define (unfold* seed*)
                (if (done? seed*) '()
                    (cons (fcar seed*)
                          (unfold* (fcdr seed*)))))
        (unfold* seed))
        

(define (.. from to)
        (unfold (lambda (i) (> i to))
                (lambda (i) i)
                (lambda (i) (+ 1 i))
                from))

(define (iter fn unit done? fcar fcdr seed)
        (define (iter* seed*)
                (if (done? seed*) unit
                    (fn (fcar seed*)
                        (iter* (fcdr seed*)))))
        (iter* seed))

(define (... from to)
        (iter cons '()
              (lambda (i) (> i to))
              (lambda (i) i)
              (lambda (i) (+ 1 i))
              from))

(define (unfolditer done? fcar fcdr seed)
        (iter cons '() done? fcar fcdr seed))

(define (.... from to)
        (unfolditer (lambda (i) (> i to))
                    (lambda (i) i)
                    (lambda (i) (+ 1 i))
                    from))

(define (fac n)
        (iter * 1 
              (lambda (i) (> i n))
              (lambda (i) i)
              (lambda (i) (+ 1 i))
              1))

