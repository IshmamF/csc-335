#lang eopl

;; Exercise 3.6 [ ] Extend the language by adding a new primitive operator minus that takes one argument, n,
;; and returns -n.
;; --> minus (+(minus(5), 9)) -> -4

(define grammar-3-1
  '((program
     (expression) a-program)
    (expression
     (number)
     lit-exp)
    (expression
     (identifier)
     var-exp)
    (expression
     (primitive "(" (separated-list expression ",")  ")" )
     primapp-exp)
    (primitive ("+")
               add-prim)
    (primitive ("-")
               subtract-prim)
    (primitive ("*")
               mult-prim)
    (primitive ("add1")
              incr-prim)
    (primitive ("sub1")
               decr-prim)
    (primitive ("minus")
               minus-prim)))

;; something's wrong here
(define scanner-spec-3-1
  '((white-sp
     (whitespace)                                    skip)
    (comment
     ("%" (arbno (not #\newline)))                   skip)
    (identifier
     (letter (arbno (or letter digit "?")))          symbol)
    (number
     (digit (arbno digit))                           number)))

(define minus-prim
  (lambda (n)
    (- n)))  

(define scan&parse
  (sllgen:make-string-parser
   scanner-spec-3-1
   grammar-3-1))

(define apply-primitive
  (lambda (prim args)
    (letrec ((accumulate (lambda (op init seq)
                           (cond ((null? seq) init)
                                 (else
                                  (op (car seq)
                                      (accumulate op init (cdr seq)))))))
             (cases primitive prim
               (add-prim () (accumulate + 0 args))
               (subtract-prim () (accumulate - 0 args))
               (mult-prim () (accumulate * 1 args))
               (incr-prim () (+ (car args) 1))
               (decr-prim () (- (car args) 1))
               (minus-prim () (* (car args) -1))
               (cons-prim () (begin
                               (newline)
                               (display "Here is args: ")
                               (display args)
                               (newline)
                               (newline)
                               (cons (car args) (cadr args))))
               (car-prim () (caar args))
               (cdr-prim () (cdar args))
               (list-prim () (accumulate cons '() args) ;; could be just args
                          ))))))                           


;; Exercise 3.7 [ ] Add list processing primitives to the language, including cons, car, cdr,
;; list, and a new variable, emptylist, which is bound to the empty list. Since there is no support for
;; symbols, lists can contain only numbers and other lists. How does this change the expressed and denoted
;; values of the language?
;; --> list (1,2,3) -> (1 2 3)
;; --> car (cons (4,emptylist)) -> 4

;; What do we need: cons, car, cdr, list, newVar, emptyList

;; Exercise 3.9 [ ] Modify the interpreter so that invoking a primitive operation on the wrong number of
;; arguments causes an error to be reported. (Since this check involves only static information, it could be done
;; prior to run-time, which is preferable for many reasons. We encourage the use of such an approach.)

(define check-program
  (lambda (prgm)
    (cases program prgm
      (a-program (exp)
                 (check-expression (exp))))))

(define check-exp
  (lambda (exp)
    (cases expression exp
      (primapp-exp (prim rands)
                   (let ((rands-check (check-rands rands)))
                     (if rands-check
                         (check-prim prim rands)
                         #f)))
      (else #t))))

(define check-rands
  (lambda (rands)
    (map check-expression rands)))

(define check-prim
  (lambda (prim rands)
    (let ((len (length rands)))
      (cases primitive prim
        (incr-prim () (= len 1))
        (decr-prim () (= len 1))
        (minus-prim () (= len 1))
        (cons-prim () (= len 2))
        (car-prim () (= len 1))
        (cdr-prim () (= len 1))
        (else #t)))))

  (define run-with-check
    (lambda (string)
      (let ((pgm (scan&parse string)))
        (if (check-pgm)
            (eval-pgm)
            (eopl:error 'run-with-check "~s fails arity condition" string)))))

  (define read-check-eval-print
    (sllgen:make-rep-loop "-->" (lambda (pgm) (if (check-program pgm)
                                                  (eval-program pgm)
                                                  (eopl:error 'read-eval-check-print
                                                              "~s fails check" pgm)))
                          sllgen:make-stream-parser
                          scanner-spec-3-1
                          grammar-3-2))
                                                              

;; Exercise 3.10 [ ] Test if forms by extending the interpreter of figure 3.2.



;; Exercise 3.13 [ ] Add to the defined language a facility that extends if as cond does in Scheme. Use the
;; grammar