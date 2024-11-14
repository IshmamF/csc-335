#lang eopl

; Implementation of Stack data structure
(define-datatype stack stack?
  (empty-stack)
  (non-empty-stack
   (top symbol?)
   (rest stack?)))

(define push
  (lambda (new s)
    (non-empty-stack new s)))

(define pop
  (lambda (s)
    (cases stack s
      (empty-stack () #f)
      (non-empty-stack (top rest) rest))))

(define top
  (lambda (s)
    (cases stack s
      (empty-stack () #f)
      (non-empty-stack (top rest) top))))

; When reading EOPL, think deeply about the code provided
; check if the input is correct and output is correct
; question the code , is it doing the right thing?
(define s1 empty-stack)
(define s2 (push 'a (s1)))
(define top1 (top s2))

; A way to modulize things in scheme
; similar to objects,  though there's
; no state still. 
(define (module msg)
  (define (f1 x) 'f1)
  (define (f2 x) 'f2)
  (define (f3 x) 'f3)
  (define (dispatch m)
    (cond ((eq? m 1) f1)
          ((eq? m 2) f2)
          ((eq? m 3) f3)))
  (dispatch msg))

(module 1) ; function f1
(module 2) ; function f2
(module 3) ; function f3

; Is there possibility of the stack persisting?