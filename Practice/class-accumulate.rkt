; L is a list with elements compatible with pred
(define filter
  (lambda (pred L)
    (cond ((null? L) L)
          ((pred  (car L))(cons (car L) (filter pred (cdr L))))
          (else (filter pred (cdr L)))
          )))

; L is a list whrere elements compatible with func
(define map
  (lambda (func L)
    (cond ((null? L) L)
          (else (cons (func(car L))(map func (cdr L))))
          )))

; There's a common pattern here - abstarcted by accumulate

(define accumulate
  (lambda (op init seq)
    (cond ((null? seq) init)
          (else (op (car seq)(accumulate op init (cdr seq))))
          )))

(define filter
  (lambda (pred L)
    (accumulate (lambda (x y)
                  (if (pred x)
                      (cons x y)
                      y))
                '()
                L)))

(define map
  (lambda (func L)
    (accumulate (lambda (x y)
                  (cons (func x) y))
                '()
                L)
    ))

; accumulate handles the 'cdr'ing down stuff 
; Design Roles
; x : (car L)
; y : "accumulation of op over (cdr L)"

