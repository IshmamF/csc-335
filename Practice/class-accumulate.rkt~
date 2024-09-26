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

