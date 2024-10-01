(define (check-2-sorted a b)
  (if (< a b)
      #f
      #t))

(define (check-sorted x)
  (cond
    ((< x 10) #t)
    (else(and (check-2-sorted (modulo x 10)(modulo (quotient x 10) 10))
              (check-sorted (quotient x 10))))))



(define (check-iter-sort x)
  (define (iter x curr)
  (cond
    ((< x 10) curr)
    (else (iter (quotient x 10) (and (check-2-sorted
                                      (modulo x 10)
                                      (modulo (quotient x 10) 10))
                                     curr)))
    ))
  (iter x #t))