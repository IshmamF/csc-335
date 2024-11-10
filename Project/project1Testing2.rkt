(define (fib-memo n memo k)
  (let ((p (assv n memo)))
    (if p
        (let ((v (cdr p)))
          (k v memo))
        (fib-memo (- n 1) memo
                  (lambda (v1 memo^)
                    (fib-memo (- n 2) memo^
                              (lambda (v2 memo^^)
                                (let ((v (+ v1 v2)))
                                  (k v (cons (cons n v) memo^^))))))))))




(define (fib n)
  (fib-memo n '((1 . 1) (0 . 0)) (lambda (v memo) v)))

(define (make-comp bal int)
  (let ((rate (+ 1 (/ int 100.0))))
    (lambda () (set! bal (* bal rate)) (round bal))))

(define bal (make-comp 100 20))

(define fact
(let ((h (make-hash))) 
  (define (fact n)
    (cond ((= n 0) 1)
          ((hash-has-key? h n) (hash-ref h n))
          (else
            (let ((f (* n (fact (- n 1)))))
              (hash-set! h n f)
              f))))
  fact))