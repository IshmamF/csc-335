(define (merge-sort lst)
  (cond
    ((= (length lst) 1) lst)
    (else (sort (merge-sort (first-half lst (quotient (length lst) 2)))
                (merge-sort (second-half lst (quotient (length lst) 2)))
                ))))

(define (sort lst1 lst2)
  (cond
    ((null? lst1) lst2)
    ((null? lst2) lst1)
    ((< (car lst1) (car lst2))(cons (car lst1) (sort (cdr lst1) lst2)))
    (else (cons (car lst2) (sort lst1 (cdr lst2))))))

(define (first-half lst size)
  (cond
    ((= size 1) (list (car lst)))
    (else (cons (car lst) (first-half (cdr lst) (- size 1))))))

(define (second-half lst size)
  (cond
    ((null? lst) '())
    ((>= size 1) (second-half (cdr lst) (- size 1)))
    (else (cons (car lst) (second-half (cdr lst) size)))))
;-----------------------------------------------------------------------------
