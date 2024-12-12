(define (make-memo-table i j)
  (letrec ((make-row (lambda ()
                       (make-vector (+ j 1) 0)))
           (gen-rows (lambda (count)
                       (cond ((= count 0) (list (make-row)))
                             (else (cons (make-row) (gen-rows (- count 1))))))))
    (list->vector (gen-rows i))))


(define (get-memo table i j)
  (let* ((get-row (vector-ref table i))
         (get-col (vector-ref get-row j)))
    get-col))

(define (update-memo table i j val)
  (let* ((get-row (vector-ref table i))
         (updated-row (vector-set! get-row j val)))
    table)) 

(define table (make-memo-table 3 4))
(define updatedTable (update-memo table 0 0 2))
(get-memo updatedTable 0 0)