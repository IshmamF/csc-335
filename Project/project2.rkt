;; Example:
;; s1 = "aggtab"
;; s2 = "gxtxayb"
;; We need a representation of lists of lists
;; [ [0 0 0 0 0 0] [0 ] [0 ] [0 ] [0 ] [0 ] [0 ] [0 ]]
;; s1[i] s2[j]
;; We create a table and initialize (i, 0) to 0s (create an array of zeros to represent collumns)
;; and initialize (0, j) to 0s (basically multiple arrays setting first element of each array to 0)

(define (create_rows i j)
  (cond ((= j 0) '())
        (else (cons (generate_cols -1 i) (create_rows i (- j 1))))))

(define (generate_cols num i)
  (cond ((= i 0) '())
        (else (cons num (generate_cols num (- i 1))))))

(define memoTable
  (lambda (s1 s2)
    (let* ((i (length s1))
           (j (length s2))
           (table (create_rows i j)))
      table)))

(define (find_item lst index)
  (cond ((= index 0) (car lst))
        (else (find_item (cdr lst) (- index 1)))))


(define (get_memo memo i j)
  (let* ((jthRow (find_item memo j))
         (ithjthVal (find_item jthRow i)))
    ithjthVal))

(define (update_item lst index val)
  (cond ((= index 0) (cons val (cdr lst)))
        (else (cons (car lst) (update_item (cdr lst) (- index 1) val)))))

(define (add_memo memo val i j)
  (let* ((jthRow (find_item memo j))
         (updatedRow (update_item jthRow i val))
         (updatedTable (update_item memo j updatedRow)))
    updatedTable))


(define (helper table s1 s2 i j)
  (cond ((or (= i (length s1)) (= j (length s2))) (list 0 table))
        ((not (= (get_memo table i j) -1)) (list (get_memo table i j) table))
        ((eq? (list-ref s1 i) (list-ref s2 j)) (let* ((call (helper table s1 s2 (+ i 1) (+ j 1)))
                                                      (call-val (car call))
                                                      (call-table (cadr call))
                                                      (curr-val (+ 1 call-val))
                                                      (new-table (add_memo call-table curr-val i j)))
                                                 (list curr-val new-table)))
        (else (let* ((call-i (helper table s1 s2 (+ i 1) j))
                     (call-j (helper table s1 s2 i (+ j 1)))
                     (i-val (car call-i))
                     (j-val (car call-j))
                     (max-info (if (>= i-val j-val)
                                   call-i
                                   call-j))
                     (max-val (car max-info))
                     (max-table (cadr max-info))
                     (new-table (add_memo max-table max-val i j)))
                (list  max-val new-table)))))

(define (LCS s1 s2)
  (let* ((table (memoTable s1 s2))
           (return (helper table s1 s2 0 0)))
    (car return)))

(define s1 '(a g g t a b))
(define s2 '(g x t x a y b))

(define table (memoTable s1 s2))
(define newTable (add_memo table 1 1 1))
(define test_access (get_memo newTable 1 1))




            

        