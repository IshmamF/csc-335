;; Example:
;; s1 = "aggtab"
;; s2 = "gxtxayb"
;; We need a representation of lists of lists
;; [ [0 0 0 0 0 0] [0 ] [0 ] [0 ] [0 ] [0 ] [0 ] [0 ]]
;; s1[i] s2[j]
;; We create a table and initialize (i, 0) to 0s (create an array of zeros to represent collumns)
;; and initialize (0, j) to 0s (basically multiple arrays setting first element of each array to 0)
 
;; helper functions to create tables / iterables
(define (create_rows_with_val i j val)
  (cond ((= j 0) '())
        (else (cons (generate_cols val i) (create_rows_with_val i (- j 1) val)))))

(define (generate_cols num i)
  (cond ((= i 0) '())
        (else (cons num (generate_cols num (- i 1))))))

(define (modified_create_rows i j)
  (cond ((= j 0) '())
        (else (cons (cons 0 (generate_cols -1 (- i 1))) (modified_create_rows i (- j 1))))))

;; we will iterate through this one because Im not sure how to skip the first indexes
(define iterable
  (lambda (s1 s2)
    (let* ((i (length s1))
           (j (length s2))
           (table (create_rows_with_val i j -1)))
      table)))

;; we will access values from here and update vales on this table
(define memoTable
  (lambda (s1 s2)
    (let* ((i (+ 1 (length s1)))
           (j (length s2))
           (temp_table (modified_create_rows i j))
           (first_row (generate_cols 0 i)))
      (cons first_row temp_table))))

(define (find_item lst index)
  (cond ((= index 0) (car lst))
        (else (find_item (cdr lst) (- index 1)))))

;; access table
(define (get_memo memo i j)
  (let* ((jthRow (find_item memo j))
         (ithjthVal (find_item jthRow i)))
    ithjthVal))

(define (update_item lst index val)
  (cond ((= index 0) (cons val (cdr lst)))
        (else (cons (car lst) (update_item (cdr lst) (- index 1) val)))))

;; update table
(define (add_memo memo val i j)
  (let* ((jthRow (find_item memo j))
         (updatedRow (update_item jthRow i val))
         (updatedTable (update_item memo j updatedRow)))
    updatedTable))

(define (iterate_row table row s1 s2 i j)
  (cond ((null? row) table)
        ((eq? (list-ref s1 (- i 1)) (list-ref s2 (- j 1))) (let* ((prev-i (- i 1))
                                                    (prev-j (- j 1))
                                                    (get-val (get_memo table prev-i prev-j))
                                                    (new-val (+ 1 get-val))
                                                    (updated-table (add_memo table new-val i j)))
                                               (iterate_row updated-table (cdr row) s1 s2 (+ i 1) j)))
        (else (let* ((prev-i (- i 1))
                     (prev-j (- j 1))
                     (get-upper-val (get_memo table i prev-j))
                     (get-left-val (get_memo table prev-i j))
                     (get-max (max get-upper-val get-left-val))
                     (updated_table (add_memo table get-max i j)))
                (iterate_row updated_table (cdr row) s1 s2 (+ i 1) j)))
        ))


(define (helper table iterable s1 s2 i j)
  (cond ((= j (+ (length s2) 1)) table)
        (else (let* ((curr_row (car iterable))
                     (new-result (iterate_row table curr_row s1 s2 i j)))
                (helper new-result (cdr iterable) s1 s2 i (+ j 1))))))


;; memoized LCS, runs in O(m * n) time complexity
(define (memo-LCS s1 s2)
  (let* ((make-table (memoTable s1 s2))
         (make-iter (iterable s1 s2))
         (return-table (helper make-table make-iter s1 s2 1 1))
         (SubstrLength (get_memo return-table (length s1) (length s2))))
    return-table))


(define (LCS s1 s2)
  (cond ((or (null? s1) (null? s2)) 0)
        ((eq? (car s1) (car s2)) (+ 1 (LCS (cdr s1) (cdr s2))))
        (else (let* ((call-i (LCS (cdr s1) s2))
                     (call-j (LCS s1 (cdr s2))))
                (max call-i call-j)))
        ))


(define s1 '(a g g t a b a))
(define s2 '(g x t x a y b))
(define str1 '(A B C D E F G H I J K L M N O P Q R S T))
(define str2 '(X Y Z A B Q R S T U V W A B C D E))
; (LCS str1 str2) takes a long time to compute
(memo-LCS s1 s2)

(define table (memoTable s1 s2))
(define newTable (add_memo table 1 1 1))
(define test_access (get_memo newTable 1 1))




            

        