"Hjelpeprosedyre: add-element-sorted"
(define (add-element-sorted e lst)
  (cond ((null? lst) (cons e lst)) ; Lista er tom, legg til e på slutten
        ((< (car lst) e) ; Dersom neste listeelement < e
         (cons (car lst)
               (add-element-sorted e (cdr lst))))
        ((< (length lst) 2) ; Dersom neste listeelem >= e og det er siste elem i lista
             (cons e
                   (cons (car lst) '())))
        (else (cons e lst)))) ; Dersom neste listeelem >= e, men det er flere elem i lista

;; Forventet oppførsel
(equal? (add-element-sorted 5 '(2 4 6)) '(2 4 5 6))
(equal? (add-element-sorted 5 '(2 5 6)) '(2 5 5 6))
(equal? (add-element-sorted 3 '(2 4 5 6)) '(2 3 4 5 6))
(equal? (add-element-sorted 10 '(2 4 6 7 8 9)) '(2 4 6 7 8 9 10))
(equal? (add-element-sorted 0 '(0)) '(0 0))
(equal? (add-element-sorted 1 '()) '(1))

"Insertion sort"
(define (insertion-sort lst)
  (define (insertion-sort-iter lst_out lst_in)
    (if (null? lst_in)
        lst_out
        (insertion-sort-iter (add-element-sorted (car lst_in) lst_out) (cdr lst_in))))
  (insertion-sort-iter '() lst))

;; Forventet oppførsel
(equal? (insertion-sort '(1 3 2 4)) '(1 2 3 4))
(equal? (insertion-sort '(1 3 8 4 5)) '(1 3 4 5 8))
(equal? (insertion-sort '(5 4 3 0)) '(0 3 4 5))
(equal? (insertion-sort '(1 3 3 2)) '(1 2 3 3))
(equal? (insertion-sort '()) '())