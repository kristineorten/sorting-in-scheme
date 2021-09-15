"Reduce"
(define (reduce proc init lst)
  (if (null? lst)
      init
      (proc (car lst) (reduce proc init (cdr lst)))))

"Add sorted"
(define (add-sorted x lst)
  (cond ((null? lst) (list x))
        ((<= x (car lst)) (cons x lst))
        (else (cons (car lst) (add-sorted x (cdr lst))))))

;; Forventet oppførsel
(equal? (add-sorted 5 '(2 4 6)) '(2 4 5 6))
(equal? (add-sorted 5 '(2 5 6)) '(2 5 5 6))
(equal? (add-sorted 3 '(2 4 5 6)) '(2 3 4 5 6))
(equal? (add-sorted 10 '(2 4 6 7 8 9)) '(2 4 6 7 8 9 10))
(equal? (add-sorted 0 '(0)) '(0 0))
(equal? (add-sorted 1 '()) '(1))

"Insertion sort"
(define (insertion-sort lst)
  (reduce add-sorted '() lst))

;; Forventet oppførsel
(equal? (insertion-sort '(1 3 2 4)) '(1 2 3 4))
(equal? (insertion-sort '(1 3 8 4 5)) '(1 3 4 5 8))
(equal? (insertion-sort '(5 4 3 0)) '(0 3 4 5))
(equal? (insertion-sort '(1 3 3 2)) '(1 2 3 3))
(equal? (insertion-sort '()) '())