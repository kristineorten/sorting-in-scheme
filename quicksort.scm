"Hjelpeprodedyre: filter-by-value"
;; Filterer ut elementer vha <,> og = og en positiv heltallsverdi
(define (filter-by-value pred value lst)
  (cond ((null? lst) '())
        ((pred (car lst) value)
         (cons (car lst) (filter-by-value pred value (cdr lst))))
        (else (filter-by-value pred value (cdr lst)))))

;; Forventet oppførsel: forventer å ta inn <,>,= og et positivt heltall
(equal? (filter-by-value < 3 '(1 2 3 4 5)) '(1 2))
(equal? (filter-by-value > 3 '(1 2 3 4 5)) '(4 5))
(equal? (filter-by-value = 3 '(1 2 3 4 5)) '(3))
(equal? (filter-by-value < 3 '(8 2 2 1)) '(2 2 1))
(equal? (filter-by-value <= 2 '(8 2 2 1)) '(2 2 1))

"Hjelpeprosedyre: concatinate-lists"
;; Legger sammen to lister
(define (concatinate-lists list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (concatinate-lists (cdr list1) list2))))

;; Forventet oppførsel
(equal? (concatinate-lists '(a) '(b)) '(a b))
(equal? (concatinate-lists '(a b) '(c d e)) '(a b c d e))
(equal? (concatinate-lists '() '(a b c)) '(a b c))
(equal? (concatinate-lists '(1 2) '(3 (4 5))) '(1 2 3 (4 5)))
(equal? (concatinate-lists '(1 2) '(3)) '(1 2 3))

"Quicksort"
;; Sorterer en liste over positive heltall
(define (quicksort lst)
  ;; Merk: Velger den "tilfeldige" x'en som første element: (car lst)
  (if (null? lst)
      lst ; Returnerer den tomme lista
      (concatinate-lists
       (quicksort (filter-by-value <= (car lst) (cdr lst)))
       (concatinate-lists
        (list (car lst))
        (quicksort (filter-by-value > (car lst) (cdr lst)))))))

;; Tester quicksort
(equal? (quicksort '(1 2 3 4 5)) '(1 2 3 4 5))
(equal? (quicksort '(1 3 2 4 5)) '(1 2 3 4 5))
(equal? (quicksort '(5 4 3 2 1)) '(1 2 3 4 5))
(equal? (quicksort '(5 2 3 4 1)) '(1 2 3 4 5))
(equal? (quicksort '(5 2 3 4)) '(2 3 4 5))
(equal? (quicksort '(2 2 3 1)) '(1 2 2 3))
(equal? (quicksort '(2 0 3 1)) '(0 1 2 3))