(define m '((1 2 3) (4 5 6)))

(define get-first-row car)
(define (get-first-col m) (map car m))
(define (del-first-col m) (map cdr m))

(define (get-row i m) (list-ref m i))
(define (get-col i m)
  (map (lambda (row) (list-ref row i)) m))

(define (transpose m)
  (if (null? (get-first-row m)) '()
      (cons (get-first-col m)
            (transpose (del-first-col m)))))

(define (transpose m) (apply map list m))

(define (sum-vectors v1 v2) (map + v1 v2))
(define (sum-matrices m1 m2) (map sum-vectors m1 m2))

(define (mult-vectors v1 v2) (apply + (map * v1 v2)))
(define (mult-matrices m1 m2)
  (map (lambda (row) ;; за всеки ред row на m1
         (map (lambda (col) (mult-vectors row col))
              (transpose m2))) ;; за всяка колона col на m2
        m1))