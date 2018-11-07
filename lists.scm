(define l (list 1 2 3 4 5))

(define (length l)
  (if (null? l) 0
      (+ 1 (length (cdr l)))))

(define (length l)
  (foldr (lambda (u v) (+ v 1)) 0 l))

(define (list-tail l n)
  (if (= n 0) l
      (list-tail (cdr l) (- n 1))))

(define (list-ref l n)
  (car (list-tail l n)))

(define (member x l)
  (cond ((null? l) #f)
        ((equal? x (car l)) l)
        (else (member x (cdr l)))))

(define (memq x l)
  (cond ((null? l) #f)
        ((eq? x (car l)) l)
        (else (memq x (cdr l)))))

(define (memv x l)
  (cond ((null? l) #f)
        ((eqv? x (car l)) l)
        (else (memv x (cdr l)))))

(define (from-to a b)
  (if (> a b) '()
      (cons a (from-to (+ a 1) b))))

(define (collect a b next)
  (if (> a b) '()
      (cons a (collect (next a) b next))))

(define (append l1 l2)
  (if (null? l1) l2
      (cons (car l1) (append (cdr l1) l2))))

(define (append l1 l2)
  (foldr cons l2 l1))

(define (reverse l)
  (if (null? l) l
      (append (reverse (cdr l)) (list (car l)))))
;                   t                    h

(define (rcons x l)
  (append l (list x)))

(define (reverse l)
  (foldr rcons '() l))
;(deep-foldr '() id rcons))

(define (snoc u v) (cons v u))

(define (reverse l)
  (foldl snoc '() l))

(define (map f l)
  (if (null? l) l
      (cons (f (car l)) (map f (cdr l)))))

(define (accumulate op nv a b term next)
  (foldr op nv (map term (collect a b next))))

(define (map f l)
  (foldr (lambda (h t) (cons (f h) t)) '() l))

(define (filter p? l)
  (cond ((null? l) l)
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else                       (filter p? (cdr l)))))


(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))


(define (filter p? l)
  (foldr (lambda (h t) (if (p? h) (cons h t) t)) '() l))

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))
