(define dl '((1 (2)) (((3) 4) (5 (6)) () (7)) 8))

(define (atom? x) (and (not (null? x)) (not (pair? x))))

(define (count-atoms dl)
  (cond ((null? dl) 0)
        ((atom? dl) 1)
        (else (+ (count-atoms (car dl)) (count-atoms (cdr dl))))))

(define (flatten dl)
  (cond ((null? dl) '())
        ((atom? dl) (list dl))
        (else (append (flatten (car dl)) (flatten (cdr dl))))))

(define (snoc x y) (cons y x))
(define (rcons x l) (append l (list x)))

(define (deep-reverse dl)
  (cond ((null? dl) '())
        ((atom? dl) dl)
        (else (rcons (deep-reverse (car dl)) (deep-reverse (cdr dl))))))

(define (deep-foldr nv term op dl)
  (cond ((null? dl) nv)
        ((atom? dl) (term dl))
        (else (op (deep-foldr nv term op (car dl))
                  (deep-foldr nv term op (cdr dl))))))

(define (count-atoms dl)
  (deep-foldr 0 (lambda (x) 1) + dl))

(define (flatten dl)
  (deep-foldr '() list append dl))

(define (id x) x)

(define (deep-reverse dl)
  (deep-foldr '() id rcons dl))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (branch p? f g) (lambda (x) (if (p? x) (f x) (g x))))

(define (deep-foldr nv term op dl)
  (foldr op nv (map
                (branch atom? term
                        (lambda (x) (deep-foldr nv term op x))) dl)))

; (define (maximum x . l)
;   (foldl1 max (cons x l)))

(define (append . pl)
  (if (null? pl) '()
      (let ((l1 (car pl)))
        (if (null? l1) (apply append (cdr pl))
            (cons (car l1) (apply append (cdr l1) (cdr pl)))))))

(define (evali x) (eval x (interaction-environment)))