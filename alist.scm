(define al '((a . 1) (b . 2) (c . 3)))

(define (keys al)
  (map car al))

(define (values al)
  (map cdr al))

(define (filter p? l)
  (cond ((null? l) l)
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))


(define (del-assoc key al)
  (filter (lambda (kv) (not (equal? key (car kv)))) al))

(define (add-assoc key value al)
  (cons (cons key value) (del-assoc key al)))


(define (exists? p? l)
  (not (null? (filter p? l))))

(define (exists? p? l)
  (foldr (lambda (x y) (or x y)) #f (map p? l)))

(define (exists? p? l)
  (foldr (lambda (x y) (or (p? x) y)) #f l))

(define (exists? p? l)
  (foldl (lambda (x y) (or x (p? y))) #f l))

(define (exists? p? l)
  (cond ((null? l) #f)
        ((p? (car l)) #t)
        (else (exists? p? (cdr l)))))

(define (search p l)
  (and (not (null? l))
       (or (p (car l)) (search p (cdr l)))))

;(exists? odd? '(1 2 3 4 5))
;(exists? negative? '(1 2 3 4 5))

(define (assoc key al)
  (search (lambda (kv) (and (equal? (car kv) key) kv)) al))

(define (all? p? l)
  (foldr (lambda (x y) (and x y)) #t (map p? l)))

(define (all? p? l)
  (not (search (lambda (x) (not (p? x))) l)))

; (if x y #f)

(define g '((1 2 3) (2 3 6) (3 4 6) (4 1 5) (5 3) (6 5)))
(define g2 '((1 2 3) (2 3 6) (3 4 6) (4 1 5) (5 3) (6)))
(define g3 '((1 2 3) (2 3 6) (3 4 6) (4 5) (5) (6)))

(define vertices keys)
(define (children v g)
  (cdr#f (assv v g)))

(define (protect-#f f)
  (lambda (x) (and x (f x))))

(define (cdr#f p) (and p (cdr p)))
(define cdr#f (protect-#f cdr))

(define (edge? u v g)
  (memv v (children u g)))

(define (map-children f v g)
  (map f (children v g)))

(define (all-children? p? v g)
  (all? p? (children v g)))


(define (filter-children f v g)
  (filter f (children v g)))

(define (search-children p v g)
  (search p (children v g)))

(define (childless g)
  (filter (lambda (v) (null? (children v g))) (vertices g)))

(define (parents v g)
  (filter (lambda (u) (edge? u v g)) (vertices g)))

(define (map-edges f g)
  (apply append
         (map (lambda (u) (map-children (lambda (v) (f u v)) u g)) (vertices g))))

(define (search-edges f g)
    (search (lambda (u) (search-children (lambda (v) (f u v)) u g)) (vertices g)))

(define (all-edges? p? g)
    (all? (lambda (u) (all-children? (lambda (v) (p? u v)) u g)) (vertices g)))

(define (symmetric? g)
  (all-edges? (lambda (u v) (edge? v u g)) g))

; работи за ациклични графи, но може да зацикли за циклични
(define (path-dfs u v g)
  (or (and (eqv? u v) (list u)) ;; ако u = v, то (u) е пътят
      (search-children (lambda (w) (cons#f u (path-dfs w v g))) u g)))

(define (path-dfs u v g)
  (define (search-dfs path)
    (let ((current (car path)))
      (or (and (eqv? current v) (reverse path))
          (search-children (lambda (w)
                             (and (not (memv w path))
                                  (search-dfs (cons w path)))) current g))))
  (search-dfs (list u)))

(define (cons#f h t) (and t (cons h t)))

(define (path-bfs u v g)
  (define (extend path)
    (map-children (lambda (u) (cons u path)) (car path) g))
  (define (remains-acyclic? path)
    (not (memv (car path) (cdr path))))
  (define (extend-acyclic path)
    (filter remains-acyclic? (extend path)))
  (define (target-path path)
    (and (eqv? (car path) v) (reverse path)))

  (define (search-bfs level)
    (and (not (null? level))
         (or (search target-path level)  ; има път в level, който завършва на v
             (search-bfs (apply append (map extend-acyclic level))))))

  (search-bfs (list (list u))))