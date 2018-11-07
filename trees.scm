(define make-tree list)
(define (tree? t)
  (and (list? t)
       (or (null? t)
           (and 
            (= (length t) 3)
            (tree? (cadr t))
            (tree? (caddr t))))))

(define empty-tree '())
(define empty-tree? null?)

(define (check-tree f)
  (lambda (t)
    (if (tree? t) (f t) 'not-a-tree)))

(define root-tree (check-tree car))
(define left-tree (check-tree cadr))
(define right-tree (check-tree caddr))

(define (make-leaf x) (make-tree x empty-tree
                                   empty-tree))

(define t (make-tree 1 (make-leaf 2)
                       (make-tree 3
                                  (make-leaf 4)
                                  (make-leaf 5))))

(define (depth-tree t)
  (if (empty-tree? t) 0
      (+ 1 (max (depth-tree (left-tree t))
                (depth-tree (right-tree t))))))

(define (member-tree-= =?)
  (lambda (x t)
    (cond ((empty-tree? t) #f)
          ((=? (root-tree t) x) t)
          (else (or (member-tree x (left-tree t)) (member-tree x (right-tree t)))))))

(define memq-tree (member-tree-= eq?))
(define memqv-tree (member-tree-= eqv?))
(define member-tree (member-tree-= equal?))

(define (path-tree x t)
  (cond ((empty-tree? t) #f)
        ((equal? (root-tree t) x) (list x))
        (else (cons#f (root-tree t)
                        (or (path-tree x (left-tree t))
                            (path-tree x (right-tree t)))))))

(define (cons#f h t) (and t (cons h t)))
               
