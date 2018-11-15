(define (fact n)
  (if (= n 0) 1 (* n (fact (- n 1)))))

(define the-empty-stream '())

(define empty-stream? null?)

(define (cons-stream h t)
  (cons h (delay t)))

(define head car)

(define (tail s)
  (force (cdr s)))

(define-syntax mydelay
  (syntax-rules ()
    ((delay x) (lambda () x))))

(define (myforce x) (x))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream h t) (cons h (delay t)))))

(define (enum a b)
  (if (> a b) the-empty-stream
      (cons-stream a (enum (+ a 1) b))))

(define (take n s)
  (if (or (empty-stream? s) (= n 0)) '()
      (cons (head s) (take (- n 1) (tail s)))))

(define (search-stream p s)
  (cond ((empty-stream? s) #f)
        ((p (head s)) s)
        (else (search-stream p (tail s)))))

(define (search-stream p s)
  (and (not (empty-stream? s))
       (or (and (p (head s)) s)
           (search-stream p (tail s)))))

(define (from n)
  (cons-stream n (from (+ n 1))))

(define nats (from 0))

(define (generate-fibs fn fn+1)
  (cons-stream fn (generate-fibs fn+1 (+ fn fn+1))))

(define fibs (generate-fibs 0 1))

(define (map-stream f s)
  (cons-stream (f (head s)) (map-stream f (tail s))))

(define (filter-stream p? s)
  (if (p? (head s)) (cons-stream (head s) (filter-stream p? (tail s)))
      (filter-stream p? (tail s))))

(define evens (filter-stream even? nats))

(define (zip-streams op s1 s2)
  (cons-stream (op (head s1) (head s2))
               (zip-streams op (tail s1) (tail s2))))

(define (map-stream f . ss)
  (cons-stream (apply f (map head ss))
               (apply map-stream f (map tail ss))))

(define ones (cons-stream 1 ones))

;; ??? (define (tail nats) (map-stream + ones nats))
(define nats (cons-stream 0 (map-stream + ones nats)))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (map-stream + fibs (tail fibs)))))

(define (notdivides d)
  (lambda (n)
    (> (remainder n d) 0)))

(define (sieve stream)
  (cons-stream (head stream)
               (filter-stream (notdivides (head stream)) (sieve (tail stream)))))

(define (sieve2 stream)
  (cons-stream (head stream)
               (sieve2 (filter-stream (notdivides (head stream)) (tail stream)))))

(define primes (sieve (from 2)))
(define primes2 (sieve2 (from 2)))