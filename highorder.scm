(define (fixed-point? f x) (= (f x) x))

(define (sum a b term next)
  (if (> a b) 0 (+ (term a) (sum (next a) b term next))))

(define (product a b term next)
  (if (> a b) 1 (* (term a) (product (next a) b term next))))

(define (accumulate op nv a b term next)
    (if (> a b) nv (op (term a) (accumulate op nv (next a) b term next))))

; P(x) = x^n + 2x^(n-1) + ... + nx + (n + 1)
; term(i) = (n-i+1)x^i

(define (1+ x) (+ x 1))

; > (p 3 1)
; 5
; > (p 5 2)
; 38
; > (p 2 7)
; 502
; > 

(define (p x n)
  (define (term i) (* (1+ (- n i)) (expt x i)))
  (accumulate + 0 0 n term 1+))

(define (id x) x)

(define (p x n)
  (define (op u v) (+ (* v x) u))
  (accumulate op 0 1 (1+ n) id 1+))

(define (accumulate op nv a b term next)
  (if (> a b) nv (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b) nv (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (p x n)
  (define (op u v) (+ (* u x) v))
  (accumulate-i op 0 1 (1+ n) id 1+))

(define (fact n)
  (accumulate * 1 1 n (lambda (i) i) 1+))

(define (pow x n)
  (accumulate * 1 1 n (lambda (i) x) 1+))

(define (myexp x n)
  (accumulate + 0 0 n (lambda (i)
                        (/ (pow x i) (fact i))) 1+))

(define (myexp x n)
  (accumulate (lambda (u v) (+ 1 (* u v)))
               0 1 n (lambda (i) (/ x i)) 1+))

(define (exists p? a b)
  (accumulate (lambda (u v) (or u v)) #f a b p? 1+))

(define (repeated f n)
  (lambda (x)
     (if (= n 0) x
         (f ((repeated f (- n 1)) x)))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 0) id
      (compose f (repeated f (- n 1)))))

(define (repeated f n)
  (accumulate compose id 1 n (lambda (i) f) 1+))

(define (derive f dx)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (derive-n f n dx)
  (if (= n 0) f
      (derive (derive-n f (- n 1) dx) dx)))

(define (derive-n f n dx)
  ((repeated (lambda (g) (derive g dx)) n) f))