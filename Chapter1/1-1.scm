;;; ex 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;;; ex 1.3
(define (sqr a) (* a a))

(define (sum-sqr a b)
	(+ (sqr a) (sqr b)))

(define (sum-max-sqr a b c)
	(cond
	 ((and (<= a b) (<= a c))
		(sum-sqr b c))
	 ((and (<= b a) (<= b c))
		(sum-sqr a c))
	 (else
		(sum-sqr a b))))

;;; ex 1.7

(define (average x y)
	(/ (+ x y) 2))

(define (good-enough? guess prev)
	(< (/ (abs (- guess prev)) guess) 0.0001))

(define (improve guess x)
	(average guess (/ x guess)))

(define (sqrt-iter guess prev x num)
	(if (good-enough? guess prev)
			guess
			(sqrt-iter (improve guess x) guess x (+ 1 num))))

(define (sqrt x)
	(sqrt-iter 1.0 x x 0))

;; for testing
(define (sqrt-difference x)
	(abs (- x (sqr (sqrt x)))))

;;; ex 1.8
(define (cube-improve guess x)
	(/ (+ (/ x (sqr guess)) (* 2 guess)) 3))

(define (cube-iter guess prev x num)
	(if (good-enough? guess prev)
			guess
			(cube-iter (cube-improve guess x) guess x (+ 1 num))))

(define (cube-root x)
	(cube-iter 1.0 x x 0))

;; for testing
(define (cube x) (* x x x))

(define (cube-difference x)
	(abs (- x (cube (cube-root x)))))


