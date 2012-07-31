;;; ex 1.11

;; recursive version
(define (tri-fib n)
	(if (> 3 n)
			n
			(+ (tri-fib (- n 1))
				 (tri-fib (- n 2))
				 (tri-fib (- n 3)))))

;; iterative version
(define (tri-fib-iterative n)
	(define (iter a b c count)
		(if (= count n)
				c
				(iter b c (+ a b c) (+ count 1))))
	(if (> 3 n)
			n
			(iter 0 1 2 2)))

;;; ex 1.12

;; doesn't count elements outside triangle
(define (pascal-elt n k)
	(if (or (= 0 n) (= 0 k) (= n k))
			1
			(+ (pascal-elt (- n 1) (- k 1))
				 (pascal-elt (- n 1) k))))

;;; ex 1.16
(define (fast-expr b n)
	(define (fast-expr-iter b n a)
		(if (= 0 n)
				a
				(if (even? n)
						(fast-expr-iter (* b b) (/ n 2) a)
						(fast-expr-iter b (- n 1) (* a b)))))
	(fast-expr-iter b n 1))

;;; ex 1.17
;;; auxiliary functions
(define (double x)
	(+ x x))

(define (halve x)
	(/ x 2))

(define (my-* a b)
	(cond ((= 1 b) a)
				((even? b) (my-* (double a) (halve b)))
				(else (+ a (my-* a (- b 1))))))

;;; ex 1.18

(define (fast-* a b)
	(define (fast-*-iter a b c)
		(cond
		 ((= 0 b) c)
		 ((even? b) (fast-*-iter (double a) (halve b) c))
		 (else (fast-*-iter a (- b 1) (+ c a)))))
	(fast-*-iter a b 0))

;;; ex 1.19

(define (sqr a) (* a a))

(define (fib n)
	(define (fib-iter a b p q count)
		(cond ((= 0 count) b)
					((even? count)
					 (fib-iter a
										 b
										 (+ (sqr p) (sqr q)) ; p'
										 (* q (+ (* 2 p) q)) ; q'
										 (/ count 2)))
					(else (fib-iter (+ (* b q) (* a q) (* a p))
													(+ (* b p) (* a q))
													p
													q
													(- count 1)))))
	(fib-iter 1 0 0 1 n))
