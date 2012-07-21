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

;; don't count elements outside triangle
(define (pascal-elt n k)
	(if (or (= 0 n) (= 0 k) (= n k))
			1
			(+ (pascal-elt (- n 1) (- k 1))
				 (pascal-elt (- n 1) k))))
