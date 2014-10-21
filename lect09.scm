;;; Implement "Streams" in Scheme

;;; Stream = Lazy List

;;; Stream API, representation

;;; representation of a stream:
;;;   - function of zero arguments
;;;   - when called, returns either
;;;      - empty list (stream was empty)
;;;      - list of two elements
;;;        - first elt of stream, as value of function of zero args
;;;        - rest of stream (i.e., a stream)

(define s-cons* (λ (ta d) (λ () (list ta d)))) ; takes A as "thunk"
(define s-cons (λ (a d) (s-cons* (λ () a) d))) ; takes A as "regular" value
(define s-car (λ (s) ((car (s)))))	; returns "regular" value
(define s-cdr (λ (s) (cadr (s))))	; returns stream
(define s-null? (λ (s) (null? (s))))
(define s-null (λ () '()))

;;; Definition: a "thunk" for EXPR is (λ () EXPR)

(define list-to-stream
  (λ (xs)
     (if (null? xs)
	 s-null
	 (s-cons (car xs) (list-to-stream (cdr xs))))))

(define s-map
  (λ (f s)
     (λ ()
	(if (s-null? s)
	    '()
	    (list (λ () (f (s-car s)))
		  (s-map f (s-cdr s)))))))

(define s-zip-with
  (λ (f s1 s2)
     (λ ()
	(if (or (s-null? s1) (s-null? s2))
	    '()
	    (list (λ () (f (s-car s1) (s-car s2)))
		  (s-zip-with f (s-cdr s1) (s-cdr s2)))))))

(define s-const
  (λ (x)
     (define s-xs (λ () (list (λ () x) s-xs)))
     s-xs))

;;; yield first N elements of S as list
(define s-take
  (λ (n s)
     (cond ((zero? n) '())
	   ((s-null? s) '())
	   (else (cons (s-car s) (s-take (- n 1) (s-cdr s)))))))

(define s-nat-from-n
  (λ (n)
     (λ () (list (λ () n) (s-nat-from-n (+ n 1))))))

(define s-nat (s-nat-from-n 0))

;;; Examples

;; > (s-take 10 (s-const 7))
;; (7 7 7 7 7 7 7 7 7 7)
;; > (s-take 10 (s-zip-with / (s-zip-with * s-nat (s-cdr s-nat)) (s-const 2)))
;; (0 1 3 6 10 15 21 28 36 45)

(define fib0
  (λ (n)
     (cond ((= n 0) 1)
	   ((= n 1) 1)
	   (else (+ (fib0 (- n 1)) (fib0 (- n 2)))))))

;;            FIB = [  1 1 2 3.. ]
;;    (s-cdr FIB) = [  1 2 3 ... ]
;;              +   ------------------
;;                  [  2 3 5  ... ]


;;; Want this, but with laziness
;; (define s-fib
;;   (s-cons 1
;; 	  (s-cons 1
;; 		  (s-zip-with +
;; 			      s-fib
;; 			      (s-cdr s-fib)))))

(define s-fib
  (λ ()
     (list (λ () 1)
	   (λ ()
	      (list (λ () 1)
		    (s-zip-with + s-fib (s-cdr s-fib)))))))

;;; Slow due to no caching
;; > (s-take 10 s-fib)
;; (1 1 2 3 5 8 13 21 34 55)
;;;  ... should add a cache!
;;;  ... use DELAY / FORCE promises or side effect our own cache ...
