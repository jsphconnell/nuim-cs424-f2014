;;; λ the Ultimate!

;;; Can use λ to:
;;; - define functions
;;; - define recursive functions (via Y combinator)
;;; - pairs (like C structs) (via Church encoding)
;;; - integers (via Church encoding)
;;; - goto
;;; - assignment to variables

;;; Continuation Passing Style = CPS

(define fact
  (λ (n)
     (if (= n 0)
	 1
	 (* n (fact (- n 1))))))

;;; - add extra arg to every procedure, call it on value to return

(define c* (λ (c x y) (c (* x y))))
(define c- (λ (c x y) (c (- x y))))
(define c= (λ (c1 c2 x y) (if (= x y) (c1) (c2))))

(define cfact
  (λ (c n)
     (c= (λ () (c 1))
	 (λ ()
	    (c- (λ (nm1)
		   (cfact (λ (fnm1)
			     (c* c n fnm1))
			  nm1))
		n
		1))
	 n 0)))

;;; CPS makes explicit:
;;; - order of evaluation
;;; - stack, represented as a chain of continuations
;;; - tail calls in the original function are calls with input
;;;   continuation in the CPS-transformed function (i.e., c* above)
;;; - work to be done *after* calling a procedure
;;; - all temporaries are given names

;;; As a consequence of "all calls are tail calls", all arguments to a
;;; procedure are either:
;;; - constant (e.g., 1)
;;; - variable
;;; - λ expression

;;; CPS = SSA

(define hypot (λ (x y) (sqrt (+ (expt x 2) (expt y 2)))))

(define chypot
  (λ (c x y) (cexpt (λ (x2)
		       (cexpt (λ (y2)
				 (c+ (λ (x2py2)
					(csqrt c x2py2))
				     x2 y2))
			      y 2))
		    x 2)))

(define cexpt (λ (c x y) (c (expt x y))))
(define c+ (λ (c x y) (c (+ x y))))
(define csqrt (λ (c x) (c (sqrt x))))
