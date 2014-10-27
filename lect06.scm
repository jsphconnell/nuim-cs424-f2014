;;; Q: what is the context of evaulating an expression?
;;; Q: what is "inside" a user-defined "anonymous" function?

;;; f : (A,B) -> C       regular binary function
;;; f : A -> (B -> C)    curried binary function (Alonzo Curry)

;;; curry   : ((A,B) -> C) -> (A -> (B -> C))
;;; uncurry : (A -> (B -> C)) -> ((A,B) -> C)

(define curry
  (λ (f)
    (λ (a)
      (λ (b)
	(f a b)))))

(define uncurry
  (λ (f)
    (λ (a b)
      ((f a) b))))

(define c+ (curry +))
(define c* (curry *))

;; Example:
;; > (map (c+ 10) '(1 2 3 4 5 6))
;; (11 12 13 14 15 16)
;; > (map (λ (y) (+ 10 y)) '(1 2 3 4 5 6))
;; (11 12 13 14 15 16)

(define my-eval
  (λ (e env)			; expr + mapping vars->vals
    (cond ((symbol? e)
	   (lookup-var e env))
	  ((pair? e)
	   (cond ((equal? (car e) 'quote) ; special form (quote FOO)
		  (cadr e))
		 ((equal? (car e) 'if) ; special form (if TEST THEN ELSE)
		  (my-eval (if (my-eval (cadr e) env) (caddr e) (cadddr e))
			   env))
		 ((equal? (car e) 'lambda) ; special form (lambda ARGS BODY)
		  (list 'funobj (cadr e) (caddr e) env))
		 (else			; procedure call
		  (let ((ee (map (λ (e1) (my-eval e1 env)) e)))
		    (my-apply (car ee) (cdr ee))))))
	  (else e))))			; if not pair, self-evaluatory

(define my-apply
  (λ (proc args)
    (cond ((procedure? proc)
	   (apply proc args))
	  ((equal? (car proc) 'funobj)
	   (let ((vars (cadr proc))
		 (body (caddr proc))
		 (env (cadddr proc)))
	     (my-eval body (append (map list vars args) env)))))))

(define lookup-var
  (λ (s ops)
    (cond ((null? ops) (error "variable not found:" s))
	  ((equal? s (caar ops)) (cadr (car ops)))
	  (else (lookup-var s (cdr ops))))))

(define global-environment
  (list (list '+ (λ (x y) (+ x y)))
	(list '* (λ (x y) (* x y)))
	(list '= =)
	(list 'sin sin)))

;; > (let ((fact (λ (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 7))
;; reference to undefined identifier: fact

(define test-form '((lambda (fact0) ((fact0 fact0) 7))
		    (lambda (f0)
		      (lambda (n)
			(if (= n 0)
			    1
			    (* n
			       ((f0 f0) (+ n -1))))))))

;; > (my-eval test-form global-environment)
;; 5040

;; Version of the Y combinator for a strict language like Scheme:

(define z
  (λ (f)
     ((λ (f) (f f))
      (λ (g)
	 (f (λ (x) ((g g) x)))))))

(define fact
  (z (λ (f)
	(λ (n)
	   (if (= n 0)
	       1
	       (* n
		  (f (+ n -1))))))))
