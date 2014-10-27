;;; Add macros to interpreter

;;; Order of evaluation:
;;;  - evaluate arguments to procedure before calling it:
;;;    "applicative order"
;;;    - Scheme, Java, C, Algol 60
;;;    - pretty much any language w/ side effects
;;;  - evaluate arguments only when needed, but (at most) once
;;;    "normal order"
;;;  - "Call by name", Algol 68, TOO CREEPY, NEVER AGAIN (sorta)

(define my-eval
  (λ (e env)			; expr + mapping vars->vals
    (cond ((symbol? e)
	   (lookup-var e env))
	  ((pair? e)
	   (cond ((equal? (car e) 'quote) ; special form (quote FOO)
		  (cadr e))
		 ((equal? (car e) 'lambda) ; special form (lambda ARGS BODY)
		  (make-closure (cadr e) (caddr e) env))
		 ((macro? (car e))
		  (my-eval (expand-macro e) env))
		 (else			; procedure call
		  (let ((ee (map (λ (e1) (my-eval e1 env)) e)))
		    (my-apply (car ee) (cdr ee))))))
	  (else e))))			; if not pair, self-evaluatory

(define my-apply
  (λ (proc args)
     (cond ((procedure? proc) (apply proc args))
	   ((equal? (car proc) 'funobj)
	    (my-eval (closure-body proc)
		     (append (map list (closure-vars proc) args)
			     (closure-env proc)))))))

(define make-closure
  (λ (vars body env)
     (list 'funobj vars body env)))

(define closure-vars cadr)
(define closure-body caddr)
(define closure-env  cadddr)

(define lookup-var
  (λ (s ops)
    (cond ((null? ops) (error "variable not found:" s))
	  ((equal? s (caar ops)) (cadr (car ops)))
	  (else (lookup-var s (cdr ops))))))

(define global-environment
  (list (list '+ (λ (x y) (+ x y)))
	(list '* (λ (x y) (* x y)))
	(list '= =)
	(list 'sin sin)
	(list '%if (λ (b t-arm f-arm) (if b t-arm f-arm)))))

;;; Transform (let BINDS BODY) -> ((lambda VARS BODY) VALS...)
(define expand-let
  (λ (e)
     (let ((clauses (cadr e))
	   (body (caddr e)))
       (cons (list 'lambda
		   (map car clauses)
		   body)
	     (map cadr clauses)))))

;;; Transform (if TEST A1 A2) -> ((%if TEST (lambda () A1) (lambda () A2)))
(define expand-if
  (λ (e)
     (list (list '%if
		 (cadr e)
		 (list 'lambda '() (caddr e))
		 (list 'lambda '() (cadddr e))))))

(define macro-alist
  (list (list 'let expand-let)
	(list 'if expand-if)))

(define macro?
  (λ (s)
     (member s (map car macro-alist))))

(define expand-macro
  (λ (e)
     ((cadr (assoc (car e) macro-alist)) e)))

;;; Church Encoding of Natural Numbers

;;; Church encoded number 3
(define c-3
  (λ (f)
     (λ (x)
	(f (f (f x))))))

;;; Church encoded number 1
(define c-1
  (λ (f)
     (λ (x)
	(f x))))

;;; Church encoded number 0
(define c-0
  (λ (f)
     (λ (x)
	x)))

;;; Call f(x) once, then n more times
(define succ
  (λ (n)
     (λ (f)
	(λ (x)
	   ((n f) (f x))))))

;;; Examples:

;; > ((c-3 (λ (xs) (cons 'foo xs))) '())
;; (foo foo foo)

;; > (((succ (succ c-1)) (λ (xs) (cons 'foo xs))) '())
;; (foo foo foo)
