;;; surprise!!!  http://github.com/barak/nuim-cs424-f2014

;;; A Meta-Circular Scheme Interpreter

(define my-eval
  (lambda (e)
    (cond ((number? e) e)
	  ;; ((boolean? e) e)
	  ((symbol? e)
	   (lookup-var e global-environment))
	  ((pair? e)			
	   (cond ((equal? (car e) 'quote) ; special form (quote FOO)
		  (cadr e))
		 ((equal? (car e) 'if) ; special form (if TEST THEN ELSE)
		  (my-eval (if (my-eval (cadr e))
			       (caddr e)
			       (cadddr e))))
		 ((equal? (car e) 'lambda) ; special form (lambda ARGS BODY)
		  (list 'funobj (cadr e) (caddr e)))
		 (else			; procedure call
		  (let ((u-proc (car e))
			(u-args (cdr e)))
		    (let ((proc (my-eval u-proc))
			  (args (map my-eval u-args)))
		      (my-apply proc args))))))
	  (else (error "invalid expression:" e)))))

(define my-apply
  (lambda (proc args)
    (cond ((procedure? proc)
	   (let ((n (length args)))
	     (cond ((= n 0) (proc))
		   ((= n 1) (proc (car args)))
		   ((= n 2) (proc (car args) (cadr args)))
		   ((= n 3) (proc (car args) (cadr args) (caddr args)))
		   ((= n 4) (proc (car args) (cadr args) (caddr args) (cadddr args)))
		   (else (error "too many args:" n)))))
	  ((equal? (car proc) 'funobj)
	   (let ((vars (cadr proc))
		 (body (caddr proc)))
	     ;; need to bind VARS to ARGS
	     (my-eval body))))))

(define lookup-var
  (lambda (s ops)
    (cond ((null? ops) (error "variable not found:" s))
	  ((equal? s (caar ops)) (cadr (car ops)))
	  (else (lookup-var s (cdr ops))))))

(define global-environment
  (list (list '+ (lambda (x y) (+ x y)))
	(list '* (lambda (x y) (* x y)))
	(list 'sin sin)))
