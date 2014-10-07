;;; Abstract this pattern:

;; (define zip2
;;   (lambda (xs ys)
;;     (my-map2 list xs ys)))

;;; as "filling in 1st arg of 3-ary function"

(define fill-in-1st-of-3
  (lambda (f c)
    (lambda (x y)
      (f c x y))))

(define zip3 (fill-in-1st-of-3 my-map2 list))

;;; 'SOMETHING  is read by Scheme as   (quote SOMETHING)

;;; Examples:

;; > (quote (1 2 3))
;; (1 2 3)
;; > '(1 2 3)
;; (1 2 3)
;; > (car (quote (quote 14)))
;; quote
;; > (car '(quote 14))
;; quote
;; > (car ''14)
;; quote
;; > (cons 'foo '(bar baz))
;; (foo bar baz)
;; > (cons 'foo ''(bar baz))
;; (foo quote (bar baz))

;;; evaluate small language of numeric expressions

;; EXPR ::= NUMBER | (BINOP EXPR EXPR) | (UNARYOP EXPR)
;; BINOP ::= + | - | *
;; UNARYOP ::= sin | cos | exp

(define eval-numexp0
  (lambda (e)
    (if (number? e)
	e
	(if (equal? (car e) '+)
	    (+ (eval-numexp (cadr e))
	       (eval-numexp (caddr e)))
	    (if (equal? (car e) '-)
		(- (eval-numexp (cadr e))
		   (eval-numexp (caddr e)))
		(if (equal? (car e) '*)
		    (* (eval-numexp (cadr e))
		       (eval-numexp (caddr e)))
		    (error "invalid numexp:" e)))))))

;;; COND aka "conditional"
;;;  = syntactic sugar for nested IFs

(define eval-numexp1
  (lambda (e)
    (cond ((number? e) e)
	  ;; arithmetic operators
	  ((and (equal? (car e) '+)
		(= (length e) 3)	; arity check
		)
	   (+ (eval-numexp (cadr e))
	      (eval-numexp (caddr e))))
	  ((equal? (car e) '-)
	   (- (eval-numexp (cadr e))
	      (eval-numexp (caddr e))))
	  ((equal? (car e) '*)
	   (* (eval-numexp (cadr e))
	      (eval-numexp (caddr e))))
	  ((equal? (car e) 'sin)
	   (sin (eval-numexp (cadr e))))
	  ((equal? (car e) 'cos)
	   (cos (eval-numexp (cadr e))))
	  ((equal? (car e) 'exp)
	   (exp (eval-numexp (cadr e))))
	  ;; New cases go here
	  (else (error "invalid numexp:" e)))))

(define eval-numexp
  (lambda (e)
    (cond ((number? e) e)
	  (else
	   (let ((o (car e)))
	     (let ((op (lookup-op o op-table)))
	       (cond ((= (length e) 2)
		      (op (eval-numexp (cadr e))))
		     ((= (length e) 3)
		      (op (eval-numexp (cadr e))
			  (eval-numexp (caddr e))))
		     (else (error "invalid numexp:" e)))))))))

(define op-table
  (list (list '+ (lambda (x y) (+ x y)))
	(list '* (lambda (x y) (* x y)))
	(list 'sin sin)))

(define lookup-op
  (lambda (op ops)
    (if (equal? op (caar ops))
	(cadr (car ops))
	(lookup-op op (cdr ops)))))
