;;; What is a "Calculus"?

;;; Calculus = Pebbles (in Latin)

;;; e                 (d e x)
;;; ----------------- -----------------------
;;; x                 1
;;; <number>          0
;;; y (y not x)       0
;;; (+ e1 e2)         (+ (d e1 x) (d e2 x))
;;; (* e1 e2)         (+ (* e1 (d e2 x)) (* (d e1 x) e2))
;;; (recip e)         (* -1 (* (* (recip e) (recip e)) (d e x)))

(define diff-table
  (list (list '+ (lambda (e1 e2 d1 d2) (s+ d1 d2)))
	(list '* (lambda (e1 e2 d1 d2) (s+ (s* e1 d2) (s* d1 e2))))
	(list 'recip (lambda (e d) (s* -1
				       (s* (s* (s-recip e)
					       (s-recip e))
					   d))))
	(list 'sin (lambda (e d) (s* (s-cos e) d)))
	(list 'cos (lambda (e d) (s* -1 (s* (s-sin e) d))))))

(define d
  (lambda (e x)
    (cond ((equal? e x) 1)
	  ((number? e) 0)
	  ((symbol? e) 0)
	  (else (let ((entry (assoc (car e) diff-table)))
		  (cond (entry
			 (apply (cadr entry)
				(append (cdr e)
					(map (lambda (e) (d e x))
					     (cdr e)))))
			(else (error "unknown expression type:" e))))))))

(define s+
  (lambda (e1 e2)
    (cond ((and (number? e1)
		(number? e2))
	   (+ e1 e2))
	  ((equal? e1 0) e2)
	  ((equal? e2 0) e1)
	  ((equal? e1 e2) (s* 2 e1))
	  (else (list '+ e1 e2)))))

(define s*
  (lambda (e1 e2)
    (cond ((and (number? e1)
		(number? e2))
	   (* e1 e2))
	  ((or (equal? e1 0) (equal? e2 0)) 0)
	  ((equal? e1 1) e2)
	  ((equal? e2 1) e1)
	  (else (list '* e1 e2)))))

(define s-sin (lambda (e) (list 'sin e)))
(define s-cos (lambda (e) (list 'cos e)))

;;; Examples:
;;; (d '(+ (* x x) (sin x)))
;;;  -> (+ (* 2 x) (cos x))


;; > (d '(recip (sin (* x x))) 'x)
;; (*
;;  -1
;;  (* (* (recip (sin (* x x))) (recip (sin (* x x)))) (* (cos (* x x)) (* 2 x))))
;; > (d '(* (recip (sin (* x x))) (recip (recip (sin (* x x))))) 'x)
;; (+
;;  (*
;;   (recip (sin (* x x)))
;;   (*
;;    -1
;;    (*
;;     (* (recip (recip (sin (* x x)))) (recip (recip (sin (* x x)))))
;;     (*
;;      -1
;;      (*
;;       (* (recip (sin (* x x))) (recip (sin (* x x))))
;;       (* (cos (* x x)) (* 2 x)))))))
;;  (*
;;   (*
;;    -1
;;    (*
;;     (* (recip (sin (* x x))) (recip (sin (* x x))))
;;     (* (cos (* x x)) (* 2 x))))
;;   (recip (recip (sin (* x x))))))
;; > recip
;; reference to undefined identifier: recip

;;  === context ===
;; /usr/share/racket/collects/racket/private/misc.rkt:87:7

;; > (define recip (lambda (x) (/ 1 x)))
;; > recip
;; #<procedure:recip>
;; > (define foo (lambda (x) (+
;;  (*
;;   (recip (sin (* x x)))
;;   (*
;;    -1
;;    (*
;;     (* (recip (recip (sin (* x x)))) (recip (recip (sin (* x x)))))
;;     (*
;;      -1
;;      (*
;;       (* (recip (sin (* x x))) (recip (sin (* x x))))
;;       (* (cos (* x x)) (* 2 x)))))))
;;  (*
;;   (*
;;    -1
;;    (*
;;     (* (recip (sin (* x x))) (recip (sin (* x x))))
;;     (* (cos (* x x)) (* 2 x))))
;;   (recip (recip (sin (* x x))))))))
;; > (foo 72.3)
;; -5.6843418860808015e-14
;; > (expt 2 48)
;; 281474976710656
;; > (/ (log (expt 2 48)) (log 10))
;; 14.449439791871095
