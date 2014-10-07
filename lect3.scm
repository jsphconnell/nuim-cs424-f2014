;;; construct a pair: cons
;;; accessors: car, cdr

;;; Printed representation of a pair:
;; > (cons 8 9)
;; (8 . 9)

(define i4 (cons 1 (cons 2 (cons 3 (cons 4 null)))))

;; > (car (cdr i4))
;; 2
;; > (car (cdr (cdr i4)))
;; 3
;; > (cadr i4)
;; 2
;; > (caddr i4)
;; 3

(define i3a (cons 1 (cons (cons 2 (cons 3 null)) (cons 4 null))))
;; (1 (2 3) 4)

;; > (cadadr i3a)
;; 3

;; > (quote (cons 3 null))
;; (cons 3 null)

;; Manual: R6RS,
;; "Revised Revised Revised Revised Revised Revised Report on
;;  the algorithmic programming language Scheme"

(define leaf-sum
  (lambda (s)
    (if (number? s)
	s
	(+ (leaf-sum (car s))
	   (leaf-sum (cdr s))))))

(define my-map
  (lambda (f xs)
    (if (null? xs)
	null
	(cons (f (car xs))
	      (my-map f (cdr xs))))))

(define zip
  (lambda (xs ys)
    (if (or (null? xs) (null? ys))
	null
	(cons (list (car xs) (car ys))
	      (zip (cdr xs) (cdr ys))))))


(define my-map2
  (lambda (f xs ys)
    (if (or (null? xs) (null? ys))
	null
	(cons (f (car xs) (car ys))
	      (my-map2 f (cdr xs) (cdr ys))))))


(define zip2
  (lambda (xs ys)
    (my-map2 list xs ys)))
