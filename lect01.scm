;;; Comments start with ;

(define var 3)

;;; EXPR ::= NUMBER | CALL | LambdaExpression | IfExpr | something else
;;; CALL ::= '(' FUNCTION ARGS* ')'
;;; LambdaExpression ::= (lambda (VAR*) EXPR)
;;; IfExpr ::= (if TEST EXP EXP)

(define a 2)
(define b 3)
(define c 5)

;; (+ a (* b c))
;;  vs
;; (* (+ a b) c)

(define sqrp1 (lambda (x) (+ 1 (* x x))))
;; > (sqrp1 7)
;; 50

(define fact
  (lambda (n)
    (if (= n 0)
	1
	(* n (fact (- n 1))))))
