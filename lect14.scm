;;; TAIL RECURSION

(define f
  (λ (x)
     (g (u x))))

;;; in body of F, the call to U is *not*
;;; "tail recursive", the call to G *is*
;;; "tail recursive".

;;; "tail recursive call" = "tail call"

;;; "Machine Code" for F

;;; Because "ret" = pop address off stack and jump to it ...
;;; apply "peephole optimization"
;;;   {push A ; jmp X ; A: ; ret} ⤳ {jmp X}

;; f:	; x is in r1
;;	; marshal args to u (1st arg to u is 1st arg to us)
;;	push f1
;;	jmp u
;; f1:
;;	; marshal args to g (return value from u to 1st arg to g)
;;	jmp g

;; g:	; arg in r3
;;	...
;;	ret

;; u:	; arg in r2
;;	...
;;	ret

;;; Space requirements (stack) is O(n), i.e., O(1) for each stack
;;; frame holding an invocation of fact1.
(define fact1
  (λ (n)
     (if (= n 0)
	 1
	 (* n (fact1 (- n 1))))))

(define fact2
  (λ (n) (fact2-aux n 1)))

;;; Theorem:
;;;  (fact2-aux n a) = n! * a

;;; Proof:
;;;  by induction on n
;;; case n=0
;;;  (fact2-aux 0 a) = a
;;; case n>0
;;;  (fact2-aux n a)
;;;       = (fact2-aux (- n 1) (* n a))
;;;       = (n-1)! * (n*a)
;;;       = n*(n-1)! * a
;;;       = n! * a
;;; QED

;;; Space requirements (stack) is O(1)

(define fact2-aux
  (λ (n a)
     (if (= n 0)
	 a
	 (fact2-aux (- n 1)
		    (* n a)))))

;;; Q: What is a procedure call?
;;; A: (a) Marshall arguments
;;;    (b) Push return address
;;;    (c) Goto address of procedure

;;; If tail call, skip step (b).
;;; If no arguments, skip step (a).

;;; So, tail call to procedure w/ zero args = GOTO.
  
;; /* look for x in sorted array a[N], return index */
;; int search (int x)
;; {
;;   int lo = 0;
;;   int hi = N;
;;  top:
;;   if (lo == hi)
;;     return lo;
;;   else
;;     {
;;       int mi = (lo + hi)/2;
;;       if (a[mi] < x)
;; 	{
;; 	  lo = mi+1;
;; 	  goto top;
;; 	}
;;       else if (a[mi] > x)
;; 	{
;; 	  hi = mi;
;; 	  goto top;
;; 	}
;;       else
;; 	return mi;
;;     }
;; }

;;; Mechanically translate code with:
;;;  - goto statement
;;;  - assignment statements
;;; into "purely functional" code, with only procedure call/return.

(define search
  (λ (x)
     (s1 x 0 N)))

(define s1
  (λ (x lo hi)
     (cond ((= lo hi) lo)
	   (else (s2 (floor (/ (+ lo hi) 2))
		     lo hi x))))) 

(define s2
  (λ (mi lo hi x)
     (cond ((< (aref a mi) x) (s1 x (+ mi 1) hi))
	   ((> (aref a mi) x) (s1 x lo mi))
	   (else mi))))
