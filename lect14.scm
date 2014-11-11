;;; TAIL RECURSION

(define f
  (λ (x)
     (g (u x))))

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
