#lang racket
(require fsm)
(require test-engine/racket-tests)

; Problem Analysis
; Stack is used to contain a's,
; either one or two a's are nondeterministically
; pushed onto the stack. When a b is encountered, the
; top of the stack is popped.

; ID Conditions that must be tracked as input is consumed
; and associate a state with each condition

; Q0: no input  consumed, starting state
; Q1: 0 or more a's are pushed onto the stack
; Q2: length of stack + b's in consumed input always between m and 2m (where m is number of a's in consumed-input)

; in Q0, consumed-input and stack are both empty
(define (Q0-INV consumed-input stack)
  (and (empty? consumed-input) (empty? stack)))

; in Q1, stack and consumed-input are always length 0 or more
(define (Q1-INV consumed-input stack)
  (and (>= (length consumed-input) 0)
       (>= (length stack) 0)))

; in Q2, length of stack + number of b's in consumed-input is always between n and 2m (where m is number of a's in consumed-input)
(define (Q2-INV consumed-input stack)
  (and (>= (+ (length stack) (length (filter (lambda (x) (eq? x 'b)) consumed-input)))
           (length (filter (lambda (x) (eq? x 'a)) consumed-input)))
       (<= (+ (length stack) (length (filter (lambda (x) (eq? x 'b)) consumed-input)))
           (* 2 (length (filter (lambda (x) (eq? x 'a)) consumed-input))))))

(define a^mb^n_m<=n<=2m
  (make-ndpda
   '(Q0 Q1 Q2)
   '(a b)
   '(a)
   'Q0
   '(Q2)
   `(((Q0 ,EMP ,EMP) (Q1 ,EMP))
     ((Q1 ,EMP ,EMP) (Q2 ,EMP))
     ((Q1 a ,EMP) (Q1 (a)))
     ((Q1 a ,EMP) (Q1 (a a)))
     ((Q2 b (a)) (Q2 ,EMP)))))

; Base Case:
; 
; t = 0
; 
; Q0:
; 
; nothing is read and the stack is currently empty because nothing has been pushed
; Thereofre, Q0-INV holds
; 
; Q1:
; 
; ((Q0 ,EMP ,EMP) (Q1 ,EMP))
; 0 a's are in the consumed input so no a was pushed onto the stack
; Therefore, Q1-INV holds
; 
; Q2:
; 
; ((Q1 ,EMP ,EMP) (Q2 ,EMP))
; no a's are in the consumed input so no a was pushed onto the stack which means no b in the consumed input
; to pop an a off the stack
; length of stack = 0
; b's in consumed input = 0
; length of stack + b's in consumed input always between m and 2m (where m is number of a's in consumed-input)
; 0 + 0 = 0
; m = 0 <= 0 <= 2m = 2 * 0 = 0
; Therefore, Q2-INV holds
; 
; Assume: INVs hold for t = k
; Show INVs hold for t = k + 1
; 
; Q1:
; 
; ((Q1 a ,EMP) (Q1 (a a)))
; nondeterministically sees ahead how many a's it would need to push
; stack is too short just before it does its last push
; stack has more than zero a's
; Q1-INV holds
; 
; ((Q1 a ,EMP) (Q1 (a)))
; an a is added onto the stack which means stack has more than zero a's
; Q1-INV holds
; 
; Q2:
; 
; ((Q2 b (a)) (Q2 ,EMP))
; a b is now in the consumed input
; an a is popped off the stack
; length of stack + b's in consumed-input is always between m and 2m (where m is number of a's in consumed-input)
; b's in consumed input increases by 1 while length of stack decreases by 1
; Therefore, Q2-INV holds
; 
; Theorem:
; w is an element of L(a^mb^n_m<=n<=2m) if and only if w is an element of L
; 
; Assume w is an element of L(a^mb^n_m<=n<=2m)
; Invariants hold after consuming w
; w is an element of L
; 
; Assume w is an element of L
; By construction of a^mb^n_m<=n<=2m, m a's are consumed and m to 2m a's are pushed onto the stack in Q1
; Then, a^mb^n_m<=n<=2m moves to Q2 where a b is read
; When a b is in the consumed input, an a is pushed off the stack
; This continues until the stack is empty
; w is accepted
; 
; Theorem:
; w is not an element of L(a^mb^n_m<=n<=2m) if and only if w is not an element of L
; 
; Assume w is not an element of L(a^mb^n_m<=n<=2m)
; if a letter of w is not a part of the alphabet, then it will not reach Q1 as Q1 can only take in 0 or more a's
; 
; Assume w is not an element of L
; Q1 invariant doesn't hold, so Q2 invariant also doesn't hold
; Therefore, w is not an element of L and L(a^mb^n_m<=n<=2m)


; ------------------------------------------

; fsa->ndpda: fsa -> ndpda
; Purpose: to take as input a fsa and return an ndpda that accepts the same language
(define (fsa->ndpda fsa)
  (make-ndpda
   (sm-getstates fsa)
   (sm-getalphabet fsa)
   '()
   (sm-getstart fsa)
   (sm-getfinals fsa)
   (convert-rules (sm-getrules fsa))))

; convert-rules: (list of rules) -> (list of rules)
; Purpose: to convert a list of fsa rules to pda rules
(define (convert-rules lor)
  (cond
    [(empty? lor) '()]
    [else (cons (convert-rules-aux (car lor)) (convert-rules (cdr lor)))]))

(define (convert-rules-aux rule) (list (list (car rule) (cadr rule) EMP) (list (caddr rule) EMP)))

(define NDFA1
  (make-ndfa '(Q0 Q1)
            '(a b)
            'Q0
            '(Q0)
            `((Q0 a Q0)
              (Q0 b Q0)
              (Q0 b Q1)
              (Q1 a Q0)
              (Q1 b Q0))))


(define NDPDA1
  (make-ndpda '(Q0 Q1)
            '(a b)
            '()
            'Q0
            '(Q0)
            `(((Q0 a ,EMP) (Q0 ,EMP))
              ((Q0 b ,EMP) (Q0 ,EMP))
              ((Q0 b ,EMP) (Q1 ,EMP))
              ((Q1 a ,EMP) (Q0 ,EMP))
              ((Q1 b ,EMP) (Q0 ,EMP)))))

; Prove an FSA can be viewed as a PDA
; 
; Let M be an NDFA and M' a PDA
; (p, u, q) ∈ ΔM ⇒ ((p, u, ε), (q, ε)) ∈ ΔM’
; 
; Theorem:
; w is an element of L((fsa->ndpda FSA-X)) if and only if w is an element of L(FSA-X)
; 
; Assume w is an element of L(FSA-X)
; There is a path from the starting state to a final state
; Therefore w is an element of L(FSA-X)
; 
; Assume w is an element of L((fsa->ndpda FSA-X))
; Stack is empty once input is consumed so w is accepted
; Therefore w is an element of L((fsa->ndpda FSA-X))
; 
; Theorem:
; w is not an element of L((fsa->ndpda FSA-X)) if and only if w is not an element of L(FSA-X)
; 
; Assume w is not an element of L(FSA-X)
; Machine rejects the word because there's no transition to satisfy it
; Therefore, w is not an element of L(FSA-X)
; 
; Assume w is not an element of L((fsa->ndpda FSA-X))
; Machine rejects because a transition failed to satisfy the input
; According to above: (p, u, q) ∈ ΔM ⇒ ((p, u, ε), (q, ε)) ∈ ΔM’
; Therefore, w is not an element of L((fsa->ndpda FSA-X))
; 
; Therefore, FSA can be viewed as a PDA


; Unit Tests
(check-expect
 (sm-apply a^mb^n_m<=n<=2m '()) 'accept) ; empty input
(check-expect
 (sm-apply a^mb^n_m<=n<=2m '(a b)) 'accept) ; m = n
(check-expect
 (sm-apply a^mb^n_m<=n<=2m '(a a b b)) 'accept) ; m = n

(check-expect
 (sm-apply a^mb^n_m<=n<=2m '()) 'accept) ; empty input
(check-expect
 (sm-apply a^mb^n_m<=n<=2m '(a b b)) 'accept) ; m + 1 = n
(check-expect
 (sm-apply a^mb^n_m<=n<=2m '(a a b b b)) 'accept) ; m + 1 = n

(check-expect
 (sm-apply a^mb^n_m<=n<=2m '()) 'accept) ; empty input
(check-expect
 (sm-apply a^mb^n_m<=n<=2m '(a b b b)) 'reject) ; m + 2 = n
(check-expect
 (sm-apply a^mb^n_m<=n<=2m '(a a b b b b)) 'accept) ; m + 2 = n

(check-expect (sm-testequiv? NDFA1 (fsa->ndpda NDFA1)) #t)
(check-expect (sm-testequiv? NDPDA1 (fsa->ndpda NDFA1)) #t)
(check-expect (sm-testequiv? NDPDA1 (fsa->ndpda NDFA1)) #t)

(test)
