#lang racket

(require fsm)
(require test-engine/racket-tests)

; ; Design and implement in FSM a finite state machine to detect if bbabbab is a substring
; ; in a word w. Follow all the steps of the design recipe. Make sure to use the state
; ; invariants you develop to prove that your machine works.
; 


(define BBABBAB
  (make-dfa '(Q0 Q1 Q2 Q3 Q4 Q5 Q6 Q7)
            '(a b)
            'Q0
            '(Q7)
            '((Q0 a Q0)
              (Q0 b Q1)
              (Q1 a Q0)
              (Q1 b Q2)
              (Q2 a Q3)
              (Q2 b Q2)
              (Q3 a Q0)
              (Q3 b Q4)
              (Q4 a Q0)
              (Q4 b Q5)
              (Q5 a Q6)
              (Q5 b Q2)
              (Q6 a Q0)
              (Q6 b Q7)
              (Q7 a Q7)
              (Q7 b Q7))))

; list-head: list, int -> list
; Purpose: to return all elements of list up to and including the element at index n
(define (list-head list n)
  (reverse (list-tail (reverse list) (- (length list) n))))

; contains-word?: list, list -> bool
; return true if word is present in lst
(define (contains-word? lst word)
  (cond
    [(< (length lst) (length word)) #false]
    [(equal? (list-head lst (length word)) word) #true]
    [else
     (contains-word? (cdr lst) word)]))

(define (Q-0-INV consumed-input) (contains-word? consumed-input '()))
(define (Q-1-INV consumed-input) (contains-word? consumed-input '(b)))
(define (Q-2-INV consumed-input) (contains-word? consumed-input '(b b)))
(define (Q-3-INV consumed-input) (contains-word? consumed-input '(b b a)))
(define (Q-4-INV consumed-input) (contains-word? consumed-input '(b b a b)))
(define (Q-5-INV consumed-input) (contains-word? consumed-input '(b b a b b)))
(define (Q-6-INV consumed-input) (contains-word? consumed-input '(b b a b b a)))
(define (Q-7-INV consumed-input) (contains-word? consumed-input '(b b a b b a b)))

(sm-visualize BBABBAB (list 'Q0 Q-0-INV)
              (list 'Q1 Q-1-INV)
              (list 'Q2 Q-2-INV)
              (list 'Q3 Q-3-INV)
              (list 'Q4 Q-4-INV)
              (list 'Q5 Q-5-INV)
              (list 'Q6 Q-6-INV)
              (list 'Q7 Q-7-INV))                                               

; Let n be a nonnegative integer between 0 and the length of the input word

; Invariant principle: if a preserved invariant P holds for a state q, P(q),
; and state s can be reached in one transition, then P(s) must hold as well

; BASE CASE:
; At starting state Q0, no correct inputs have been consumed

; ASSUME:
; At state Qn, n correct inputs have been consumed

; INDUCTIVE STEP:
; By Invariant Principle, at state Qn+1, n+1 correct inputs have been consumed

; In the case of the input b b a b b a b, after 7 correct inputs are consumed,
; the machine will be in final state Q7, where there are no transitions out of the state


(check-expect (sm-apply BBABBAB '(a a a b a a a a a a b b a b b a b a a a b a a)) 'accept)
(check-expect (sm-apply BBABBAB '(b b b b b a b b a b b b b b b)) 'accept)
(check-expect (sm-apply BBABBAB '(a b b b a b b a b)) 'accept)
(check-expect (sm-apply BBABBAB '(b b a b b a b b a b b a b b a b)) 'accept)
(check-expect (sm-apply BBABBAB '(b b a b b a a b b a b b a a b a b)) 'reject)
(check-expect (sm-apply BBABBAB '()) 'reject)
(check-expect (sm-apply BBABBAB '(a)) 'reject)


; ; The sketch of the proof that finite state machines are closed under union developed
; ; in class stated:
; ; 
; ; .
; ; 
; ; Prove the above by induction on the length of w.
; 



; let L1 and L2 be regular languages
; let M1 = (L1, Σ, δ, s1, F1)
; let M2 = (L2, Σ, δ, s2, F2)
; let M = ((L1 U L2), Σ, δ, s, F)
; let s1, q1 be states in M1
; let s2, q2 be states in M2
; let s, q be states in M
; let δ be the set of rules of M
; let δ contain (s e s1) U (s e s2), δ1, δ2

; BASE CASE:
; let w be length 0 (w is e in this case)
; (s, e)⊢(q, e) for q in F iff (s1, e)⊢(q1, e) for q1 in F1 OR (s2, e)⊢(q2, e) for q2 in F2
; this is to say, if there is an empty transition s->q which is accepted by either M1 or M2,
; M will accept it.


; ASSUME:
; (⊢n meaning n transitions)
; w is length n
; (s1, w)⊢n(q1, e) for q1 in F1
; OR
; (s2, w)⊢n(q2, e) for q2 in F2

; then (s, w)⊢n(q, e) for q in F


; INDUCTIVE STEP:
; w is length n
; qn is a state in M
; p is length 1, where {w, p} is the consumed input

; (s1, w)⊢n(qn, p)
; (qn, p)⊢(q1, e) for q1 in F1
; OR
; (s2, w)⊢n(qn, p)
; (qn, p)⊢(q2, e) for q2 in F2

; then (s, w+p)⊢n+1(q, e) for q in F

(test)
