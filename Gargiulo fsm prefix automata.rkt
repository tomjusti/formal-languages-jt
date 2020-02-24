#lang racket
(require fsm)


; X (listof X) -> bool
; return true if (eqv? X Y) where Y is an element of (listof X)
(define member?
  (lambda (element lst)
    (cond
      [(empty? lst) #f]
      [(eqv? element (car lst)) #t]
      [else (member? element (cdr lst))])))

; ndfa -> listof sym
; return a list of all reachable states from the start state of ndfa
(define (reachable4 ndfa)
  (let*
      ((rules (sm-getrules ndfa)) ; list of rules of the ndfa
       (i (length (sm-getstates ndfa)))) ; used to limit recursion depth
    (local ((define (reach-aux r visited)
             (cond
               [(zero? i) visited]
               [(empty? r) (begin
                             (set! i (- i 1)) ; whenever a pass is completed, i is decreased. At most, recursion can occur one time per state in the machine
                             (reach-aux rules visited))] ; list of rules has been scanned, reset rules for another pass
               [(and (member? (caar r) visited) (not (member? (caddar r) visited))) (reach-aux (cdr r) (cons (caddar r) visited))] ; found a member of visited pointing to a state not already visited
               [else ; did not find member of visited
                (reach-aux (cdr r) visited)])))
      (reach-aux rules (list (sm-getstart ndfa)))))) ; "visited" only contains the start state by default


; sym ndfa -> listof sym
; return a list of all states in ndfa that are on a path to the final states of ndfa
; (these are made final states in the prefix ndfa, as any state not on a path to final would not be reached by a valid prefix)
(define (path-to-final ndfa)
  (let*
      ((rules (sm-getrules ndfa)) ; list of rules of the ndfa
       (i (length (sm-getstates ndfa))) ; used to limit recursion depth
       (reachable (reachable4 ndfa)))
    (local ((define (reach-aux r visited)
              (cond
                [(zero? i) visited]
                [(empty? r) (begin
                              (set! i (- i 1))
                              (reach-aux rules visited))] ; list of rules has been scanned, reset rules for another pass
                [(and (member? (caddar r) visited) (not (member? (caar r) visited)) (member? (caar r) reachable)) (reach-aux (cdr r) (cons (caar r) visited))] ; found a member of visited pointing to a state not already visited
                [else ; did not find member of visited
                 (reach-aux (cdr r) visited)])))
      (reach-aux rules (sm-getfinals ndfa)))))

(define (make-prefix-ndfa ndfa)
  (let*
      ((new-finals (path-to-final ndfa)))
    (make-ndfa (sm-getstates ndfa)
               (sm-getalphabet ndfa)
               (sm-getstart ndfa)
               new-finals
               (sm-getrules ndfa))))


(define C
  (make-ndfa '(Q0 Q1 Q2 Q3 Q4)
             '(a b c)
             'Q0
             '(Q4)
             '((Q0 a Q0)
               (Q0 b Q4)
               (Q0 c Q1)
               (Q0 b Q2)
               (Q2 a Q4)
               (Q3 a Q4))))

(define D
  (make-ndfa '(Q0 Q1 Q2 Q3 Q4)
             '(a b)
             'Q0
             '(Q4)
             '((Q0 a Q1)
               (Q1 b Q2)
               (Q2 b Q3)
               (Q3 a Q4))))


; Let A be an ndfa accepting L
; Let A' be an ndfa accepting Prefix(L)
; Let q be the start state of A
; let F be the set of final states of A
; let F' be the set of final states for A'
; let N(x, y) be every state on the path from x to y
; let k be the state

; BASE CASE:
; L is length 0
; Prefix(L) = '()
; F' = F

; INDUCTIVE STEP:
; F' = F U N(q, k-1)

; SHOW:
; F' = F U N(q, k)
; N(q, k) is all states from q to k


