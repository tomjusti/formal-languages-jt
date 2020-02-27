#lang racket
(require fsm)

(define B
  (make-dfa '(Q0 Q1 Q2 Q3 Q4 Q5)
             '(a b)
             'Q0
             '(Q4)
             '((Q0 a Q1)
               (Q1 b Q2)
               (Q2 b Q3)
               (Q3 a Q4)
               (Q5 a Q4)
               (Q5 b Q4))))

(define C
  (make-dfa '(Q0 Q1)
             '(a b)
             'Q0
             '(Q1)
             '((Q0 a Q1))))

(define D
  (make-dfa '(Q0 Q1 Q2 Q3)
             '(a b)
             'Q0
             '(Q1)
             '((Q0 a Q1)
               (Q1 b Q3)
               (Q3 b Q3)
               ;(Q3 a Q1)
               (Q2 b Q0))))


;; member?: X, (list-of-X) -> boolean
;; Purpose: to return true if (eqv? X Y) where Y is an element of (listof X)
(define member?
  (lambda (element lst)
    (cond
      [(empty? lst) #f]
      [(eqv? element (car lst)) #t]
      [else (member? element (cdr lst))])))

;; reachable4: dfa -> (list-of-sym)
;; Purpose: to return a list of all reachable states from the start state of dfa
(define (reachable4 dfa)
  (let*
      ((rules (sm-getrules dfa)) ; list of rules of the dfa
       (i (length (sm-getstates dfa)))) ; used to limit recursion depth
    (local ((define (reach-aux r visited)
             (cond
               [(zero? i) visited]
               [(empty? r) (begin
                             (set! i (- i 1)) ;; whenever a pass is completed, i is decreased
                                              ;; at most, recursion can occur one time per state in the machine
                             (reach-aux rules visited))] ;; list of rules has been scanned, reset rules for another pass
               ;; found a member of visited pointing to a state not already visited
               [(and (member? (caar r) visited) (not (member? (caddar r) visited))) (reach-aux (cdr r) (cons (caddar r) visited))]
               [else ;; did not find member of visited
                (reach-aux (cdr r) visited)])))
      (reach-aux rules (list (sm-getstart dfa)))))) ;; "visited" only contains the start state by default


;; path-to-final: sym, dfa -> (list-of-sym)
;; Purpose: to return a list of all states in dfa that are on a path to the final states of dfa
;; these are made final states in the prefix dfa, as any state not on a path to final would not be reached by a valid prefix
(define (path-to-final dfa)
  (let*
      ((rules (sm-getrules dfa)) ;; list of rules of the dfa
       (i (length (sm-getstates dfa))) ;; used to limit recursion depth
       (reachable (reachable4 dfa)))
    (local ((define (reach-aux r visited)
              (cond
                [(zero? i) visited]
                [(empty? r) (begin
                              (set! i (- i 1))
                              (reach-aux rules visited))] ;; list of rules has been scanned, reset rules for another pass
                [(and (member? (caddar r) visited) (not (member? (caar r) visited)) (member? (caar r) reachable)) (reach-aux (cdr r) (cons (caar r) visited))] ; found a member of visited pointing to a state not already visited
                [else ;; did not find member of visited
                 (reach-aux (cdr r) visited)])))
      (reach-aux rules (sm-getfinals dfa)))))

(define DFA1 (make-dfa '(S U V T D)
                       '(a b)
                       'S
                       '(S V T)
                       `((S a U)
                         (S b U)
                         (U a V)
                         (U b T)
                         (V a D)
                         (V b D)
                         (T a D)
                         (T b U)
                         (D a D)
                         (D b D))))

(define (remove-silly dfa)
  (let*
      ((newrules (new-rules (sm-getrules dfa) (filter (lambda (x) (not (member? x (path-to-final dfa)))) (sm-getstates dfa)))))
    (make-dfa
     (path-to-final dfa)
     (sm-getalphabet dfa)
     (sm-getstart dfa)
     (sm-getfinals dfa)
     newrules
     'no-dead)))

(define new-rules
  (lambda (lor lou)
    (if (null? lor) lor
        (if (or (member? (caar lor) lou) (member? (caddar lor) lou)) (new-rules (cdr lor) lou)
            (cons (car lor) (new-rules (cdr lor) lou))))))

;; PROOF
;; let A and B be grammars
;; let B = {A, n silly rules}
;; let n be a non-negative integer representing the number of silly rules in A

;; BASE CASE:
;; n == 0
;; B = {A, 0 silly rules}
;; B has no silly rules and A has no silly rules
;; B == A
;; There are no silly rules to remove
;; Therefore: (remove-silly B) == A

;; ASSUME:
;; B = {A, k silly rules}
;; A has no silly rules
;; (remove-silly B) == A

;; SHOW:
;; {B, 1 silly rule} = {A, k + 1 silly rules}
;; A has no silly rules
;; (remove-silly {B, 1 silly rule}) ==  A

;; {B, 1 silly rule} = {A, k + 1 silly rules}
;; {A, k + 1 silly rules} = {{A, k silly rules}, 1 silly rule}
;; By assumption, {A, k silly rules} = B
;; {{A, k silly rules}, 1 silly rule} = {B, 1 silly rule}
;; Therefore: (remove-silly {B, 1 silly rule}) ==  A


