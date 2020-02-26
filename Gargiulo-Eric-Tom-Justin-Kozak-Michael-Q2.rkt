#lang racket

(require fsm)
(require test-engine/racket-tests)


;; member?: X, (list-of-X) -> boolean
;; Purpose: to return true if (eqv? X Y) where Y is an element of (listof X)
(define member?
  (lambda (element lst)
    (cond
      [(empty? lst) #f]
      [(eqv? element (car lst)) #t]
      [else (member? element (cdr lst))])))

(check-expect (member? 3 '(1 2 3)) #t)
(check-expect (member? 4 '(1 2 3)) #f)
(check-expect (member? 4 '()) #f)

;; reachable-states: ndfa -> (list-of-sym)
;; Purpose: to return a list of all reachable states from the start state of ndfa
(define (reachable-states ndfa)
  (let*
      ((rules (sm-getrules ndfa)) ; list of rules of the ndfa
       (i (length (sm-getstates ndfa)))) ; used to limit recursion depth
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
      (reach-aux rules (list (sm-getstart ndfa)))))) ;; "visited" only contains the start state by default

(define ndfa-from-marco
  (make-ndfa
   '(Q0 Q1 Q2)
   '(a b)
   'Q0
   '(Q1)
   '((Q2 a Q2)
     (Q2 b Q2)
     (Q1 a Q1)
     (Q1 b Q2)
     (Q0 a Q1)
     (Q0 b Q0))))

(check-expect (reachable-states ndfa-from-marco) '(Q2 Q1 Q0))

;; path-to-final: sym, ndfa -> (list-of-sym)
;; Purpose: to return a list of all states in ndfa that are on a path to the final states of ndfa
;; these are made final states in the prefix ndfa, as any state not on a path to final would not be reached by a valid prefix
(define (path-to-final ndfa)
  (let*
      ((rules (sm-getrules ndfa)) ;; list of rules of the ndfa
       (i (length (sm-getstates ndfa))) ;; used to limit recursion depth
       (reachable (reachable-states ndfa)))
    (local ((define (reach-aux r visited)
              (cond
                [(zero? i) visited]
                [(empty? r) (begin
                              (set! i (- i 1))
                              (reach-aux rules visited))] ;; list of rules has been scanned, reset rules for another pass
                [(and (member? (caddar r) visited)
                      (not (member? (caar r) visited))
                      (member? (caar r) reachable)) (reach-aux (cdr r) (cons (caar r) visited))] ; found a member of visited pointing to a state not already visited
                [else ;; did not find member of visited
                 (reach-aux (cdr r) visited)])))
      (reach-aux rules (sm-getfinals ndfa)))))

;; make-prefix-ndfa: ndfa -> ndfa
;; Purpose: to create an ndfa that accepts all prefixes
(define (make-prefix-ndfa ndfa)
  (let*
      ((new-finals (path-to-final ndfa)))
    (make-ndfa (sm-getstates ndfa)
               (sm-getalphabet ndfa)
               (sm-getstart ndfa)
               new-finals
               (sm-getrules ndfa))))

(define A
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

(check-expect (reachable-states A) '(Q2 Q1 Q4 Q0))
(check-expect (path-to-final A) '(Q2 Q0 Q4))

(check-expect (sm-apply A '(a a a a a a a a a a a a b)) 'accept)
(check-expect (sm-apply A '(a a a a a a a a a a a a b a)) 'accept)
(check-expect (sm-apply A '(b)) 'accept)
(check-expect (sm-apply A '(b a)) 'accept)
(check-expect (sm-apply A '(a b)) 'accept)
(check-expect (sm-apply A '(a b a)) 'accept)

(check-expect (sm-apply (make-prefix-ndfa A) '(a a a a a a a a a a a a b)) 'accept)
(check-expect (sm-apply (make-prefix-ndfa A) '(a a a a a a a a a a a a b a)) 'accept)
(check-expect (sm-apply (make-prefix-ndfa A) '(b)) 'accept)
(check-expect (sm-apply (make-prefix-ndfa A) '(b a)) 'accept)
(check-expect (sm-apply (make-prefix-ndfa A) '(a b)) 'accept)
(check-expect (sm-apply (make-prefix-ndfa A) '(a b a)) 'accept)
(check-expect (sm-apply (make-prefix-ndfa A) '()) 'accept)
(check-expect (sm-apply (make-prefix-ndfa A) '(a a a a a a a a a a a a)) 'accept)
(check-expect (sm-apply (make-prefix-ndfa A) '(a a a a a a a a a a a)) 'accept)
(check-expect (sm-apply (make-prefix-ndfa A) '(a a a)) 'accept)
(check-expect (sm-apply (make-prefix-ndfa A) '(a)) 'accept)

(define B
  (make-ndfa '(Q0 Q1 Q2 Q3 Q4)
             '(a b)
             'Q0
             '(Q4)
             '((Q0 a Q1)
               (Q1 b Q2)
               (Q2 b Q3)
               (Q3 a Q4))))

(check-expect (sm-apply B '(a b b a)) 'accept)

(check-expect (sm-apply (make-prefix-ndfa B) '(a b b a)) 'accept)
(check-expect (sm-apply (make-prefix-ndfa B) '(a b b)) 'accept)
(check-expect (sm-apply (make-prefix-ndfa B) '(a b)) 'accept)
(check-expect (sm-apply (make-prefix-ndfa B) '(a)) 'accept)
(check-expect (sm-apply (make-prefix-ndfa B) '()) 'accept)

;; Let A be an ndfa accepting L.
;; Let A' be an ndfa accepting Prefix(L).
;; Let q be the start state of A.
;; Let F be the set of final states of A.
;; Let F' be the set of final states for A'.
;; Let N(x, y) be y steps from x on all paths to the final state of a machine.
;; Let k be the number of states in A.

;; BASE CASE:

;; L has length 0.
;; Prefix(L) = '().
;; F' = F

;; ASSUME:

;; L is length k.
;; F' = F U N(q, k - 1)

;; INDUCTIVE STEP:

;; F' = N(q, k)
;; N(q, k) is all states from q to k.
;; Therefore, A' accepts any prefix of an accepting word in the language of A(L).
;; Therefore, a regular language is closed under Prefix.

(test)