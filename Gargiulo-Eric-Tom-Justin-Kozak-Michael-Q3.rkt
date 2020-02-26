#lang racket
(require fsm)

(define B
  (make-ndfa '(Q0 Q1 Q2 Q3 Q4 Q5)
             '(a b)
             'Q0
             '(Q4)
             '((Q0 a Q1)
               (Q1 b Q2)
               (Q2 b Q3)
               (Q3 a Q4)
               (Q5 a Q4)
               (Q5 b Q4))))


;; member?: X, (list-of-X) -> boolean
;; Purpose: to return true if (eqv? X Y) where Y is an element of (listof X)
(define member?
  (lambda (element lst)
    (cond
      [(empty? lst) #f]
      [(eqv? element (car lst)) #t]
      [else (member? element (cdr lst))])))

;; reachable4: ndfa -> (list-of-sym)
;; Purpose: to return a list of all reachable states from the start state of ndfa
(define (reachable4 ndfa)
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


;; path-to-final: sym, ndfa -> (list-of-sym)
;; Purpose: to return a list of all states in ndfa that are on a path to the final states of ndfa
;; these are made final states in the prefix ndfa, as any state not on a path to final would not be reached by a valid prefix
(define (path-to-final ndfa)
  (let*
      ((rules (sm-getrules ndfa)) ;; list of rules of the ndfa
       (i (length (sm-getstates ndfa))) ;; used to limit recursion depth
       (reachable (reachable4 ndfa)))
    (local ((define (reach-aux r visited)
              (cond
                [(zero? i) visited]
                [(empty? r) (begin
                              (set! i (- i 1))
                              (reach-aux rules visited))] ;; list of rules has been scanned, reset rules for another pass
                [(and (member? (caddar r) visited) (not (member? (caar r) visited)) (member? (caar r) reachable)) (reach-aux (cdr r) (cons (caar r) visited))] ; found a member of visited pointing to a state not already visited
                [else ;; did not find member of visited
                 (reach-aux (cdr r) visited)])))
      (reach-aux rules (sm-getfinals ndfa)))))


(define (returnsilly ndfa)
  (let*
      ((states (sm-getstates ndfa))
       (reachable (reachable4 ndfa)))
    (filter (lambda (x) (not (member? x reachable))) states)))

(define (nonsilly-grammar ndfa)
  (sm->grammar (make-ndfa (reachable4 ndfa)
                          (sm-getalphabet ndfa)
                          (sm-getstart ndfa)
                          (sm-getfinals ndfa)
                          (sm-getrules ndfa))))

(returnsilly B)