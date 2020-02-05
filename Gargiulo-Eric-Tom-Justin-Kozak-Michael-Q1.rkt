;; Formal Languages
;; Quiz 1
;; Eric Gargiulo, Justin Tom & Michael Kozak

#lang racket
(require fsm)
(require test-engine/racket-tests)

(define QUIZ0
  (make-dfa '(Q0 Q1 Q2)
            '(a b)
            'Q0
            '(Q0)
            '((Q2 a Q2)
              (Q0 a Q0)
              (Q0 b Q1)
              (Q1 a Q1)
              (Q1 b Q0))))

(define QUIZ0_
  (make-dfa '(Q0 Q1 Q2)
            '(a b)
            'Q0
            '(Q0)
            '((Q0 a Q0)
              (Q0 b Q1)
              (Q1 a Q1)
              (Q1 b Q0))))

(define QUIZ1
  (make-dfa '(Q0 Q1 Q2)
            '(a b)
            'Q2
            '(Q0)
            '((Q2 a Q2)
              (Q0 a Q0)
              (Q0 b Q1)
              (Q1 a Q1)
              (Q1 b Q0))))

(define QUIZ1_
  (make-dfa '(Q0 Q1 Q2)
            '(a b)
            'Q2
            '(Q0)
            '((Q2 a Q2))))

(define QUIZ2
  (make-dfa '(Q0 Q1 Q2 Q3)
            '(a b)
            'Q0
            '(Q2)
            '((Q0 a Q1)
              (Q0 b Q3)
              (Q1 a Q1)
              (Q1 b Q2)
              (Q2 b Q2)
              (Q2 a Q3)
              (Q3 a Q3)
              (Q3 b Q3))))

(define QUIZ2_
  (make-dfa '(Q0 Q1 Q2 Q3)
            '(a b)
            'Q0
            '(Q2)
            '((Q0 a Q1)
              (Q0 b Q3)
              (Q1 a Q1)
              (Q1 b Q2)
              (Q2 b Q2)
              (Q2 a Q3)
              (Q3 a Q3)
              (Q3 b Q3))))

(define QUIZ3
  (make-dfa '(Q0 Q1 Q2 Q3)
            '(a b)
            'Q0
            '(Q2)
            '((Q0 a Q1)
              (Q3 b Q3)
              (Q1 a Q1)
              (Q1 b Q2)
              (Q2 b Q2)
              (Q3 a Q3))))

(define QUIZ3_
  (make-dfa '(Q0 Q1 Q2 Q3)
            '(a b)
            'Q0
            '(Q2)
            '((Q0 a Q1)
              (Q1 a Q1)
              (Q1 b Q2)
              (Q2 b Q2))))

(define QUIZ4
  (make-dfa '(Q0 Q1 Q2)
            '(a b)
            'Q0
            '(Q0 Q1)
            '((Q0 a Q1)
              (Q0 b Q0)
              (Q1 a Q0)
              (Q1 b Q1)
              (Q2 a Q2)
              (Q2 b Q2))))

(define QUIZ4_
  (make-dfa '(Q0 Q1)
            '(a b)
            'Q0
            '(Q0 Q1)
            '((Q0 a Q1)
              (Q0 b Q0)
              (Q1 a Q0)
              (Q1 b Q1))))

;; member?: x, (list of x) -> boolean
;; Purpose: to return a boolean if a specific element is in a list
(define member?
  (lambda (element lst)
    (cond
      [(empty? lst) #f]
      [(eqv? element (car lst)) #t]
      [else (member? element (cdr lst))])))

(check-expect (member? 3 '(1 2 3)) #t)
(check-expect (member? 4 '(1 2 3)) #f)
(check-expect (member? 4 '()) #f)

;; reachable: (list of rules), (list of sm-getstart) -> (list of reachable states)
;; Purpose: to return a list of reachable states
;; Accumulative Invariant: res is the list of reachable states
(define reachable
  (lambda (lor res)
    (cond
      [(null? lor) res]
      [(and (member? (caar lor) res) (not (member? (caddar lor) res))) (reachable (cdr lor) (cons (caddar lor) res))]
      [else (reachable (cdr lor) res)])))

(check-expect (reachable (sm-getrules QUIZ0) (list (sm-getstart QUIZ0))) '(Q1 Q0))
(check-expect (reachable (sm-getrules QUIZ1) (list (sm-getstart QUIZ1))) '(ds Q2))
(check-expect (reachable (sm-getrules QUIZ2) (list (sm-getstart QUIZ2))) '(Q2 Q3 Q1 Q0))
(check-expect (reachable (sm-getrules QUIZ3) (list (sm-getstart QUIZ3))) '(ds Q2 Q1 Q0))
(check-expect (reachable (sm-getrules QUIZ4) (list (sm-getstart QUIZ4))) '(Q1 Q0))

;; unreachable: dfa -> (list of unreachable states)
;; Purpose: to return a list of the unreachable states in dfa
(define unreachable
  (lambda (dfa)
    (remove-duplicates (remove* (reachable (sm-getrules dfa) (list (sm-getstart dfa))) (map car (sm-getrules dfa))))))

(check-expect (unreachable QUIZ0) '(Q2 ds))
(check-expect (unreachable QUIZ1) '(Q0 Q1))
(check-expect (unreachable QUIZ2) '(ds))
(check-expect (unreachable QUIZ3) '(Q3))
(check-expect (unreachable QUIZ4) '(Q2 ds))

;; new-rules: (list of rules) (list of unreachable states) -> (list of rules)
;; Purpose: to return a list of new rules that do not involve the unreachable states
(define new-rules
  (lambda (lor lou)
    (if (null? lor) lor
        (if (or (member? (caar lor) lou) (member? (caddar lor) lou)) (new-rules (cdr lor) lou)
            (cons (car lor) (new-rules (cdr lor) lou))))))

(check-expect (new-rules (sm-getrules QUIZ0) (unreachable QUIZ0)) '((Q0 a Q0) (Q0 b Q1) (Q1 a Q1) (Q1 b Q0)))
(check-expect (new-rules (sm-getrules QUIZ1) (unreachable QUIZ1)) '((Q2 a Q2) (ds a ds) (ds b ds) (Q2 b ds)))
(check-expect (new-rules (sm-getrules QUIZ2) (unreachable QUIZ2)) '((Q0 a Q1) (Q0 b Q3) (Q1 a Q1) (Q1 b Q2) (Q2 b Q2) (Q2 a Q3) (Q3 a Q3) (Q3 b Q3)))
(check-expect (new-rules (sm-getrules QUIZ3) (unreachable QUIZ3)) '((Q0 a Q1) (Q1 a Q1) (Q1 b Q2) (Q2 b Q2) (ds a ds) (ds b ds) (Q0 b ds) (Q2 a ds)))
(check-expect (new-rules (sm-getrules QUIZ4) (unreachable QUIZ4)) '((Q0 a Q1) (Q0 b Q0) (Q1 a Q0) (Q1 b Q1)))

(sm-testequiv? QUIZ0 QUIZ0_)
(sm-testequiv? QUIZ1 QUIZ1_)
(sm-testequiv? QUIZ2 QUIZ2_)
(sm-testequiv? QUIZ3 QUIZ3_)
(sm-testequiv? QUIZ4 QUIZ4_)

;; PROOF:

;; Let A and B be dfa's
;; Let n be a non-negative integer
;; B = {A, n unreachable states}
;; A has no unreachable states
;; Prove that (reachable B) == A

;; BASE CASE: n = 0

;; B = {A, 0 unreachable states}
;; B has no unreachable states and A has no unreachable states
;; B == A
;; There are no unreachable states to remove
;; Therefore: (reachable B) == A

;; INDUCTION STEP: Assume true for n = k, show true for n = k + 1

;; ASSUME:

;; B = {A, k unreachable states}
;; A has no unreachable states
;; (reachable B) == A

;; SHOW:

;; {B, 1 unreachable state} = {A, k + 1 unreachable states}
;; A has no unreachable states
;; (reachable {B, 1 unreachable state}) ==  A

;; {B, 1 unreachable state} = {A, k + 1 unreachable states}
;; {A, k + 1 unreachable states} = {{A, k unreachable states}, 1 unreachable state}
;; By assumption, {A, k unreachable states} = B
;; {{A, k unreachable states}, 1 unreachable state} = {B, 1 unreachable state}
;; Therefore: (reachable {B, 1 unreachable state}) ==  A

(test)