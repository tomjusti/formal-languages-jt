#lang racket
(require fsm)
(require test-engine/racket-tests)

(define QUIZ
  (make-dfa '(Q0 Q1 Q2)
            '(a b)
            'Q0
            '(Q0)
            '((Q2 a Q2)
              (Q0 a Q0)
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

(define member?
  (lambda (x lst)
    (cond
      [(empty? lst) #f]
      [(eqv? x (car lst)) #t]
      [else (member? x (cdr lst))])))

(check-expect (member? 3 '(1 2 3)) #t)
(check-expect (member? 4 '(1 2 3)) #f)
(check-expect (member? 4 '()) #f)

;; reachable: (list of rules), (list of sm-getstart) -> (list of reachable states)
;; Purpose: to return a list of reachable states
;; Accumulative Invariant: res is the list of reachable states
(define reachable
  (lambda (x res)
    (cond
      [(null? x) res]
      [(and (member? (caar x) res) (not (member? (caddar x) res))) (reachable (cdr x) (cons (caddar x) res))]
      [else (reachable (cdr x) res)])))

(check-expect (reachable (sm-getrules QUIZ) (list (sm-getstart QUIZ))) '(Q1 Q0))
(check-expect (reachable (sm-getrules QUIZ1) (list (sm-getstart QUIZ1))) '(ds Q2))

;; unreachable: dfa -> (list of unreachable states)
;; Purpose: to return a list of unreachable states
(define unreachable
  (lambda (dfa)
    (remove* (reachable (sm-getrules dfa) (list (sm-getstart dfa))) (map car (sm-getrules dfa)))))

(check-expect (unreachable QUIZ) '(Q2 ds ds Q2))
(check-expect (unreachable QUIZ1) '(Q0 Q0 Q1 Q1))

(test)