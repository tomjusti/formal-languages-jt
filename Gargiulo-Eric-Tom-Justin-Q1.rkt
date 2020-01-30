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

(define member?
  (lambda (x lst)
    (cond
      [(empty? lst) #f]
      [(eqv? x (car lst)) #t]
      [else (member? x (cdr lst))])))

(check-expect (member? 3 '(1 2 3)) #t)
(check-expect (member? 4 '(1 2 3)) #f)

; x is a list of rules
; res is a list of start and final states
(define reachable
  (lambda (x res)
    (cond
      [(null? x) '()]
      [(and (member? (caar x) res) (not (member? (caddar x) res))) (reachable (cdr x) (cons (caddar x) res))]
      [else (reachable (cdr x) res)])
    res))



(check-expect (reachable (sm-getrules QUIZ) (list (sm-getstart QUIZ))) '(Q0 Q1))

(test)