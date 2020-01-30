#lang eopl
(require fsm)

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

(sm-getrules QUIZ)


; x is a list of rules
; res is a list of start and final states
(define unreachable
  (lambda (x res)
    (cond
      [(empty? x) empty]
      [(and (member? (car x) res) (not (member? (caddr x) res))) (unreachable (cdr x) (cons (caddr x) res))])
    res))



(unreachable (sm-getrules QUIZ) (sm-getstartstate QUIZ))