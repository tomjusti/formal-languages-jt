#lang racket
(require fsm)

(define QUIZ
  (make-dfa '(q0 q1)
            '(a b)
            'q0
            '(q0)
            '((q0 a q0)
              (q0 b q1)
              (q1 a q1)
              (q1 b q0))))