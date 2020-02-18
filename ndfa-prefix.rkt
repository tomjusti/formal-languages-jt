#lang racket
(require fsm)

; ndfa ndfa --> ndfa
(define (ndfa-union m1 m2)
  (let* ((new-m2 (sm-rename-states (sm-getstates m1) m2)) ; rename states of m2 to not conflict with m1
         (sts (append (sm-getstates m1) (sm-getstates new-m2)))
         (new-start (generate-symbol 'S sts)) ; generate a new name for the start state of the union machine
         (new-states (cons new-start sts)) ; the new states
         (new-rules-from-start (list (list new-start EMP (sm-getstart m1))
                                     (list new-start EMP (sm-getstart new-m2))))
         (new-rules (append new-rules-from-start (sm-getrules m1)(sm-getrules new-m2)))
         (new-sigma (remove-duplicates (append (sm-getalphabet m1)
                                               (sm-getalphabet new-m2))))
         (new-finals (append (sm-getfinals m1) (sm-getfinals new-m2))))
    (make-ndfa new-states new-sigma new-start new-finals new-rules)))

(define A
  (make-ndfa '(q0 q1)
             '(a b)
             'q0
             '(q1)
             '((q0 a q1))))

(define B
  (make-ndfa '(q0 q1)
           '(a b)
           'q0
           '(q1)
           '((q0 a q1))))

; starting state/new final state
; all states that are not the start state
; remove duplicate ds from the states
; new rules to return to the starting state on EMP for prefix
; all the rules for the machine plus the new rules added
; new list of final states