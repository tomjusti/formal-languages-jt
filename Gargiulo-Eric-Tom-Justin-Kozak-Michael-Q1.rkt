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

(check-expect (reachable (sm-getrules QUIZ) (list (sm-getstart QUIZ))) '(Q1 Q0))
(check-expect (reachable (sm-getrules QUIZ1) (list (sm-getstart QUIZ1))) '(ds Q2))
(check-expect (reachable (sm-getrules QUIZ2) (list (sm-getstart QUIZ2))) '(Q2 Q3 Q1 Q0))
(check-expect (reachable (sm-getrules QUIZ3) (list (sm-getstart QUIZ2))) '(ds Q2 Q1 Q0))

;; unreachable: dfa -> (list of unreachable states)
;; Purpose: to return a list of unreachable states
(define unreachable
  (lambda (dfa)
    (remove-duplicates (remove* (reachable (sm-getrules dfa) (list (sm-getstart dfa))) (map car (sm-getrules dfa))))))

(check-expect (unreachable QUIZ) '(Q2 ds))
(check-expect (unreachable QUIZ1) '(Q0 Q1))
(check-expect (unreachable QUIZ2) '(ds))
(check-expect (unreachable QUIZ3) '(Q3))

;; new-rules: (list of rules) (list of unreachable states) -> (list of rules)
;; Purpose: to return a list of new rules that do not involve the useless unreachable states
(define new-rules
  (lambda (lor lou)
    (if (null? lor) lor
        (if (or (member? (caar lor) lou) (member? (caddar lor) lou)) (new-rules (cdr lor) lou)
            (cons (car lor) (new-rules (cdr lor) lou))))))

(check-expect (new-rules (sm-getrules QUIZ) (unreachable QUIZ)) '((Q0 a Q0) (Q0 b Q1) (Q1 a Q1) (Q1 b Q0)))
(check-expect (new-rules (sm-getrules QUIZ1) (unreachable QUIZ1)) '((Q2 a Q2) (ds a ds) (ds b ds) (Q2 b ds)))
(check-expect (new-rules (sm-getrules QUIZ2) (unreachable QUIZ2)) '((Q0 a Q1) (Q0 b Q3) (Q1 a Q1) (Q1 b Q2) (Q2 b Q2) (Q2 a Q3) (Q3 a Q3) (Q3 b Q3)))
(check-expect (new-rules (sm-getrules QUIZ3) (unreachable QUIZ3))  '((Q0 a Q1) (Q1 a Q1) (Q1 b Q2) (Q2 b Q2) (ds a ds) (ds b ds) (Q0 b ds) (Q2 a ds)))

(test)