#lang racket

(require fsm)
(require test-engine/racket-tests)

(define A (singleton-regexp "A"))
(define A* (kleenestar-regexp A))
(define B (singleton-regexp "B"))
(define B* (kleenestar-regexp B))
(define AB* (kleenestar-regexp (concat-regexp A B)))
(define BA* (kleenestar-regexp (concat-regexp B A)))

(define FIRST-LANGUAGE
  (union-regexp (concat-regexp AB* BA*) (concat-regexp A A*)))

(define SECOND-LANGUAGE
  (kleenestar-regexp (concat-regexp (kleenestar-regexp (union-regexp (concat-regexp A B) (concat-regexp A (concat-regexp A B)))) A*)))

(define THIRD-LANGUAGE
  (kleenestar-regexp (concat-regexp (kleenestar-regexp (concat-regexp A* (concat-regexp B* A*))) B)))

(define FOURTH-LANGUAGE
  (union-regexp (kleenestar-regexp (union-regexp (concat-regexp B A) B)) (kleenestar-regexp (union-regexp (concat-regexp B B) A))))

(define TWO-TWO-THREE-A
  (make-ndfa '(Q0 Q1 Q2 Q3 Q4)
            '(a b)
            'Q0
            '(Q2 Q3)
            `((Q0 a Q1)
              (Q0 a Q2)
              (Q0 ,EMP Q3)
              (Q1 b Q0)
              (Q2 a Q2)
              (Q3 b Q4)
              (Q4 a Q3))))

;; (sm-apply TWO-TWO-THREE-A '(a a))
;; (sm-test TWO-TWO-THREE-A 10)

(define TWO-TWO-THREE-B
  (make-ndfa '(Q0 Q1 Q2 Q3)
            '(a b)
            'Q0
            '(Q0)
            `((Q0 a Q0)
              (Q0 a Q1)
              (Q1 a Q2)
              (Q1 b Q3)
              (Q2 b Q3)
              (Q3 ,EMP Q0))))

;; (sm-test TWO-TWO-THREE-B)

(define TWO-TWO-THREE-C
  (make-ndfa '(Q0 Q1 Q2 Q3 Q4)
            '(a b)
            'Q0
            '(Q4)
            `((Q0 ,EMP Q1)
              (Q0 b Q4)
              (Q1 a Q1)
              (Q1 ,EMP Q2)
              (Q2 b Q2)
              (Q2 ,EMP Q3)
              (Q3 a Q3)
              (Q3 ,EMP Q0)
              (Q4 ,EMP Q0))))

;; (sm-test TWO-TWO-THREE-C)

(define TWO-TWO-THREE-D
  (make-ndfa '(Q0 Q1)
            '(a b)
            'Q0
            '(Q0)
            `((Q0 a Q0)
              (Q0 b Q0)
              (Q0 b Q1)
              (Q1 a Q0)
              (Q1 b Q0))))

; (sm-test TWO-TWO-THREE-D)

(define TWO-TWO-SIX-A
  (make-ndfa '(Q0 Q1 Q2 Q3)
            '(a b)
            'Q0
            '(Q0)
            `((Q0 a Q1)
              (Q1 a Q2)
              (Q1 b Q0)
              (Q1 b Q3)
              (Q2 b Q0)
              (Q3 a Q0))))

; (sm-test TWO-TWO-SIX-A)

(define TWO-TWO-SIX-B
  (make-ndfa '(Q0 Q1 Q2 Q3)
            '(a b)
            'Q0
            '(Q0)
            `((Q0 a Q1)
              (Q1 b Q2)
              (Q1 a Q3)
              (Q2 a Q0)
              (Q3 b Q0)
              (Q1 b Q0))))

(sm-testequiv? TWO-TWO-SIX-A TWO-TWO-SIX-B)

(define N
  (make-ndfa
   '(Q0 Q1 Q2 Q3 Q4 Q5)
   '(a b)
   'Q0
   '(Q5)
   `((Q0 a Q0)
     (Q0 b Q0)
     (Q0 a Q1)
     (Q1 a Q2)
     (Q2 b Q3)
     (Q3 a Q4)
     (Q4 b Q5))))

(define TWO-TWO-SEVEN-B
  (make-dfa
        '(Q0 Q1 Q2 Q3 Q4 Q5)
        '(a b)
        'Q0
        '(Q5)
        `((Q0 a Q1)
          (Q0 b Q0)
          (Q1 a Q2)
          (Q1 b Q0)
          (Q2 a Q2)
          (Q2 b Q3)
          (Q3 a Q4)
          (Q3 b Q0)
          (Q4 a Q2)
          (Q4 b Q5)
          (Q5 a Q1)
          (Q5 b Q0))))

(sm-testequiv? TWO-TWO-SEVEN-B N)

(define TWO-TWO-EIGHT-A
  (make-ndfa
   '(Q0 Q1 Q2 Q3 Q4 Q5)
   '(a b)
   'Q0
   '(Q5)
   `((Q0 a Q0)
     (Q0 b Q0)
     (Q0 a Q1)
     (Q1 a Q2)
     (Q1 b Q2)
     (Q2 a Q3)
     (Q2 b Q3)
     (Q3 a Q4)
     (Q3 b Q4)
     (Q4 a Q5)
     (Q4 b Q5))))

(define TWO-TWO-NINE-A
  (make-ndfa
   '(Q0 Q1 Q2 Q3 Q4)
   '(a b)
   'Q0
   '(Q3 Q4)
   `((Q0 a Q0)
     (Q0 b Q0)
     (Q0 b Q2)
     (Q0 ,EMP Q1)
     (Q1 b Q4)
     (Q1 b Q2)
     (Q2 a Q3)
     (Q3 ,EMP Q4)
     (Q4 a Q3))))

(define TWO-TWO-NINE-A_
  (make-dfa
   '(Q0 Q1 Q2)
   '(a b)
   'Q0
   '(Q1 Q2)
   '((Q0 a Q0)
     (Q0 b Q1)
     (Q1 a Q2)
     (Q1 b Q1)
     (Q2 a Q2)
     (Q2 b Q1))))

(sm-testequiv? TWO-TWO-NINE-A TWO-TWO-NINE-A_)

(test)