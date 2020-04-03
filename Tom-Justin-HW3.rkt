#lang racket

(require fsm)
(require test-engine/racket-tests)

(define TWO-THREE-FOUR-A
  (make-ndfa '(Q0 Q1 Q2 Q3)
            '(a b)
            'Q0
            '(Q2)
            `((Q0 a Q0)
              (Q0 a Q1)
              (Q0 b Q3)
              (Q0 ,EMP Q2)
              (Q1 b Q2)
              (Q3 a Q2)
              (Q2 b Q2))))

(sm-apply TWO-THREE-FOUR-A '(a a b b))
(sm-apply TWO-THREE-FOUR-A '(a b a b))
(sm-apply TWO-THREE-FOUR-A '(a b))
(sm-apply TWO-THREE-FOUR-A '(a a a a a a a b b b b b b b))
(sm-apply TWO-THREE-FOUR-A '(a a a a a b a b b b b b))
(sm-apply TWO-THREE-FOUR-A '(a a a a a b b b b b))
(sm-apply TWO-THREE-FOUR-A '(b a))
(sm-apply TWO-THREE-FOUR-A '())

(define TWO-THREE-FOUR-B
  (make-ndfa '(Q0 Q1)
            '(a b c)
            'Q0
            '(Q1)
            `((Q0 a Q0)
              (Q0 b Q0)
              (Q0 ,EMP Q1)
              (Q1 c Q1)
              (Q1 ,EMP Q0))))

(sm-apply TWO-THREE-FOUR-B '())
(sm-apply TWO-THREE-FOUR-B '(a))
(sm-apply TWO-THREE-FOUR-B '(b))
(sm-apply TWO-THREE-FOUR-B '(a b))
(sm-apply TWO-THREE-FOUR-B '(a a b b))
(sm-apply TWO-THREE-FOUR-B '(a b a b))
(sm-apply TWO-THREE-FOUR-B '(c))
(sm-apply TWO-THREE-FOUR-B '(c c c))
(sm-apply TWO-THREE-FOUR-B '(a b c))
(sm-apply TWO-THREE-FOUR-B '(b a c))
(sm-apply TWO-THREE-FOUR-B '(b a))
(sm-apply TWO-THREE-FOUR-B '(b a a a a a b a a a b a))
(sm-apply TWO-THREE-FOUR-B '(c a a a a a b c c c a a))

(define TWO-THREE-FOUR-C
  (make-ndfa '(Q0 Q1 Q2 Q3 Q4 Q5 Q6 Q7 Q8)
            '(a b c)
            'Q0
            '(Q8)
            `((Q0 ,EMP Q1)
              (Q0 ,EMP Q4)
              (Q0 ,EMP Q6)
              (Q1 a Q2)
              (Q2 b Q3)
              (Q3 ,EMP Q1)
              (Q3 a Q7)
              (Q7 b Q8)
              (Q4 b Q5)
              (Q5 c Q6)
              (Q6 ,EMP Q4)
              (Q6 a Q7))))

(sm-apply TWO-THREE-FOUR-C '(a b))
(sm-apply TWO-THREE-FOUR-C '(a b a b))
(sm-apply TWO-THREE-FOUR-C '(a b a b a b))
(sm-apply TWO-THREE-FOUR-C '(b c a b))
(sm-apply TWO-THREE-FOUR-C '(b c b c a b))

(define A
  (make-ndfa '(Q0 Q1)
             '(a b)
             'Q0
             '(Q1)
             '((Q0 a Q1))))

(define B
  (make-ndfa '(Q0 Q1)
             '(a b)
             'Q0
             '(Q1)
             '((Q0 b Q1))))

(define AB (sm-concat A B))
(define ABA (sm-concat AB A))
(define ABUABA (sm-union AB ABA))
(define ABUABA* (sm-kleenestar ABUABA))
(define ABUABA*A (sm-concat ABUABA* A))
(define ABUABA*A* (sm-kleenestar ABUABA*A))

(define A* (sm-kleenestar A))
(define A*B (sm-concat A* B))
(define BA* (sm-concat B A*))
(define BA*B (sm-concat BA* B))
(define BA*BUA (sm-union BA*B A))
(define BA*BUA* (sm-kleenestar BA*BUA))
(define A*B-BA*BUA* (sm-concat A*B BA*BUA*))

(sm-test A*B-BA*BUA*)

(test)