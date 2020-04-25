#lang racket

(require fsm)
(require test-engine/racket-tests)

(define FOUR-SEVENTEEN
    (make-tm
   '(Q0 Q1 Q2 Q3)
   '(a b)
   `(((Q0 a) (Q1 ,RIGHT))
     ((Q0 b) (Q0 ,RIGHT))
     ((Q0 ,BLANK) (Q3 ,BLANK))
     ((Q1 a) (Q2 a))
     ((Q1 b) (Q0 ,RIGHT))
     ((Q1 ,BLANK) (Q3 ,BLANK)))
   'Q0
   '(Q2 Q3)
   'Q2))

(define (Q1-INV t i) (eq? (list-ref t (- i 1)) 'a))

(define (Q2-INV t i)
  (and (eq? (list-ref t i) 'a)
       (eq? (list-ref t (- i 1)) 'a)))

(define (Q3-INV t i) (not (FOUR-SEVENTEEN-AUX t)))

;; Purpose: checks if it contains two consecutive a's
(define (FOUR-SEVENTEEN-AUX t)
  (if (or (empty? t) (empty? (cdr t))) #f
      (if (and (eq? (car t) 'a) (eq? (cadr t) 'a)) #t
          (FOUR-SEVENTEEN-AUX (cdr t)))))

(check-expect (sm-apply FOUR-SEVENTEEN `(,LM a a)) 'accept)
(check-expect (sm-apply FOUR-SEVENTEEN `(,LM b b)) 'reject)
(check-expect (sm-apply FOUR-SEVENTEEN `(,LM a a b b)) 'accept)
(check-expect (sm-apply FOUR-SEVENTEEN `(,LM a)) 'reject)
(check-expect (sm-apply FOUR-SEVENTEEN `(,LM b)) 'reject)
(check-expect (sm-apply FOUR-SEVENTEEN `(,LM b a a)) 'accept)
(check-expect (sm-apply FOUR-SEVENTEEN `(,LM a b b a)) 'reject)
(check-expect (sm-apply FOUR-SEVENTEEN `(,LM b b a a)) 'accept)

(check-expect (FOUR-SEVENTEEN-AUX '(a b b a)) #f)
(check-expect (FOUR-SEVENTEEN-AUX '(b a a)) #t)
(check-expect (FOUR-SEVENTEEN-AUX '(a b)) #f)
(check-expect (FOUR-SEVENTEEN-AUX '(a a)) #t)
(check-expect (FOUR-SEVENTEEN-AUX '(a)) #f)
(check-expect (FOUR-SEVENTEEN-AUX '(b)) #f)
(check-expect (FOUR-SEVENTEEN-AUX '()) #f)

(test)