#lang racket

(require fsm)
(require test-engine/racket-tests)

(define FOUR-ONE-SEVEN
  (make-tm
   '(Q0 Q1 Q2)
   `(a b ,LM)
   `(((Q0 ,BLANK) (Q0 ,RIGHT))
     ((Q0 b) (Q0 ,RIGHT))
     ((Q1 ,BLANK) (Q0 ,RIGHT))
     ((Q1 b) (Q0 ,RIGHT))
     ((Q0 a) (Q1 ,RIGHT))
     ((Q1 a) (Q2 a)))
   'Q0
   '(Q2)
   'Q2))

;; Q0-INV: a b or blank has been read
(define (Q0-INV t i)
  (or (eq? (list-ref t i) 'b) (eq? (list-ref t i) BLANK)))

;; Q1-INV: an a has been read after a b or blank was read
(define (Q1-INV t i) (eq? (list-ref t (- i 1)) 'a))

;; Q2-INV: an a has been read after another a was read
(define (Q2-INV t i)
  (and (eq? (list-ref t i) 'a)
       (eq? (list-ref t (- i 1)) 'a)))

; Q0-INV:
; 
; ((Q0 ,BLANK) (Q0 ,RIGHT))
; A blank is read on Q0
; It remains in Q0
; Therefore, invariant holds because a blank is not an a
; An a was not read
; 
; ((Q0 b) (Q0 ,RIGHT))
; A b is read on Q0
; Therefore, invariant holds because a was not what was read
; 
; ((Q1 b) (Q0 ,RIGHT))
; A b is read after an a is read
; Therefore, invariant holds because a was not what was read
; 
; ((Q1 ,BLANK) (Q0 ,BLANK))
; A blank is read on Q1
; Therefore, invariant holds because a blank is not an a
; An a was not read
; 
; Q1-INV:
; 
; ((Q0 a) (Q1 ,RIGHT))
; An a is read in Q0
; Now in Q1
; Therefore, invariant holds because to reach Q0, an A needs to be read
; 
; Q2-INV:
; 
; ((Q1 a) (Q2 a))
; An a is read in Q1
; Now in Q2
; To get to Q1 in the first place, an A needs to be read in Q0
; Therefore, invariant holds because there are two consecutive a’s


(check-expect (last (sm-showtransitions FOUR-ONE-SEVEN `(,LM a b b a a))) '(Q2 5 (@ a b b a a)))
(check-expect (last (sm-showtransitions FOUR-ONE-SEVEN `(,LM a a))) '(Q2 2 (@ a a)))
(check-expect (last (sm-showtransitions FOUR-ONE-SEVEN `(,LM b a a))) '(Q2 3 (@ b a a)))
(check-expect (last (sm-showtransitions FOUR-ONE-SEVEN `(,LM b a b a a))) '(Q2 5 (@ b a b a a)))

;; 4.1.8c

; (define R
;   (make-tm 
;    '(S H)
;    `(a b ,LM)
;    `(((S ,LM) (S ,RIGHT))
;      ((S a) (H ,RIGHT))
;      ((S b) (H ,RIGHT))
;      ((S ,BLANK) (H ,RIGHT)))
;    'S
;    '(H)))


(define R (make-tm
           '(S H)
           `(a b)
           `(((S a) (H ,RIGHT))
             ((S b) (H ,RIGHT))
             ((S ,BLANK) (H ,RIGHT)))
           'S
           '(H)))

(define (S-INV t i) #t)

;; Position of the tape has to be >= 1
;; Whatever is being referenced on the tape has to be either be an a, b, or BLANK
;; Cannot be left hand marker because position of head is >= 1
;; Moves one position to the left when a, b, or blank is read
(define (H-INV t i)
  (or (>= i 1)
      (eq? (list-ref t (sub1 i)) 'a)
      (eq? (list-ref t (sub1 i)) 'b)
      (eq? (list-ref t (sub1 i)) BLANK)))

; S-INV:
; INV holds because it's always true
; 
; H-INV:
; 
; ((S a) (H ,RIGHT))
; An a is referenced on the tape
; INV holds
; 
; ((S b) (H ,RIGHT))
; A b is referenced on the tape
; INV holds
; 
; ((S ,LM) (H ,RIGHT))
; A left marker is referenced on the tape
; INV holds
; 
; ((S ,BLANK) (H ,RIGHT))
; A blank is referenced on the tape
; INV holds


; (define L
;   (make-tm
;    '(S H)
;    `(a b ,LM)
;    `(((S ,LM) (S ,RIGHT))
;      ((S a) (H ,LEFT))
;      ((S b) (H ,LEFT))
;      ((S ,BLANK) (H ,LEFT)))
;    'S
;    '(H)))


(define L (make-tm
           '(S H)
           `(a b)
           `(((S a) (H ,LEFT))
             ((S b) (H ,LEFT))
             ((S ,BLANK) (H ,LEFT)))
           'S
           '(H)))

(define (S-INV-L t i) #t)

;; Position of the tape has to be >= 1
;; Whatever is being referenced on the tape has to be either be an a, b, or BLANK
;; Cannot be left hand marker because position of head is >= 1
;; Moves one position to the left when a, b, or blank is read
(define (H-INV-L t i)
  (or (>= i 1)
      (eq? (list-ref t (add1 i)) 'a)
      (eq? (list-ref t (add1 i)) 'b)
      (eq? (list-ref t (add1 i)) BLANK)))

; S-INV:
; INV holds because it's always true
; 
; H-INV:
; 
; ((S a) (H , LEFT))
; An a is referenced on the tape
; INV holds
; 
; ((S b) (H , LEFT))
; A b is referenced on the tape
; INV holds
; 
; ((S ,BLANK) (H ,LEFT))
; A blank is referenced on the tape
; INV holds


(define HALT (make-tm '(S)
                      '(a b)
                      '()
                      'S
                      '(S)))

; (define FBR (combine-tms
;              (list 0 R (cons BRANCH
;                              (list (list 'a (list GOTO 0))
;                                    (list 'b (list GOTO 0))
;                                    (list LM (list GOTO 0))
;                                    (list BLANK HALT))))
;              (list 'a 'b LM)))


(define FOUR-ONE-EIGHT-C (combine-tms
             (list L (cons BRANCH
                           (list (list 'a HALT)
                                 (list 'b HALT)
                                 (list LM HALT)
                                 (list BLANK R))))
             (list 'a 'b LM)))

(check-expect (ctm-run FOUR-ONE-EIGHT-C `(,LM a ,BLANK b) 1) '(S 0 (@ a _ b)))
(check-expect (ctm-run FOUR-ONE-EIGHT-C `(,LM a ,BLANK b) 2) '(S 1 (@ a _ b)))
(check-expect (ctm-run FOUR-ONE-EIGHT-C `(,LM a ,BLANK b) 3) '(H 3 (@ a _ b)))
(check-expect (ctm-run FOUR-ONE-EIGHT-C `(,LM a b b a) 1) '(S 0 (@ a b b a)))
(check-expect (ctm-run FOUR-ONE-EIGHT-C `(,LM a b b a) 2) '(S 1 (@ a b b a)))
(check-expect (ctm-run FOUR-ONE-EIGHT-C `(,LM a b b a) 3) '(S 2 (@ a b b a)))

(test)
