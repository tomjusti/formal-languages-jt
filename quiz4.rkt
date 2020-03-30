#lang racket
(require fsm)
(require test-engine/racket-tests)

; Problem Anal
; Stack is used to contain a's,
; either one or two a's are nondeterministically
; pushed onto the stack. When a b is encountered, the
; top of the stack is popped. yeet?

; Name the PDA
; jeff

; Unit Tests
(check-expect
 (sm-apply pda-m->2m '()) 'accept) ; empty input
(check-expect
 (sm-apply pda-m->2m '(a b)) 'accept) ; m = n
(check-expect
 (sm-apply pda-m->2m '(a a b b)) 'accept) ; m = n

(check-expect
 (sm-apply pda-m->2m '()) 'accept) ; empty input
(check-expect
 (sm-apply pda-m->2m '(a b b)) 'accept) ; m + 1 = n
(check-expect
 (sm-apply pda-m->2m '(a a b b b)) 'accept) ; m + 1 = n

(check-expect
 (sm-apply pda-m->2m '()) 'accept) ; empty input
(check-expect
 (sm-apply pda-m->2m '(a b b b)) 'reject) ; m + 2 = n
(check-expect
 (sm-apply pda-m->2m '(a a b b b b)) 'accept) ; m + 2 = n

; ID Conditions that must be tracked as input is consumed
; and associate a state with each condition

; Q0: no input  consumed, starting state
; Q1: 0 or more a's are pushed onto the stack
; Q2: length of stack + b's in ci always between n and 2n

(define (Q0-INV consumed-input stack)
  (and (empty? consumed-input) (empty? stack)))

(define (Q1-INV consumed-input stack)
  (and (>= (length consumed-input) 0)
       (>= (length stack))))

(define (Q2-INV consumed-input stack)
  (and (>= (+ (length stack) (length (filter (lambda (x) (eq? x 'b)) consumed-input)))
           (length (filter (lambda (x) (eq? x 'a)) consumed-input)))
       (<= (+ (length stack) (length (filter (lambda (x) (eq? x 'b)) consumed-input)))
           (* 2 (length (filter (lambda (x) (eq? x 'a)) consumed-input))))))

(define pda-m->2m
  (make-ndpda
   '(S M F)
   '(a b)
   '(a)
   'S
   '(F)
   `(((S ,EMP ,EMP) (M ,EMP))
     ((M ,EMP ,EMP) (F ,EMP))
     ((M a ,EMP) (M(a a)))
     ((M a ,EMP) (M(a)))
     ((F b (a)) (F ,EMP)))))

; num b - num a in CI always 1 off length stack
; len ci - len s


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

(define (list-head lst n)
  (reverse (list-tail (reverse lst) (- (length lst) n))))

(define (fsarules->pdarules lor)
  (local
    [(define (fsa-aux lor res)
       (cond
         [(empty? lor) res]
         [else
          (append (list-head (caar (sm-getrules  



(define (fsa-ndpda fsa)
  (make-ndpda
   (sm-getrules fsa)
   (sm-getalphabet fsa)
   '()
   (sm-getstart fsa)
   (sm-getfinals fsa)
   (sm-getrules fsa)))









(test)    