#lang racket
(require fsm)
(require test-engine/racket-tests)

; Problem Analysis
; Stack is used to contain a's,
; either one or two a's are nondeterministically
; pushed onto the stack. When a b is encountered, the
; top of the stack is popped.

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
; Q2: length of stack + b's in ci always between n and 2n (where n is number of a's in consumed-input)

; in Q0, consumed-input and stack are both empty
(define (Q0-INV consumed-input stack)
  (and (empty? consumed-input) (empty? stack)))

; in Q1, stack and consumed-input are always length 0 or more
(define (Q1-INV consumed-input stack)
  (and (>= (length consumed-input) 0)
       (>= (length stack) 0)))

; in Q2, length of stack + number of b's in consumed-input is always between n and 2n (where n is number of a's in consumed-input)
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

; convert the given fsa to an ndpda
; fsa -> ndpda
(define (fsa->ndpda fsa)
  (make-ndpda
   (sm-getstates fsa)
   (sm-getalphabet fsa)
   '()
   (sm-getstart fsa)
   (sm-getfinals fsa)
   (converted-rules (sm-getrules fsa))))

; convert a list of fsa rules to pda rules
; list -> list
(define (converted-rules lor)
  (cond
    [(empty? lor) '()]
    [else (cons (converted-rules-aux (car lor)) (converted-rules (cdr lor)))]))

(define (converted-rules-aux rule) (list (list (car rule) (cadr rule) EMP) (list (caddr rule) EMP)))


(define NDFA1
  (make-ndfa '(Q0 Q1)
            '(a b)
            'Q0
            '(Q0)
            `((Q0 a Q0)
              (Q0 b Q0)
              (Q0 b Q1)
              (Q1 a Q0)
              (Q1 b Q0))))


(define NDPDA1
  (make-ndpda '(Q0 Q1)
            '(a b)
            '()
            'Q0
            '(Q0)
            `(((Q0 a ,EMP) (Q0 ,EMP))
              ((Q0 b ,EMP) (Q0 ,EMP))
              ((Q0 b ,EMP) (Q1 ,EMP))
              ((Q1 a ,EMP) (Q0 ,EMP))
              ((Q1 b ,EMP) (Q0 ,EMP)))))


(check-expect (sm-testequiv? NDFA1 (fsa->ndpda NDFA1)) #t)
(check-expect (sm-testequiv? NDPDA1 (fsa->ndpda NDFA1)) #t)

(test)    