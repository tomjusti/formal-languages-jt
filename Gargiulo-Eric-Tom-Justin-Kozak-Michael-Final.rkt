#lang racket

(require fsm)
(require test-engine/racket-tests)

; ;  INSTRUCTIONS
; ; 
; ; During the final exam period, you will be asked to present your
; ; solution to one of the following problems chosen at random. Your
; ; grade will solely depend on the question randomly chosen and your
; ; other two answers will play no role in your final exam. You must,
; ; however, solve all three problems. The correctness of your presented
; ; solution and the thoroughness of your presentation will determine your
; ; grade. Your talk should be 20 to 25 minutes long.
; 


; ; 
; ; Q1
; ; 
; ; Design and implement a TM or ctm that multiplies two unary number. For
; ; example, 4 = iiii, 2 = ii, and iiii * ii = iiiiiiii (which is 8). Design
; ; your machine using the following precondition and postcondition:
; ; 
; ;  Pre: ⌴nxm⌴, where n and m are the unary numbers and the head starts over x
; ; Post: ⌴p⌴, where p = n x m and the head is on the first blank after p
; 


(define R
  (make-tm
   '(S H)
   `(i x ,LM)
   `(((S i) (H ,RIGHT))
     ((S x) (H ,RIGHT))
     ((S ,BLANK) (H ,RIGHT)))
   'S
   '(H)))

;; an 'i, 'x, or blank is read
(define (R-H-INV t i)
  (or (>= i 1)
      (eq? (list-ref t (+ i 1)) 'i)
      (eq? (list-ref t (+ i 1)) 'x)
      (eq? (list-ref t (+ i 1)) BLANK)))

; H-INV:
; 
; ((S i) (H ,RIGHT))
; An i is read on S
; Goes to H
; INV holds because either an i, x or blank is read
; 
; ((S x) (H ,RIGHT))
; An x is read on S
; Goes to H
; INV holds because either an i, x or blank is read
; 
; ((S ,BLANK) (H ,RIGHT))
; A blank is read on S
; Goes to H
; INV holds because either an i, x or blank is read


(define L
  (make-tm
   '(S H)
   `(i x ,LM)
   `(((S i) (H ,LEFT))
     ((S x) (H ,LEFT))
     ((S ,BLANK) (H ,LEFT)))
   'S
   '(H)))

(define (L-H-INV t i)
  (or (>= i 1)
      (eq? (list-ref t (+ i 1)) 'i)
      (eq? (list-ref t (+ i 1)) 'x)
      (eq? (list-ref t (+ i 1)) BLANK)))

; H-INV:
; 
; ((S i) (H ,LEFT))
; An i is read on S
; Goes to H
; INV holds because either an i, x or blank is read
; 
; ((S x) (H ,LEFT))
; An x is read on S
; Goes to H
; INV holds because either an i, x or blank is read
; 
; ((S ,BLANK) (H ,LEFT))
; A blank is read on S
; Goes to H
; INV holds because either an i, x or blank is read


(define HALT (make-tm '(S)
                      `(i x ,LM)
                      `()
                      'S
                      `(S)))

(define (FB R-or-L) (combine-tms
                    (list 0 R-or-L (cons BRANCH
                                    (list (list 'i (list GOTO 0))
                                          (list 'x (list GOTO 0))
                                          (list LM (list GOTO 0))
                                          (list BLANK HALT))))
                    (list 'i 'x LM)))

;; writes a blank to the tape
(define Mblank (make-tm '(S H)
                        `(i x ,LM)
                        `(((S i) (H ,BLANK))
                          ((S x) (H ,BLANK))
                          ((S ,BLANK) (H ,BLANK)))
                        'S
                        '(H)))

;; FBL, FBR, R
;; just apply the same machine twice
(define (^2 machine) (combine-tms (list machine machine) `(i x ,LM)))

(define SHIFTL
  (combine-tms
   ;; move to the right
   (list R
         (cons BRANCH (list  (list BLANK (list GOTO 10))
                             (list 'i (list GOTO 20))
                             (list 'x (list GOTO 20))
                             (list LM R (list GOTO 20)))) ;; move to the right then go to 20
         10 L
         (list GOTO 40)
         20 (list (list VAR 'y))
         30
         Mblank
         L
         'y
         (^2 R)
         ;; variable abstraction
         (list (list VAR 'x)
               (cons BRANCH (list (list BLANK (list GOTO 10))
                                  (list 'i (list GOTO 30))
                                  (list 'x (list GOTO 30))
                                  (list LM R (list GOTO 30)))))
         40) ;; empty, just halts
   `(i x ,LM)))

(define tm-multiply
  (combine-tms
   (list
    0
    (FB L)
    R
    (cons BRANCH (list (list 'x (list GOTO 10))
                       (list 'i (list GOTO 20))))
    10
    Mblank
    R
    (cons BRANCH (list (list 'i (list GOTO 10))
                       (list BLANK (list GOTO 30))))
    20
    Mblank
    (list GOTO 40)
    
    30
    (FB R)
    (FB L)
    L
    (cons BRANCH (list (list BLANK (list GOTO 50))
                       (list LM R L)))
    
    40
    R
    (cons BRANCH (list (list 'i (list GOTO 40))
                       (list 'x (list GOTO 60))))
    
    50
    R
    SHIFTL
    L
    (list GOTO 30)
    
    60
    R
    (cons BRANCH (list (list 'i (list GOTO 70))
                       (list BLANK (list GOTO 0))))
    
    70
    (list (list VAR 'a)
          Mblank
          (^2 (FB R))
          'a
          (^2 (FB L))
          'a
          (list GOTO 60)))
   (list 'i 'x LM)))

(check-expect (ctm-run tm-multiply `(,LM ,BLANK i i x i i ,BLANK) 2) '(H 1 (@ _ i i i i _ _ _ _ _ _ _)))
(check-expect (ctm-run tm-multiply `(,LM ,BLANK i i x i i i ,BLANK) 2) '(H 1 (@ _ i i i i i i _ _ _ _ _ _ _ _)))

; ; 
; ; Q2
; ; 
; ; Show that recursive languages are closed under union.
; ; Implement your construction algorithm in FSM. 
; 


; (define (ndfa-union m1 m2)
;   (let* ((new-m2 (sm-rename-states (sm-getstates m1) m2)) ; rename states of m2 to not conflict with m1
;          (sts (append (sm-getstates m1) (sm-getstates new-m2)))
;          (new-start (generate-symbol 'S sts)) ; generate a new name for the start state of the union machine
;          (new-states (cons new-start sts)) ; the new states
;          (new-rules-from-start (list (list new-start EMP (sm-getstart m1))
;                                      (list new-start EMP (sm-getstart new-m2))))
;          (new-rules (append new-rules-from-start (sm-getrules m1) (sm-getrules new-m2)))
;          (new-sigma (remove-duplicates (append (sm-getalphabet m1)
;                                                (sm-getalphabet new-m2))))
;          (new-finals (append (sm-getfinals m1) (sm-getfinals new-m2))))
;     (make-ndfa new-states new-sigma new-start new-finals new-rules)))


(define (tm-union m1 m2)
  (let* ((new-m2 (sm-rename-states (sm-getstates m1) m2)) ; rename states of m2 to not conflict with m1

         (sts (append (sm-getstates m1) (sm-getstates new-m2)))

         (new-start (generate-symbol 'S sts)) ; generate a new name for the start state of the union machine

         (new-accept (generate-symbol 'Y sts))

         (new-states (cons new-start (cons new-accept sts))) ; the new states

         (new-rules-from-start (list (list (list new-start LM) (list (sm-getstart m1) RIGHT))
                                     (list (list new-start LM) (list (sm-getstart new-m2) RIGHT))))
         
         (new-rules (list (list (list (sm-getaccept m1) BLANK) (list new-accept BLANK))
                                    (list (list (sm-getaccept new-m2) BLANK) (list new-accept BLANK))))
         
         (new-rules2 (append new-rules-from-start (sm-getrules m1) (sm-getrules new-m2) new-rules))
         
         (new-sigma (remove-duplicates (append (sm-getalphabet m1) (sm-getalphabet new-m2))))

         (new-finals (cons new-accept (append
                                       (filter (lambda (x) (not (eq? (sm-getaccept m1) x)))(sm-getfinals m1))
                                       (filter (lambda (x) (not (eq? (sm-getaccept new-m2) x)))(sm-getfinals new-m2))))))
    
    (make-tm new-states new-sigma new-rules2 new-start new-finals new-accept)))

(define A* (make-tm
              '(S Y N)
              `(a b)
              `(((S a) (S ,RIGHT))
                ((S b) (N b))
                ((S ,BLANK) (Y ,BLANK)))
              'S
              '(Y N)
              'Y))

(define B* (make-tm
              '(S2 Y N)
              `(a b)
              `(((S2 a) (N b))
                ((S2 b) (S2 ,RIGHT))
                ((S2 ,BLANK) (Y ,BLANK)))
              'S2
              '(Y N)
              'Y))

(define A*UB* (tm-union A* B*))

(check-expect (sm-apply A* `(,LM a a a)) 'accept)
(check-expect (sm-apply B* `(,LM b b b)) 'accept)
(check-expect (sm-apply A* `(,LM a)) 'accept)
(check-expect (sm-apply B* `(,LM b)) 'accept)
(check-expect (sm-apply A* `(,LM)) 'accept)
(check-expect (sm-apply B* `(,LM)) 'accept)
(check-expect (sm-apply A* `(,LM a b)) 'reject)
(check-expect (sm-apply B* `(,LM a b)) 'reject)

(check-expect (sm-apply A*UB* `(,LM a a a)) 'accept)
(check-expect (sm-apply A*UB* `(,LM b b b)) 'accept)
(check-expect (sm-apply A*UB* `(,LM a)) 'accept)
(check-expect (sm-apply A*UB* `(,LM b)) 'accept)
(check-expect (sm-apply A*UB* `(,LM)) 'accept)
(check-expect (sm-apply A*UB* `(,LM a b)) 'reject)

; Construct a turing machine M
; Give it an input w
; If the input belongs to the language L, the machine accepts
; If the input doesn’t belong to the language L, the machine rejects
; It is a recursive language as it may halt and accept or halt and reject
; 
; L1 is recursive and L2 is recursive
; The union of the recursive languages is also recursive
; Let L1 and L2 be languages that accept different things
; Whenever the input is accepted by either L1 or L2, the input should be accepted by L1UL2
; 
; Prove L1UL2 is also recursive
; 
; Construct a turing machine M1 for L1 and give an input w
; If w belongs to the language, input will be accepted
; If w doesn’t belong to the language, input will be rejected
; 
; If the input is rejected by the machine M1 for L1, check if it accepted by the machine M2 for L2
; 
; Construct a turing machine M2 for L2 and give an input w
; If w belongs to the language, input will be accepted
; If w doesn’t belong to the language, input will be rejected
; 
; Construct a machine M for L1UL2 that takes the data contained in M1 and M2
; If the input is accepted by either L1 or L2, the input will be accepted by the machine M for L1UL2
; If the input is rejected by both L1 and L2, the input will be rejected by the machine M  for L1UL2
; 
; -----
; 
; Recursive languages are closed under union
; BC: Given an input of length 0
; 
; A* is a language that accepts 0 or more a’s
; B* is a language that accepts 0 or more b’s
; AUB is the union of A* and B*
; 
; An input of length 0 is given
; Length 0 means empty
; A* and B* accept 0 or more
; AUB also accepts
; 
; Given an input of length n + 1
; 
; Union machine non-deterministically decides which machine to check first
; If A* or B* accepts, AUB accepts 
; 
; If one machine rejects, check the second machine
; If the second machine rejects, A*UB* rejects


; ; 
; ; Q3
; ; 
; ; Prove that the following problem is unsolvable:
; ; 
; ; Given a TM, M, an input word, w, and two of its
; ; states, P and Q, determine if M reaches both
; ; P and Q when given w as input.
; 


; t(x) takes in the machine
; 
; On w, go to P
; Try to get to Q
; If it gets to Q, it halts
; If it doesn't get to Q, it doesn't halt
; 
; The machine accepts if the machine halts
; The machine rejects if the machine does not halt
; 
; This makes the machine "decidable"
; which supposedly makes the TM for the halting problem decidable
; 
; This is a contradiction since the TM for the halting problem is undecidable
; Problems for which no algorithm exist are undecidable
; Therefore, P to Q is also undecidable

(test)
