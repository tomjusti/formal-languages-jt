#lang racket
(require fsm)



(define member?
  (lambda (element lst)
    (cond
      [(empty? lst) #f]
      [(eqv? element (car lst)) #t]
      [else (member? element (cdr lst))])))

; ndfa -> listof sym
(define (reachable4 ndfa)
  (let*
      ((rules (sm-getrules ndfa)) ; list of rules of the ndfa
       (i (length (sm-getstates ndfa)))) ; used to limit recursion depth
    (local ((define (reach-aux r visited)
             (cond
               [(zero? i) visited]
               [(empty? r) (begin
                             (set! i (- i 1))
                             (reach-aux rules visited))] ; list of rules has been scanned, reset rules for another pass
               [(and (member? (caar r) visited) (not (member? (caddar r) visited))) (reach-aux (cdr r) (cons (caddar r) visited))] ; found a member of visited pointing to a state not already visited
               [else ; did not find member of visited
                (reach-aux (cdr r) visited)])))
      (reach-aux rules (list (sm-getstart ndfa)))))) ; "visited" only contains the start state by default


; sym ndfa -> listof sym
(define (path-to-final ndfa)
  (let*
      ((rules (sm-getrules ndfa)) ; list of rules of the ndfa
       (i (length (sm-getstates ndfa))) ; used to limit recursion depth
       (reachable (reachable4 ndfa)))
    (local ((define (reach-aux r visited)
              (cond
                [(zero? i) visited]
                [(empty? r) (begin
                              (set! i (- i 1))
                              (reach-aux rules visited))] ; list of rules has been scanned, reset rules for another pass
                [(and (member? (caddar r) visited) (not (member? (caar r) visited)) (member? (caar r) reachable)) (reach-aux (cdr r) (cons (caar r) visited))] ; found a member of visited pointing to a state not already visited
                [else ; did not find member of visited
                 (reach-aux (cdr r) visited)])))
      (reach-aux rules (sm-getfinals ndfa)))))


(define C
  (make-ndfa '(Q0 Q1 Q2 Q3 Q4)
             '(a b c)
             'Q0
             '(Q4)
             '((Q0 a Q0)
               (Q0 b Q4)
               (Q0 c Q1)
               (Q0 b Q2)
               (Q2 a Q4)
               (Q3 a Q4))))

(define D
  (make-ndfa '(Q0 Q1 Q2 Q3 Q4)
             '(a b)
             'Q0
             '(Q4)
             '((Q0 a Q1)
               (Q1 b Q2)
               (Q2 b Q3)
               (Q3 a Q4))))