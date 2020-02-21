(define (reachable4 ndfa)
  (let*
      ((rules (sm-getrules ndfa)) ; list of rules of the ndfa
       (states (sm-getstates ndfa)) ; all states of ndfa
       (S (sm-getstart ndfa)) ; starting state
       (res (list S))
       (i (length states)))
    (local ((define (reach-aux r visited)
             (cond
               [(zero? i) visited]
               [(empty? r) (begin
                             (set! i (- i 1))
                             (reach-aux rules visited))] ; list of rules has been scanned, reset rules for another pass
               [(and (member? (caar r) visited) (not (member? (caddar r) visited))) (reach-aux (cdr r) (cons (caddar r) visited))] ; found a member of visited pointing to a state not already visited
               [else ; did not find member of visited
                (reach-aux (cdr r) visited)])))
      (reach-aux rules (list S)))))

  
(define A-STAR
  (make-ndfa
   '(Q0 Q1 Q2)
   '(a b)
   'Q0
   '(Q1)
   '((Q2 a Q2)
     (Q2 b Q2)
     (Q1 a Q1)
     (Q1 b Q2)
     (Q0 a Q1)
     (Q0 b Q0))))


(reachable4 A-STAR)
