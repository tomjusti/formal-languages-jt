#lang racket
(require fsm)

;; powerSet: (listof X) -> (listof (listof X))
(define (powerSet A)
  (cond [(null? A) (list null)]
        [else
         (let ((rest (powerSet (cdr A))))
           (append
            (map (lambda (x) (cons (car A) x)) rest)
            rest))]))