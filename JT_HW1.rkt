#lang eopl
(require fsm)
(require test-engine/racket-tests)

;; 1.8.3

(define A (singleton-regexp "A"))
(define B* (kleenestar-regexp (singleton-regexp "B")))

(define NO-MORE-THAN-3 (union-regexp
                        (concat-regexp B* (concat-regexp A (concat-regexp B* (concat-regexp A  (concat-regexp B* (concat-regexp A B*))))))
                        (union-regexp
                         (concat-regexp B* (concat-regexp A (concat-regexp B* (concat-regexp A B*))))
                         (union-regexp
                          (concat-regexp B* (concat-regexp A B*)) B*))))

(define MULTIPLE-OF-3 (union-regexp
                       (kleenestar-regexp
                        (concat-regexp B* (concat-regexp A (concat-regexp B* (concat-regexp A (concat-regexp B* (concat-regexp A B*))))))) B*))

(test)