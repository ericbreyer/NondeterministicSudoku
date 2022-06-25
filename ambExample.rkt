#lang racket
;;;---------------
;;;---Amb Logic---
;;;---------------

(define amb-fail (lambda ()
            (display "amb tree exhausted")))

(define-syntax amb
  (syntax-rules ()
    ((amb) (amb-fail))
    ((amb alt ...)
     (let ((prev-amb-fail amb-fail))
       (call/cc
        (lambda (sk)
          (call/cc
           (lambda (fk)
             (set! amb-fail
                   (lambda ()
                     (set! amb-fail prev-amb-fail)
                     (fk 'fail)))
             (sk alt))) ...

          (prev-amb-fail)))))))

(define assert
  (lambda (pred)
    (unless pred (amb))))

(define-syntax amb-list
  (syntax-rules ()
    ((amb-list e)
     (let ((prev-amb-fail amb-fail)
           (results '()))
       (when (call/cc
              (lambda (k)                                                
                (set! amb-fail (lambda () (k #f)))
                (let ((v e))  
                  (set! results (cons v results))                       
                  (k #t))))                                             
         (amb-fail))                 
       (set! amb-fail prev-amb-fail)
       (reverse results)))))

;;;---------------
;;;---Amb Usage---
;;;---------------

(define getPythagTriple 
  (lambda ()
     ;;;---Step 1: Declare Possiblities---
    (let [(x (amb 1 2 3 4 5 6 7 8 9 10 11 12 13))
          (y (amb 1 2 3 4 5 6 7 8 9 10 11 12 13))
          (z (amb 1 2 3 4 5 6 7 8 9 10 11 12 13))]

     ;;;---Step 2: Declare Constraints---
      (assert (= (+ (* x x) (* y y)) (* z z)))

     ;;;---Step 3: Get results---
      (list x y z))))

;Get one possibility
(getPythagTriple)

;Get all possibilities (if there are multiple)
(amb-list (getPythagTriple))