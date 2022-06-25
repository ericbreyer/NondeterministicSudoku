#lang racket
(require math/array)
(provide sudokus)

(define sudokus '())

;Each cell is populated with its x corrdinate plus 10 times its y corrdinate
;Any given number is filled in
;now all empty cells are above 9 and unique, making checking for them and for validity easy

(define make-board
  (lambda (board)
    (let [(populatedBoard (array->mutable-array (build-array #(9 9) (lambda (x) (+ (vector-ref x 0)
                                                                     (* (vector-ref x 1) 10) 10)))))]
      (letrec [(helper
                (lambda (y x)
                  (let [(val (array-ref board (vector y x)))]
                    (when (not (= val 0))
                           (array-set! populatedBoard (vector y x) val)))
                  
                  (cond
                    ((and (= y 8) (= x 8)) populatedBoard)
                    ((= x 8) (helper (+ 1 y) 0))
                    (#t (helper y (+ 1 x))))))]
        (helper 0 0)))))


(set! sudokus (cons (make-board (array #(#(0 0 0 0 0 0 0 0 0)
                                         #(6 1 0 0 0 0 3 8 0)
                                         #(9 0 0 0 0 0 0 0 4)
                                         #(0 0 0 0 2 0 0 0 0)
                                         #(3 6 4 0 9 0 0 0 8)
                                         #(2 0 0 0 0 8 0 5 0)
                                         #(0 0 0 0 0 0 5 0 0)
                                         #(0 2 0 4 0 6 0 1 0)
                                         #(0 3 1 0 0 7 2 0 0)))) sudokus))

(set! sudokus (cons (make-board (array #(#(1 0 9 4 0 7 5 6 8)
                                         #(8 0 6 9 0 5 3 4 2)
                                         #(5 0 2 0 8 3 1 9 7)
                                         #(0 0 0 0 0 0 6 8 5)
                                         #(0 8 0 7 0 4 0 0 9)
                                         #(6 2 1 5 9 8 7 3 0)
                                         #(0 0 3 0 0 1 9 2 0)
                                         #(0 0 0 0 4 0 8 5 0)
                                         #(0 1 0 0 5 6 4 0 0)))) sudokus))

(set! sudokus (cons (make-board (array #(#(0 7 0 0 2 0 0 4 6)
                                         #(0 6 0 0 0 0 8 9 0)
                                         #(2 0 0 8 0 0 7 1 5)
                                         #(0 8 4 0 9 7 0 0 0)
                                         #(7 1 0 0 0 0 0 5 9)
                                         #(0 0 0 1 3 0 4 8 0)
                                         #(6 9 7 0 0 2 0 0 8)
                                         #(0 5 8 0 0 0 0 6 0)
                                         #(4 3 0 0 8 0 0 7 0)))) sudokus))