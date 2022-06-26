#lang racket/gui

;;;-------------
;;;---Imports---
;;;-------------
(require math/array)
(require racket/gui/base)


;;;--------------------------
;;;---Current Sudoku Board---
;;;--------------------------

(require "./sudokuBoards.rkt")
(define chosenBoard (list-ref sudokus 0))


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


;;;----------------------
;;;---Main Solve Logic---
;;;----------------------

;Solves one square using nonderterminism
;pretty much all other code is gui and aplication of this function
(define solveSquare
  (lambda (board y x)
    ;can't mutate input array, bad news
    (let [(board (mutable-array-copy board))]

      ;square can be any of these values
      (array-set! board (vector y x) (amb 1 2 3 4 5 6 7 8 9))

      ;the square's row must be unique
      (assert (equal? #f (check-duplicates (array->list
                                            (array-slice-ref board (list `(,y) (::)) )))))

      ;the square's column must be unique
      (assert (equal? #f (check-duplicates (array->list
                                            (array-slice-ref board (list (::) `(,x)) )))))
      ;the square's 3x3 square must be unique
      (assert (equal? #f (check-duplicates
                          (array->list
                           (array-slice-ref board (list (:: (* 3 (quotient y 3))
                                                            (* 3 (+ 1 (quotient y 3))))
                                                        (:: (* 3 (quotient x 3))
                                                            (* 3 (+ 1 (quotient x 3))))))))))
      ;return the board with the correct square
      board)))

;loop through the board and solve all squares (that need solving)
(define solveBoard
  (lambda (originalBoard slow)
    (let [(board (mutable-array-copy originalBoard))]
      (map-board board
                 (lambda (board y x)
                   (when slow (begin (print-board board originalBoard dc-global h)))
                   (when (>= (array-ref chosenBoard (vector y x)) 10) (set! board (solveSquare board y x)))
                   board)))))


;;;-----------------------
;;;---Utility Functions---
;;;-----------------------

;defines a way to traverse the 9x9 sudoku board
;acts like a map function
(define map-board
  (lambda (board mapping-func)
    (let [(board (mutable-array-copy board))]
      (letrec [(helper
                (lambda (y x)
                  ;apply callback to each square
                  ;callback returns a modified board
                  (set! board (mapping-func board y x))
                  (cond
                    ((and (= y 8) (= x 8)) board)
                    ((= x 8) (helper (+ 1 y) 0))
                    (#t (helper y (+ 1 x))))))]
                    ;((= y 8) (helper 0 (+ 1 x)))
                    ;(#t (helper (+ 1 y) x)))))]
        (helper 0 0)))))


;;;---------
;;;---GUI---
;;;---------

(define h 600)
(define w h)

(define canvas-global '())
(define dc-global '())

(define frame (new frame% [label "Nondeterministic Sudoku Solver"] [width w] [height (+ 100 h)]))

(define panel1 (new horizontal-panel% [parent frame]
                    [alignment '(center center)]
                    [min-height h]
                    [min-width w]
                    [stretchable-width false]	 
                    [stretchable-height false]))

(new canvas% [parent panel1]
     [min-height h]
     [min-width w]
     [paint-callback
      (lambda (canvas dc)
        (set! canvas-global canvas)
        (set! dc-global dc)
        (draw-grid canvas dc h)
        (draw-sub-grid canvas dc h)
        (print-board chosenBoard  chosenBoard  dc-global h)
        )])

;;; Draw the main-grid structure on canvas.   
(define (draw-grid canvas dc s)
  (send dc set-pen "black" 4 'solid)
  (send dc draw-line (* 1/3 s) 0 (* 1/3 s) s)
  (send dc draw-line (* 2/3 s) 0 (* 2/3 s) s)
  (send dc draw-line 0 (* 1/3 s) s (* 1/3 s))
  (send dc draw-line 0 (* 2/3 s) s (* 2/3 s)))

;;; Draw the sub-grid structure on canvas.
(define (draw-sub-grid canvas dc s)
  (send dc set-pen "black" 1 'solid)
  (for ((i 9))
    (send dc draw-line (* (/ i 9) s) 0 (* (/ i 9) s) s)
    (send dc draw-line 0 (* (/ i 9) s) s (* (/ i 9) s))))

(define (print-square val original x y dc s)
  (send dc-global set-font (make-font #:size 25 #:family 'roman #:weight 'bold))
  (let [(helper
         (lambda (col)

           (send dc set-text-foreground col)
           (send dc set-brush (new brush% [color "white"]))
           
           (let ((x (* (/ x 9) s)) (y (* (/ y 9) s)))
             (send dc set-pen "white" 1 'transparent)
             (send dc draw-rectangle (+ x 5) (+ y 5) (/ w 11) (/ h 11)))
            
           (when (< val 10)
             (send dc draw-text (if (equal? val " ") val (number->string val))
                   (+ (* (/ x 9) s) 20) (+ (* (/ y 9) s) 15)))))]
          
    (if (equal? val 0) (set! val " ") #f)
    (if (not (>= (array-ref original (vector y x)) 10))
        (helper "black")   
        (helper "green"))))

(define print-board
  (lambda (chosenBoard original dc size)
    (map-board chosenBoard (lambda (board y x)
                     (let [(val  (array-ref chosenBoard (vector y x)))]
                       (print-square val original x y dc size))
                     board))))

(define panel2 (new horizontal-panel% [parent frame]
                    [alignment '(center bottom)]
                    [min-height 50]
                    [min-width 600]))

(define solve-button (new button% [parent panel2] 
                          [label "Solve"]
                          [min-width 190]
                          [min-height 50]
                          [callback  (lambda (button event)
                                       (begin
                                         (print-board chosenBoard chosenBoard dc-global h)
                                         (print-board (solveBoard chosenBoard #f) chosenBoard dc-global h)
                                         ))]))

(define step-button (new button% [parent panel2] 
                         [label "Slow Solve"]
                         [min-width 190]
                         [min-height 50]
                         [callback  (lambda (button event)
                                      (begin
                                        (print-board chosenBoard chosenBoard dc-global h)
                                        (print-board (solveBoard chosenBoard #t) chosenBoard dc-global h)
                                        ))]))

(define clear-button (new button% [parent panel2] 
                         [label "Clear"]
                         [min-width 190]
                         [min-height 50]
                         [callback  (lambda (button event)
                                      (begin
                                        (print-board chosenBoard chosenBoard dc-global h)
                                        ))]))

(define panel3 (new horizontal-panel% [parent frame]
                    [alignment '(center bottom)]
                    [min-height 50]
                    [min-width 600]))

(define sudokuChoice (new choice%
                          [label "Choose Sudoku"]
                          [min-height 50]
                          [parent panel3]
                          [choices (list "Sudoku 1" "Sudoku 2 - Easy" "Sudoku 3 - Hard (Takes a while)")]
                          [callback (lambda (c e)
                                      (set! chosenBoard (list-ref sudokus (send sudokuChoice get-selection)))
                                      (print-board chosenBoard chosenBoard dc-global h))]))


(send frame show #t)