# Nondeterministic Sudoku Solver
Using the `amb` form in scheme (racket) to solve sudoku.

## Inspiration
I came across this [video](https://www.youtube.com/watch?v=iOJD7nd7cyY) in which I was introduced to the Amb operator. It immediately looked like black magic so I did some more research to understand how it works and its implementations.

## Amb
`amb` is the ambiguous choice (nondeterminism) operator. It takes a list of possibilities and evaluates to an option that allows the program to succeed. In practice, `amb` usually tries its options left to right but theoretically, the evaluation order doesn't matter - hence "nondeterminism".

`assert`, also sometimes `require`, defines a predicate the program has to satisfy. If the predicate fails, the current branch of the program is abandoned and new possibilities of `amb` forms are tried until the assertion holds.

```racket
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
```
This is the implementation of `amb` I have chosen to use.
It defines 2 continuations, a success (sk) and a fail (fk). The success continuation is for passing the next possibility of the `amb` to. The fail continuation is created to capture the state of the program before a path is tried. If the program fails under this possibility, it abandons everything and restores the program to this point in time, ready to move on to the next possibility to try (note the `...`, a `call/cc`/`fk` "block" is created for each possibility to allow `amb` to try the next possibility after it returns from a failure.

Because of this nature of trying all possibilities of an `amb` before calling fail for a previous `amb`, the actual underlying logic to find a valid set of options for the `amb` form is a depth-first search. This is useful to understand when determining the efficiency of this strategy for solving problems (though an iterative or traditionally recursive depth-first search will probably be more efficient than all the `call/cc` overhead `amb` requires).

Further readings and implementations I consulted:
- http://community.schemewiki.org/?amb
- http://www.sfu.ca/~tjd/383summer2019/scheme-amb.html
- http://wiki.c2.com/?AmbSpecialForm
- https://gist.github.com/Liudx1985/11308921

## Logical Programming

Logical programming is a style of programming in which the programmer:
1. States facts about the problem domain (like the possible values of sudoku squares)
2. Makes assertions about the facts of the domain (that the numbers in the squares must satisfy sudoku constraints)
3. Queries, or asks questions, about the facts of the domain (give a valid sudoku)

This is a declarative rather than imperative programming paradigm: telling the computer **what** to do instead of **how** to do it.

`amb` lends itself to this style.
- `amb` statements are the facts about the domain
- `assert` statements are assertions about the domain
- asking for the value of a variable is a query

## Solving Sudoku

This style should make solving a sudoku simple. It is literally called a **logic** puzzle. Theoretically, we just need to populate all empty squares with `(amb 1 2 3 4 5 6 7 8 9)`, `assert` that rows should have unique numbers, etc., and then ask for the board. 
This will work, but it will take forever. Declaring all the `amb`s first and then asserting will search a lot of dead ends that don't need to be searched (and wouldn't be in a proper DFS algorithm). A better way is to go square by square.
```racket
;Solves one square using nondeterminism
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
```
Checking and asserting square by square cuts off a lot of searching through paths that could never work, as if the square doesn't work, it will never search further. Declaring all the `amb`s at once always starts the search at the very bottom of the tree of possibilities and turns this into more of a brute force algorithm.

Then we just need to run this function on every unsolved square. The logical programming is still here:
1. First declare the fact that the square has to be an integer 1-9
2. Then assert that it must satisfy the sudoku rules
3. We query the final board after solving every square

This is the whole algorithm needed to solve a sudoku. The rest of the code is there to build a sudoku board in the correct format, apply `solveSquare`, and create a GUI.

I find `amb` fascinating and it still almost feels like magic. You just have to tell the program what you want, and it will create and destroy universes and rewind time until it magically produces a valid result.
