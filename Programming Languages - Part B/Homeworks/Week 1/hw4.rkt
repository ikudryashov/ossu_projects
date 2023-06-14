
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below


; 1. Write a function sequence that takes 3 arguments low, high, and stride,
; all assumed to be numbers. Further assume stride is positive. sequence produces
; a list of numbers from low to high (including low and possibly high) separated
; by stride and in sorted order.

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

; 2. Write a function string-append-map that takes a list of strings xs and
; a string suffix and returns list of strings. Each element of the output
; should be the corresponding element of the input appended with suffix
; (with no extra space between the element and suffix). You must use
; Racket-library functions map and string-append.

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

; 3. Write a function list-nth-mod that takes a list xs and a number n.
; If the number is negative, terminate the computation with
; (error "list-nth-mod: negative number"). Else if the list is empty,
; terminate the computation with (error "list-nth-mod: empty list"). Else
; return the ith element of the list where we count from zero and i is the
; remainder produced when dividing n by the list’s length. Library functions
;length, remainder, car, and list-tail are all useful – see the Racket documentation.

(define (list-nth-mod xs n)
  (letrec ([l (length xs)]
           [r (remainder n l)]
           [helper (lambda (xs n acc)
                     (if (= acc r)
                         (car xs)
                         (helper (cdr xs) n (+ acc 1))))])
    (cond [(< n 0) (error "list-nth-mod: negative number")]
          [(null? xs) (error "list-nth-mod: empty list")]
          [#t (helper xs n 0)])))

; 4. Write a function stream-for-n-steps that takes a stream s and a number n.
; It returns a list holding the first n values produced by s in order. Assume
; n is non-negative. Sample solution: 5 lines. Note: You can test your streams
; with this function instead of the graphics code.

(define (stream-for-n-steps s n)
  (let ([ans (s)])
    (if (= n 0)
        null
        (cons (car ans) (stream-for-n-steps (cdr ans) (- n 1))))))

; 5. Write a stream funny-number-stream that is like the stream of natural numbers
; (i.e., 1, 2, 3, ...) except numbers divisble by 5 are negated
; (i.e., 1, 2, 3, 4, -5, 6, 7, 8, 9, -10, 11, ...). Remember a stream is a thunk that
; when called produces a pair. Here the car of the pair will be a number and the cdr
; will be another stream.

(define funny-number-stream
  (letrec ([f (lambda (x) (if (= (remainder x 5) 0)
                              (cons (- x) (lambda () (f (+ x 1))))
                              (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

; 6. Write a stream dan-then-dog,where the elements of the stream alternate between the strings "dan.jpg"
; and "dog.jpg" (starting with "dan.jpg"). More specifically, dan-then-dog should be a thunk
; that when called produces a pair of "dan.jpg" and a thunk that when called produces a pair
; of "dog.jpg" and a thunk that when called... etc.

(define dan-then-dog
  (letrec ([f (lambda (x) (if (string=? x "dan.jpg")
                              (cons "dan.jpg" (lambda () (f "dog.jpg")))
                              (cons "dog.jpg" (lambda () (f "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

; 7. Write a function stream-add-zero that takes a stream s and returns another stream. If s
; would produce v for its ith element, then (stream-add-zero s) would produce the pair (0 . v)
; for its ith element. Sample solution: 4 lines. Hint: Use a thunk that when called uses s and recursion.
; Note: One of the provided tests in the file using graphics uses (stream-add-zero dan-then-dog)
; with place-repeatedly.

(define (stream-add-zero s)
  (letrec ([ans (s)]
           [f (lambda (x) (cons (cons 0 (car ans)) (lambda () (f (cdr ans)))))])
    (lambda () (f ans))))

; 8. Write a function cycle-lists that takes two lists xs and ys and returns a stream. The lists may
; or may not be the same length, but assume they are both non-empty. The elements produced by the stream
; are pairs where the first part is from xs and the second part is from ys. The stream cycles forever through
; the lists. For example, if xs is ’(1 2 3) and ys is ’("a" "b"), then the stream would produce, (1 . "a"),
; (2 . "b"), (3 . "a"), (1 . "b"), (2 . "a"), (3 . "b"), (1 . "a"), (2 . "b"), etc.

(define (cycle-lists xs ys)
  (letrec ([helper (lambda (n)
                     (lambda () (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (helper (+ n 1)))))])
    (helper 0)))



; 9. Write a function vector-assoc that takes a value v and a vector vec. It should behave like Racket’s assoc
; library function except (1) it processes a vector (Racket’s name for an array) instead of a list, (2) it
; allows vector elements not to be pairs in which case it skips them, and (3) it always takes exactly two arguments.
; Process the vector elements in order starting from 0. You must use library functions vector-length, vector-ref,
; and equal?. Return #f if no vector element is a pair with a car field equal to v, else return the first pair
; with an equal car field. Sample solution is 9 lines, using one local recursive helper function.

(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [helper (lambda (acc)
                     (cond [(= acc len) #f]
                           [(not (pair? (vector-ref vec acc))) (helper (+ acc 1))]
                           [(equal? (car (vector-ref vec acc)) v) (vector-ref vec acc)]
                           [#t (helper (+ acc 1))]))])
    (helper 0)))

; 10. Write a function cached-assoc that takes a list xs and a number n and returns a function that takes
; one argument v and returns the same thing that (assoc v xs) would return. However, you should use an n-element
; cache of recent results to possibly make this function faster than just calling assoc (if xs is long and a
; few elements are returned often). The cache must be a Racket vector of length n that is created by the call to
; cached-assoc (use Racket library function vector or make-vector) and used-and-possibly-mutated each time the
; function returned by cached-assoc is called. Assume n is positive.
; The cache starts empty (all elements #f). When the function returned by cached-assoc is called, it first checks
; the cache for the answer. If it is not there, it uses assoc and xs to get the answer and if the result is
; not #f (i.e., xs has a pair that matches), it adds the pair to the cache before returning (using vector-set!).
; The cache slots are used in a round-robin fashion: the first time a pair is added to the cache it is put
; in position 0, the next pair is put in position 1, etc. up to position n - 1 and then back to
; position 0 (replacing the pair already there), then position 1, etc.

(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [slot-to-replace 0]
           [helper (lambda (v)
                     (let ([ans (vector-assoc v memo)])
                       (if ans
                           ans
                           (let ([new-ans (assoc v xs)])
                             (if new-ans
                                 (begin
                                   (vector-set! memo slot-to-replace new-ans)
                                   (if (= slot-to-replace (- n 1))
                                       (set! slot-to-replace 0)
                                       (set! slot-to-replace (+ slot-to-replace 1)))
                                   new-ans)
                                 new-ans)))))])
    (lambda (v) (helper v))))



