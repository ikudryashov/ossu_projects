;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

; (a) Write a Racket function racketlist->mupllist that takes a Racket list
; (presumably of mupl values but that will not affect your solution) and
; produces an analogous mupl list with the same elements in the same order.

(define (racketlist->mupllist xs)
  (cond [(null? xs) (aunit)]
        [(apair (car xs) (racketlist->mupllist (cdr xs)))]))

; (b) Write a Racket function mupllist->racketlist that takes a mupl list
; (presumably of mupl values but that will not affect your solution) and
; produces an analogous Racket list (of mupl values) with the same elements
; in the same order.

(define (mupllist->racketlist xs)
  (cond [(aunit? xs) null]
        [(cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))]))

;; Problem 2
; Implementing the mupl Language: Write a mupl interpreter, i.e.,
; a Racket function eval-exp that takes a mupl expression e and either
; returns the mupl value that e evaluates to under the empty environment
; or calls Racket’s error if evaluation encounters a run-time mupl type
; error or unbound mupl variable. A mupl expression is evaluated under
; an environment (for evaluating variables, as usual). In your interpreter,
; use a Racket list of Racket pairs to represent this environment
; (which is initially empty) so that you can use without modification the
; provided envlookup function.

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(aunit? e) e]
        [(closure? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL comparison applied to non-number")))]
        [(fun? e) (closure env e)]
        [(call? e)
         (let ([e1 (eval-under-env (call-funexp e) env)]
               [e2 (eval-under-env (call-actual e) env)])
           (if (closure? e1)
               (eval-under-env (fun-body (closure-fun e1))
                               (if (fun-nameopt (closure-fun e1))
                                   (append (list (cons (fun-nameopt (closure-fun e1)) e1)
                                                 (cons (fun-formal (closure-fun e1)) e2))
                                           (closure-env e1))
                                   (append (list (cons (fun-formal (closure-fun e1)) e2))
                                           (closure-env e1))))
               (error "Must be a closure")))]
        [(mlet? e)
         (let* ([val (eval-under-env (mlet-e e) env)]
                [new-env (append (list (cons (mlet-var e) val)) env)])
           (eval-under-env (mlet-body e) new-env))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([val (eval-under-env (fst-e e) env)])
           (if (apair? val)
               (apair-e1 val)
               (error "Given expression must evaluate to a pair")))]
        [(snd? e)
         (let ([val (eval-under-env (snd-e e) env)])
           (if (apair? val)
               (apair-e2 val)
               (error "Given expression must evaluate to a pair")))]
        [(isaunit? e)
         (let ([val (eval-under-env (isaunit-e e) env)])
           (if (aunit? val)
               (int 1)
               (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

; (a) Write a Racket function ifaunit that takes three mupl expressions
; e1, e2, and e3. It returns a mupl expression that when run evaluates
; e1 and if the result is mupl’s aunit then it evaluates e2 and that is
; the overall result, else it evaluates e3 and that is the overall result.
; Sample solution: 1 line.

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

; (b) Write a Racket function mlet* that takes a Racket list of Racket
; pairs ’((s1 . e1) ...(si . ei) ...(sn . en)) and a final mupl expression
; en+1. In each pair, assume si is a Racket string and ei is a mupl expression.
; mlet* returns a mupl expression whose value is en+1 evaluated in an environment
; where each si is a variable bound to the result of evaluating the corresponding
; ei for 1 ≤ i ≤ n. The bindings are done sequentially, so that each ei is evaluated
; in an environment where s1 through si-1 have been previously bound to the values
; e1 through ei-1.

(define (mlet* lstlst e2)
  (cond [(null? lstlst) e2]
        [(mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))]))

; (c) Write a Racket function ifeq that takes four mupl expressions e1, e2, e3,
; and e4 and returns a mupl expression that acts like ifgreater except e3 is
; evaluated if and only if e1 and e2 are equal integers. Assume none of the
; arguments to ifeq use the mupl variables _x or _y. Use this assumption so that
; when an expression returned from ifeq is evaluated, e1 and e2 are evaluated
; exactly once each.

(define (ifeq e1 e2 e3 e4) 
  (mlet "_x" e1
        (mlet "_y" e2
              (ifgreater (var "_x") (var "_y")
                         e4
                         (ifgreater (var "_y") (var "_x")
                                    e4
                                    e3)))))
  

;; Problem 4
; (a) Bind to the Racket variable mupl-map a mupl function that acts like
; map (as we used extensively in ML). Your function should be curried: it
; should take a mupl function and return a mupl function that takes a mupl
; list and applies the function to every element of the list returning a
; new mupl list. Recall a mupl list is aunit or a pair where the second
; component is a mupl list.

(define mupl-map
  (fun #f "f"
       (fun "g" "xs"
            (ifaunit (var "xs")
                     (aunit)
                     (apair
                      (call (var "f") (fst (var "xs")))
                      (call (var "g") (snd (var "xs"))))))))

; (b) Bind to the Racket variable mupl-mapAddN a mupl function that takes
; an mupl integer i and returns a mupl function that takes a mupl list of
; mupl integers and returns a new mupl list of mupl integers that adds i
; to every element of the list. Use mupl-map (a use of mlet is given to you
; to make this easier).

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "x"
             (call (var "map")
                   (fun #f "q" (add (var "x") (var "q")))))))