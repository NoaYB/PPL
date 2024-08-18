#lang racket

(provide (all-defined-out))

(define id (lambda (x) x))
(define cons-lzl cons)
(define empty-lzl? empty?)
(define empty-lzl '())
(define head car)
(define tail
  (lambda (lzl)
    ((cdr lzl))))

;;; Q1.a
; Signature: compose(f g)
; Type: [T1 -> T2] * [T2 -> T3]  -> [T1->T3]
; Purpose: given two unary functions return their composition, in the same order left to right
; test: ((compose - sqrt) 16) ==> -4
;       ((compose not not) true)==> true
(define compose
  (lambda (f g)
    (lambda (x)
       (g (f x)))))



; exapmle: the list fs contains only add1$, square$. so f$ is add1$ and g$ is square$ (and id is pipe_cont)
; so, as same as we saw in page 96 in lecture notes, this id(square(add1(x))) will look like this in CPS:
; (id (lambda (x outer_cont) 
;       (add1$ x (lambda (res) 
;                 ((square$ res outer_cont))))))

(define compose$
  (lambda (f$ g$ pipe_cont)
    (pipe_cont (lambda (x outer_cont)
                        (f$ x (lambda (res) 
                                       (g$ res outer_cont)))))))
       
; Signature: pipe(lst-fun)
; Type: [[T1 -> T2],[T2 -> T3]...[Tn-1 -> Tn]]  -> [T1->Tn]
; Purpose: Returns the composition of a given list of unary functions. For (pipe (list f1 f2 ... fn)), returns the composition fn(....(f1(x)))
; test: ((pipe (list sqrt - - number?)) 16)) ==> true
;       ((pipe (list sqrt - - number? not)) 16) ==> false
;       ((pipe (list sqrt add1 - )) 100) ==> -11
(define pipe
  (lambda (fs)  
    (if (empty? (cdr fs))
        (car fs)
        (compose (car fs) (pipe (cdr fs))))))

; Signature: pipe$(lst-fun,cont)
;         [T1 * [T2->T3] ] -> T3,
;         [T3 * [T4 -> T5] ] -> T5,
;         ...,
;         [T2n-1 * [T2n -> T2n+1]] -> T2n+1
;        ]
;        *
;       [[T1 * [T2n -> T2n+1]] -> T2n+1] -> 
;              [[T1 * [T2n+1 -> T2n+2]] -> T2n+2]
;      -> [T1 * [T2n+1 -> T2n+2]] -> T2n+2
; Purpose: Returns the composition of a given list of unry CPS functions. 

;(define id lambda (x) x)

;example:
;(
;(pipe$ (list square$ add1$ div2$)
;id)
;3 (lambda (x) (* x 10))
;)
;⇒ 50
;pipe_cont = id
;outer_cont = (lambda (x) (* x 10))
;we want that every function will be a lambda of the next function


(define pipe$
  (lambda (fs pipe_cont)  
    (if (empty? (cdr fs))
        (pipe_cont (lambda (x outer_cont) ((car fs) x outer_cont))) 
        (pipe$ (cdr fs) (lambda (res) (compose$ (car fs) res pipe_cont))))))


;;; Q2a
; Signature: reduce1-lzl(reducer, init, lzl) 
; Type: [T2*T1 -> T2] * T2 * LzL<T1> -> T2
; Purpose: Returns the reduced value of the given lazy list

;> (reduce1-lzl + 0
;(cons-lzl 3 (lambda () (cons-lzl 8 (lambda () ‘())))))
;11

; reducer = +
; init = 0
; lzl = (cons-lzl 3 (lambda () (cons-lzl 8 (lambda () ‘()))

(define reduce1-lzl 
  (lambda (reducer init lzl)
    (if (empty-lzl? lzl)
      init
      (reduce1-lzl reducer (reducer init (head lzl)) (tail lzl))
  )
))

;;; Q2b
; Signature: reduce2-lzl(reducer, init, lzl, n) 
; Type: [T2*T1 -> T2] * T2 * LzL<T1> * Number -> T2
; Purpose: Returns the reduced value of the first n items in the given lazy list
(define reduce2-lzl 
  (lambda (reducer init lzl n)
    (if (or (empty-lzl? lzl) (= n 0))
      init
      (reduce2-lzl reducer (reducer init (head lzl)) (tail lzl) (- n 1))
  ))
)  

;;; Q2c
; Signature: reduce3-lzl(reducer, init, lzl) 
; Type: [T2 * T1 -> T2] * T2 * LzL<T1> -> Lzl<T2>
; Purpose: Returns the reduced values of the given lazy list items as a lazy list
(define reduce3-lzl 
  (lambda (reducer init lzl)
    (if (empty-lzl? lzl)
      empty-lzl
      (cons-lzl (reducer init (head lzl)) (lambda () (reduce3-lzl reducer (reducer init (head lzl)) (tail lzl)))))
  )
)  
 
;;; Q2e
; Signature: integers-steps-from(from,step) 
; Type: Number * Number -> Lzl<Number>
; Purpose: Returns a list of integers from 'from' with 'steps' jumps
(define integers-steps-from
  (lambda (from step)
    (cons-lzl from (lambda () (integers-steps-from (+ from step) step))) ;the lambda returns step, so cons-lzl returns (from step)
  )
)

;; from lecture notes
;; Signature: map-lzl(f, lz)
;; Type: [[T1 -> T2] * Lzl(T1) -> Lzl(T2)]
(define map-lzl
  (lambda (f lzl)
    (if (empty-lzl? lzl)
        lzl
        (cons-lzl (f (head lzl))
                       (lambda () (map-lzl f (tail lzl)))))))


;;; Q2f
; Signature: generate-pi-approximations() 
; Type: Empty -> Lzl<Number>
; Purpose: Returns the approximations of pi as a lazy list

; element = 

;(define f lambda (x) (/ 1 (* (+ a x) (+ a (+ x 2)))))

(define generate-pi-approximations
  (lambda ()
    (let ((f (lambda (a) (/ 8 (* a (+ a 2))))) (x-lzl-lst (integers-steps-from 1 4))) ;x-lzl-lst = 1, 5, 9, .. ==> after map ==> 1/(1*3), 1/(5*7), 1/(9*11), ...
        (reduce3-lzl + 0 (map-lzl f x-lzl-lst)))
          ))