#lang eopl

; structural implementation of continuation
#;(define-datatype continuation continuation?
  (end-cont)
  (fact1-cont
   (n integer?)
   (cont continuation?)))

#;(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont () val)
      (fact1-cont (saved-n saved-cont)
                  (apply-cont saved-cont (* saved-n val))))))


; procedural implementation of continuation
;   a continuation is represented by its action under `apply-cont`
(define end-cont
  (lambda ()
    (lambda (val) val)))

(define fact1-cont
  (lambda (n saved-cont)
    (lambda (val)
      (apply-cont saved-cont (* n val)))))

(define apply-cont
  (lambda (cont val)
    (cont val)))

; inlined version of factorial function
(define fact
  (lambda (n)
    (fact/k n (lambda (val) val))))

(define fact/k
  (lambda (n cont)
    (if (zero? n)  ; if n is zero
        (cont 1)   ; send 1 to the continuation
        (fact/k (- n 1) (lambda (val) (cont (* n val))))))) ; otherwise, evaluate fact of n-1 in a continuation that calls the result val,
                                                            ; and then sends to the continuation the value (* n val)

; invariant : (fact/k n g) = (g n!)

(define fib
  (lambda (n)
    (fib/k n (lambda (val) val))))

(define fib/k
  (lambda (n cont)
    (if (< n 2)
        (cont 1)
        (fib/k (- n 1) (lambda (val1)
                         (lambda (val2)
                           (cont (+ val1 val2))))))))
          



