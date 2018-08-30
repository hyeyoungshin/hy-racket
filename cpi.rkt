#lang racket

(struct const-exp (num))
(struct bool-exp (bool))
(struct var-exp (id))
(struct diff-exp (e1 e2))
(struct if-exp (test true-branch false-branch))
(struct let-exp (id exp body))
(struct proc-exp (var body))
(struct app-exp (rator rand))

(struct num-val (num))
(struct bool-val (bool))
(struct proc-val (fun))

(struct environment-representation (new extend lookup))
(struct closure-representation (make apply))
(struct closure (parameter body env))

(define my-env
  (environment-representation
    (lambda () (lambda (x) (error 'lookup "not found: ~e" x)))
    (lambda (e x a) (lambda (y) (if (eq? x y) a (e y))))           
    (lambda (e x) (e x))))


#;(define my-closure
  (closure-representation 
   closure                      ; <-- make
   (lambda (a-closure argument) ; <-- apply
     (match-define (closure parameter body env) a-closure)
     (interpreter body (my-env-extend env parameter argument)))))

;; (ER INT -> CR)
(define (make-closures-as-structs an-environment-representation interpreter) 
  (match-define (environment-representation new extend lookup)               
    an-environment-representation)
  
  (struct closure (parameter body env))
  ;; type Closure = (closure Symbol AST an-environment-representation.Environment 

  (closure-representation
   closure                      ; argument for the `make` parameter
   (lambda (a-closure argument) ; argument for the `apply` parameter
     (match-define (closure parameter body env) a-closure)
     (interpreter body (extend env parameter argument)))))

(define (value-of-program prg);; Program -> FinalAnswer
  (value-of prg (my-env-new)))

(define (value-of exp env) ;; Expression Environment -> ExpVal
  (match exp
    [(const-exp num) (num-val num)]
    [(bool-exp bool) (bool-val bool)]
    [(var-exp var) (lookup env var)]
    ))