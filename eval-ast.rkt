
#lang racket
(provide (all-defined-out))
(require eopl)
(require racket/trace)
(require rackunit)
(require rackunit/text-ui)
(require "ast.rkt" "ops.rkt" "env.rkt" "parser.rkt")

(define rev-reduce (lambda (f x ls)
    (if (null? ls) x
    (rev-reduce f (f x (car ls)) (cdr ls)))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;eval-ast : [Ast? env?]--> ans? or error
;; throws error for arity mismatch and unbound identifier
(define eval-ast
  (lambda (ast env)
    (cases Ast ast 

      [num (n)  n]
 
      [bool(b) b]

      [id (s)  (denotable->expressible(lookup-env s env))]
 
      [ifte (test then else) (let ((b (eval-ast test env)))
                                  (cond
                                  [(boolean? b) (if b (eval-ast then env)
                                                   (eval-ast else env))]
                                  (else (error 'eval-ast "ifte:test condition must be boolean instead of ~a" b))))]

      [assume (binds body)   (let ((tpls (map  (lambda(u) 
                                                   (mk-tuple  (first u)  
                                                              (expressible->denotable 
                                                                  (eval-ast (second u) env)))) 
                                           binds))) ;eval-astuate asts to get values to be bound to identifiers

                                  (eval-ast body (extended-env tpls env)))] ;eval-astuate the body in the extended envirnment
      
      [assume& (binds body)   (let ((tpls (rev-reduce  (lambda(x u) 
                                                   (extended-env (list (mk-tuple  (first u)  
                                                              (expressible->denotable 
                                                                  (eval-ast (second u) x)))) x)) 
                                          env binds))) ;eval-astuate asts to get values to be bound to identifiers

                                  (eval-ast body tpls))] ;eval-astuate the body in the extended envirnment
     
      [primApp (s rands) (letrec ((proc   (op s))     ;get the operator procedure
                                   (args   (map (lambda(u)(eval-ast u env)) rands)))   ;eval-astuate operands to get actual arguments
                           (apply proc args))]
      )))

(define env1 (extended-env (list (mk-tuple 'x 2)(mk-tuple 'y 3)(mk-tuple 'z 4) (mk-tuple 'b 0)) (empty-env)))
(define env2 (extended-env (list (mk-tuple 'a 20)(mk-tuple 'b 30)(mk-tuple 'c 40)) env1))
(define env3 (extended-env (list (mk-tuple 's 200)(mk-tuple 'u 300)(mk-tuple 'v 400)) env2))

;;arithmetic
(check-equal? -45 (eval-ast (parse '(* 5 (- 4 (+ 3 x)))) e1))

;;algebraic
(check-equal? 2 (eval-ast (id 'x) env1))
(check-equal? 5 (eval-ast (parse '(+ x y)) env1))
(check-equal? 4 (eval-ast (parse '(+ x 2)) env1))
(check-equal? (/ 5 2) (eval-ast (parse '(+ 2 (/ x (* z (- 4 y))))) env1))
(check-equal? 23 (eval-ast (parse '(+ a (/ 9 (* y (- 4 3))))) env2))
(check-equal? 602 (eval-ast (parse '(+ (* 3 s) (/ 64  (+ 2 b)))) env3))

;;algebraic error
(check-exn exn? (lambda () 
                (eval-ast (parse '(+ (* 3 alpha) (/ 64  (+ 2 beta)))) env3)) "algebraic: unbound identifier")

;;lexical
(check-equal? 1 (eval-ast (parse '(assume ((x -1)(y -2)) ( + x y z))) env3))
(check-equal? -4 (eval-ast (parse '(assume ((x 2)(y 3)(z 4))
                                      (assume ((x 4)(z (* x 2)))
                                          (assume ((z (+ x z)) (y (- y x)))
                                              (+ x (* z y))))))  
                             (empty-env)))

;;ifte
(check-equal? 31 (eval-ast (parse '(assume ((x 20)
                                            (y  (ifte (IsZero? (- a 1)) (+ a 10) a)))
                                           (+ x y)))
                           (extended-env (list (mk-tuple 'a 1)) (empty-env))) "eval-ast: assume-ifte-true")

(check-equal? 30 (eval-ast (parse '(assume ((x 20)
                                            (y (ifte (IsZero? (- a 1)) (+ a 10) a)))
                                            (+ x y)))
                           (extended-env (list (mk-tuple 'a 10)) (empty-env))) "eval-ast: assume-ifte-false")
;;ifte:error:test_not_boolean
(check-exn exn? (lambda () (eval-ast (parse '(assume ((x 20)
                                            (y (ifte  (- a 1) (+ a 10) a)))
                                           (+ x y)))
                          (extended-env (list (mk-tuple 'a 10)) (empty-env)))) "eval-ast: assume-ifte-test-not-boolean")
;;unbound identifier 
(check-exn exn? (lambda()(eval-ast (parse 'foo) (empty-env))) "eval-ast:unbound identifier")
