
#lang racket
(provide (all-defined-out))
(require eopl)
(require racket/trace)
(require rackunit)
(require rackunit/text-ui)
(require "ops.rkt")

(define expressible? (or/c number? boolean?)) 
(define ans? expressible?)
(define denotable? expressible?)
(define denotable->expressible (lambda(thing) thing))
(define expressible->denotable (lambda(thing) thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defining environment tuple
(define tuple? (lambda(x)
                    (and (list? x) 
                         (symbol? (first x)) 
                         (denotable? (second x)))))

;;mk-tuple: [symbol? denotable?] => tuple?
(define mk-tuple (lambda (x y) (list x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype Env Env?
        [empty-env]
        [extended-env (tuples (list-of tuple?)) (outer-env Env? )]
)
(define G (empty-env))
(define e1 (extended-env (list (mk-tuple 'x 10) (mk-tuple 'y 20)) G))
(define e2 (extended-env (list (mk-tuple 'a 30) (mk-tuple 'b 40) (mk-tuple 'c 50)) e1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;lookup-env : (symbol? Env?) -> denotable?
;; looks for id represented by var in given environment and returns its denoted value if found
;; if id is not found or env is not good type, throws error

(define lookup-env (lambda (var env)
                          (cases Env env
                           [empty-env () (error "lookup-env:empty-env" "unbound identifier")]
                           [extended-env (tuples outer-env)
                                            (let ((tpl (findf (lambda(u)(equal? var (first u))) tuples)))
                                                 (if (not tpl) 
                                                     (lookup-env var outer-env)
                                                     (second tpl)))])))
                            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define a1 (extended-env (list (mk-tuple 'x 2)(mk-tuple 'y 3)(mk-tuple 'z 4) (mk-tuple 'b 0)) (empty-env)))
(define a2 (extended-env (list (mk-tuple 'a 20)(mk-tuple 'b 30)(mk-tuple 'c 40)) a1))
(define a3 (extended-env (list (mk-tuple 's 200)(mk-tuple 'u 300)(mk-tuple 'v 400)) a2))

(check-equal? 200 (lookup-env 's a3))
(check-equal? 30 (lookup-env 'b a3))
(check-equal? 4 (lookup-env 'z a3))
