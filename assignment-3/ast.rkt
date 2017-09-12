
#lang racket
(provide (all-defined-out))
(require eopl)
(require racket/trace)
(require rackunit)
(require rackunit/text-ui)
(require "run.rkt")

;;Ast : num | primApp | id | b | ifte | assume 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype Ast Ast?
  [num (n number?)]
  [id (s symbol?)]
  [primApp (op op?) (rands (list-of Ast?))]
  [bool (b boolean?)]
  [ifte (test Ast?) (then Ast?) (else Ast?)]
  [assume (binds (list-of bind?)) (exp Ast?)]
  [assume& (binds (list-of bind?)) (exp Ast?)])

;;mk-bind? [symbol? Ast?] => bind?
(define mk-bind (lambda(s a)(list s a)))

(define bind? (lambda(b)(and (symbol? (first b))(Ast? (second b)))))

;;test cases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-equal? (num 3) (num 3))
(check-equal? (id 'x) (id 'x))
(check-equal? (primApp '+ (list (num 3) (id 'x))) (primApp '+ (list (num 3) (id 'x))))
(check-equal? (primApp '- (list (num 4) (primApp '+ (list (num 3) (id 'x))))) (primApp '- (list (num 4) (primApp '+ (list (num 3) (id 'x))))))
(check-equal? (primApp '* (list (num 5) (primApp '- (list (num 4) (primApp '+ (list (num 3) (id 'x))))))) (primApp '* (list (num 5) (primApp '- (list (num 4) (primApp '+ (list (num 3) (id 'x))))))))
(check-equal? (assume (list (list 'x (num 2)) (list 'y (num 3))) (primApp '+ (list (id 'x) (id 'y))))
              (assume (list (mk-bind 'x (num 2))(mk-bind 'y (num 3))) (primApp '+ (list (id 'x) (id 'y)))))
