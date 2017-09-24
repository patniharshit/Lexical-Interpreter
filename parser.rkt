
#lang racket
(provide (all-defined-out))
(require eopl)
(require racket/trace)
(require rackunit)
(require rackunit/text-ui)
(require "ast.rkt" "ops.rkt")

;; parse: list? --> Ast?
;; throws error if input is not in right format
(define parse
  (lambda (ls)
    (cond
      [(null? ls) (error "Invalid input (null)")]
     
      [(list? ls) ;process the list by traversing it
         (let ((head (first ls))(tail (rest ls)))
          (cond
                [(number? head) (num head)]
	    
                [(boolean? head)(bool head)]

                [(op? head)  (letrec ((opInfo (lookup-op head))   ;find the operator metadata from the operator table
                                        (arity (second opInfo)))   ;get the arity
                        
                                  (if (or (equal? arity 'n) (and (number? arity) (equal? arity (length tail)))) 
                                      (primApp head (map parse tail))
                                      (error "Number of arguments do not match with arity of operator")))]

                [(equal? 'assume head) (if (not (equal? 2 (length tail)))
                                           (error "parse :" "Bad syntax for assume")
                                           (letrec ((ast-binds (map 
                                                                 (lambda(u)
                                                                        (mk-bind (first u)
                                                                                 (parse (second u)))) 
                                                                  (first tail)))
                                                   (ast-body  (parse (second tail))))
                                           (assume ast-binds ast-body)))]
                
                [(equal? 'assume& head) (if (not (equal? 2 (length tail)))
                                           (error "parse :" "Bad syntax for assume&")
                                           (letrec ((ast-binds (map 
                                                                 (lambda(u)
                                                                        (mk-bind (first u)
                                                                                 (parse (second u)))) 
                                                                  (first tail)))
                                                   (ast-body  (parse (second tail))))
                                           (assume& ast-binds ast-body)))]

                [(equal? 'ifte head) (if (not (equal? 3 (length tail))) 
                                              (error "parse :" "Bad syntax for ifte") 
                                           (letrec ((test (parse (first tail))) 
                                                    (then (parse (second tail)))
                                                    (else (parse (third tail))))
                                                               (ifte test then else)))]

                [(and (symbol? head) (zero? (length tail)))        (id head)]
                
                [else           (error "parse :" "bad type")]))]
       (else (parse (list ls))) ;single item can be converted into list to enter the main code
      
  )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; primApp and id
(check-equal? (parse '3) (num 3))
(check-equal? (parse 'x) (id 'x))
(check-equal? (parse '(+ 3 x)) (primApp '+ (list (num 3) (id 'x))))
(check-equal? (parse '(- 4 (+ 3 x))) (primApp '- (list (num 4) (primApp '+ (list (num 3) (id 'x))))))
(check-equal? (parse '(* 5 (- 4 (+ 3 x)))) (primApp '* (list (num 5) (primApp '- (list (num 4) (primApp '+ (list (num 3) (id 'x))))))))

;;assume
(check-equal?   (assume (list (list 'x (num 2)) (list 'y (num 3))) (primApp '+ (list (id 'x) (id 'y))))      
                (parse '(assume ((x 2)(y 3)) (+ x y))))

(check-equal? (assume (list (list 'x (num 2)) (list 'y (num 3))) (primApp '+ (list (id 'x) (id 'y))))
              (assume (list (mk-bind 'x (num 2))(mk-bind 'y (num 3))) (primApp '+ (list (id 'x) (id 'y)))))
;;ifte
(check-equal? (parse '(assume ((x 20) (y  (ifte (IsZero? (- a 1)) (+ a 10) a))) (+ x y)))
                    (assume (list (list 'x (num 20)) (list 'y (ifte (primApp 'IsZero? (list (primApp '- (list (id 'a) (num 1))))) (primApp '+ (list (id 'a) (num 10))) (id 'a))))
                    (primApp '+ (list (id 'x) (id 'y))))
              "parse:ifte")
;;errors
(check-exn exn? (lambda()(parse '())) "parse:null input")
(check-exn exn? (lambda()(parse "hello")) "parse:string")
(check-exn exn? (lambda()(parse '(assume ((x 5)) x y))) "parse:mismatch arity - bad syntax")
(check-exn exn? (lambda()(parse '(assume** ((x 5)) x y))) "parse:bad-type")
