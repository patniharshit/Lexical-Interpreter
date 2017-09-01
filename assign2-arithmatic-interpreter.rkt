#lang racket
(require eopl/eopl)
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype Ast Ast?
  [num (n number?)]
  [primApp (op IsOp?) (rands (list-of Ast?))])

(define primApp? (lambda (x)
                   (cond
                     [(not (Ast? x)) #f]
                     [else #t])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; power operator

(define (^ . xs)
  (define power(lambda(ls)
                 (if (null? ls) 1 (expt (car ls) (apply * (rest ls))))))   
   (power xs))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operator table keeps all supported operations with metadata
;; table has name, procedure,and arity for each supported operator
;; table can be enhanced by adding signature of arguments
(define opTable (list (list '+ + 'n 'plus)
                      (list '- - 'n 'sub)
                      (list '* * 'n 'mul)
                      (list '/ / 'n 'div)
                      (list '^ ^ 'n 'pow)))

;; lookupOp: symbol? -> operator | null
;; looking for match for operator symbol in a table for supported operations
;; if match is found, metadata (p:procedure ar:arity) is returned
(define lookupOp
   (lambda (sym)
     (let ((vals (filter (lambda(u)(equal? (car (cdddr u)) sym)) opTable)))
             (if (not (null? vals))
                 (rest (car vals))
                 null))))


;; IsOp?: symbol? -> boolean
;; looks up in the table for match, returns boolean
(define IsOp?
   (lambda (sym)
     (not (null? (lookupOp sym)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse: list? --> Ast?
;; throws error if input is not in right format
(define dict(lambda(sym)
              (cond
                [(eq? '+ sym) 'plus]
                [(eq? '- sym) 'sub]
                [(eq? '* sym) 'mul]
                [(eq? '/ sym) 'div]
                [(eq? '^ sym) 'pow]
                (else 'error)
                )))

(define check-sanity (lambda(ls)
                       (cond
                         [(null? ls) #f]
                         [(or (number? (car ls)) (list? (car ls))) (check-sanity (rest ls))]
                         [else #t])))

(define parse
  (lambda (ls)
    (cond
      [(null? ls) (error "Input in bad format:(null)")] 
      [(list? ls) ;process the list by traversing it
         (let ((head (first ls))(tail (rest ls)))
          (cond
                [(number? head) (num head)]
                [(IsOp? (dict head)) (if (check-sanity tail) (error "parse: unrecognized type") (primApp (dict head) (map parse tail)))]
                [else (error "Bad operator type")]))]
       (else (parse (list ls))) ;single item can be converted into list to enter the main code          
  )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ans? number?)
;;eval : Ast? --> ans?
;; throws error if arity is mismatched.
(define eval
  (lambda (ast)
    (cases Ast ast 
      [num (n)  n]
      [primApp (op rands) (letrec ((opInfo (lookupOp op))     ;find the operator metadata from the operator table
                                   (proc (first opInfo))      ;get the operator procedure
                                   (arity (second opInfo))    ;get the arity
                                   (args (map eval rands)))   ;evaluate operands to get actual arguments

                            ;; proceed only if either arity is defined as unlimited ('n) or
                            ;; # of actual args match arity

                              (if (or (equal? arity 'n)
                                      (and (number? arity)
                                           (equal? arity (length args)))) 
                                  (apply proc args)
                                  (error "eval: primApp: " "wrong number of arguments")))] 
      )))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require rackunit)
(check-equal? 15 (eval (parse '(+ (* 3 4) (/ 9  (+ 2 1))))) "arith:test")
(check-equal? 5 (eval (parse '(+ 2 (/ 9 (* 3 (- 4 3)))))))
(check-equal? 11 (eval (parse '(+ 3 5 (/ 9 3)))))
(check-equal?  7 (eval (primApp  'plus (list (num 5)  (primApp 'div (list (num 20) (num 2) (num 5) ))))))
(check-equal? 8 (eval (parse '(+ 5 (/ 9 3)))))
(check-equal? 11 (eval (parse '(+ 3 5 (/ 9 3)))))
(check-equal? 10 (eval (parse '(+ 7 (/ 9 (* 3 (- 4 3)))))))
(check-equal? 15 (eval (parse '(+ (* (* 3 2) (- (* 5 3) (+ 9 4))) (/ (* 3 3)  (+ (- 19 17) 1))))))
;(eval (parse '(+ 2 (/ 9 (*3 (- 4 3)))))) ;bad type for operator
;(eval (parse '()))
;(eval (parse '(+ #f 3)))

(check-equal? 5 (eval (parse '(+ 2 (/ 9 (* 3 (- 4 3)))))))
(check-equal? 11 (eval (parse '(+ 3 5 (/ 9 3)))))
(check-equal?  7 (eval (primApp  'plus (list (num 5)  (primApp 'div (list (num 20) (num 2) (num 5) ))))))
(check-equal? 8 (eval (parse '(+ 5 (/ 9 3)))))
(check-equal? 8 (eval (parse '(^ 2 (* 3 (- 4 3))))))
(check-equal? 36 (eval (parse '(^ (- (* 3 3) 3) (* 2 (/ 7 7) (- 10 9)))))) 
(check-error (eval (parse '(+ 2 (/ 9 (*3 (- 4 3)))))) "Bad operator type")
(check-error (eval (parse '())) "Input in bad format:(null)")
(check-error (eval (parse '(+ #f 3))) "parse: unrecognized type")
(check-equal? 16 (eval (parse '(^ 2 (^ 2 2)))))
