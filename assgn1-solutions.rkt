#lang racket
(require eopl)
(provide (all-defined-out))

(define-datatype tree tree?
  [null]		;;; Null
  [node (val number?)	;;; Value of the node
        (left tree?)	;;; Left subtree
        (right tree?)])	;;; Right subtree

;; Que 1
;; Wrapping extra parantheses
(define wrap (lambda(ls)
               (cond
                 [(null? ls) '()]
                 (else (cons (cons (car ls) `()) (wrap (cdr ls)))))))

;; Que 2
;; Counting occurrences
(define count-occurrences (lambda(x ls)
                           (cond
                             [(null? ls) 0]
                             [(pair? (car ls)) (+ (count-occurrences x (car ls)) (count-occurrences x (cdr ls)))]
                             [(equal? x (car ls)) (+ 1 (count-occurrences x (cdr ls)))]
                             (else (count-occurrences x (cdr ls))))))

;; Que 3
;; Merge sorted lists
(define merge (lambda(ls1 ls2)
                (cond
                  [(null? ls1) ls2]
                  [(null? ls2) ls1]
                  [(< (car ls1) (car ls2)) (cons (car ls1) (merge (cdr ls1) ls2))]
                  (else (cons (car ls2) (merge ls1 (cdr ls2)))))))

;; Que 4
;; Cartesian product of lists
(define product
  (lambda (ls1 ls2)
    (if (or (null? ls1) (null? ls2)) `()
          (append (map (lambda (y) (list (car ls1) y)) ls2) (product (cdr ls1) ls2)))))

;; Que 5
;; Inorder traversal of binary tree
(define traverse   
  (lambda (root)                
    (cond                     
      ((null? root) '())
      (else (append
              (traverse (first (rest root)))
              (list (first root))
              (traverse (first (rest (rest root)))))))))

;; Que 6
;; Finding path in bst
(define findpath (lambda(val root)
                   (cases tree root
                     (null () `())
                     (node (value left right) (cond
                                                  [(equal? val value) `()]
                                                  [(> val value) (cons 'right (findpath val right))]
                                                  [(< val value) (cons 'left (findpath val left))])))))

;; Que 7
;; Currying functions
(define func (lambda (f args) (apply f args)))

(define curry (lambda (f n)
  (let curry-n ((n1 n) (args `()))
  (if (> n1 0) (lambda (x) (curry-n (- n1 1) (cons x args))) (func f args)))))

;; Que 8
;; Checking subsequence
(define is-subseq (lambda(ls1 ls2)
                    (cond
                      [(null? ls1) #true]
                      [(null? ls2) #false]
                      [(equal? (car ls1) (car ls2)) (is-subseq (cdr ls1) (cdr ls2))]
                      (else (is-subseq ls1 (cdr ls2))))))

;; Que 9
;; Reduce on binary tree
(define tree-reduce (lambda (x f t)
                      (cases tree t
                        (null () x)
                        (node (value left right) (f value (tree-reduce x f left) (tree-reduce x f right))))))



(wrap '())
(wrap '(1 2 3 4))
(wrap '(1 (2 (3)) 4))

(count-occurrences 1 '())
(count-occurrences 2 '((1 2) 3 (((2 4) 2))))
(count-occurrences 5 '((1 2) 3 (((2 4) 2))))
(count-occurrences 'a '('f 5 2 'r 'a 'a 3))
(count-occurrences 'g '(4 5 6 7))

(merge '(1 2 3) '())
(merge '() '(1 2 3))
(merge '(1 3 5) '(2 3 7 8))

(product '(a b c) '(1 2))
(product '(a b) '(1 2 3))
(product '(a b c) '())

(traverse '())
(traverse '(2 (1 () ()) (3 () ())))
(traverse '(1 (2 () ()) (3 (4 () (5 () ())) (6 () ()))))
(traverse '('f ('e () ()) ('d ('c () ('b () ())) ('a () ()))))

(findpath 0 (node 0 (null) (null)))
(findpath 3 (node 2 (node 1 (null) (null)) (node 3 (null) (null))))
(findpath 4 (node 3 (node 1 (null) (node 2 (null) (null))) (node 6 (node 5 (node 4 (null) (null)) (null)) (node 7 (null) (null)))))

((curry add1 1) 5)
(((curry map 2) '(1 2 3)) add1)
((((curry + 3) 1) 2) 3)

(is-subseq '() '(1 2 3))
(is-subseq '(1 3 5) '(1 2 4 5 6))
(is-subseq '(1 3 5) '(0 1 3 4 5 6))

(tree-reduce 10 + (null))
(tree-reduce 0 (lambda (v l r) (+ v l r)) (node 2 (node 1 (null) (null)) (node 3 (null) (null))))
(tree-reduce 0 (lambda (v l r) (+ 1 l r)) (node 2 (node 1 (null) (null)) (node 3 (null) (null))))
(tree-reduce '() (lambda (v l r) (append l (cons v r))) (node 3 (node 1 (null) (node 2 (null) (null))) (node 6 (node 5 (node 4 (null) (null)) (null)) (node 7 (null) (null)))))
(tree-reduce '() (lambda (v l r) (cons v (append l r))) (node 3 (node 1 (null) (node 2 (null) (null))) (node 6 (node 5 (node 4 (null) (null)) (null)) (node 7 (null) (null)))))
(tree-reduce '() (lambda (v l r) (append (append l r) (list v))) (node 3 (node 1 (null) (node 2 (null) (null))) (node 6 (node 5 (node 4 (null) (null)) (null)) (node 7 (null) (null)))))