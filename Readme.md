# Lexical Interpreter


## Concrete Syntax
>;;;  <exp> ::= <number> | <boolean> |(<op> <exp> …) | <symbol> | (ifte <exp> <exp> <exp>) | (assume ((<symbol> <exp>) …) <exp>) | (assume& ((<symbol> <exp>) …) <exp>) </br>
;;;   op ::= one of op-symbols


## Parser
>;;;  (parse '(+ 5 (/ 20 2 5))) := (primApp  'plus (list (num 5)  (primApp 'div (list (num 20) (num 2) (num 5) )))) </br>
;;;  (parse '(^ 2 (* 3 (- 4 3)))) := (primApp 'pow (list (num 2) (primApp 'mul (list (num 3) (primApp 'minus (list (num 4) (num 3)))))))

## Evaluator
>;;;  (eval (parse '(+ 5 (/ 20 2 5)))) := 7 </br>
;;;  (eval (parse '(^ 2 (* 3 (- 4 3))))) := 8
