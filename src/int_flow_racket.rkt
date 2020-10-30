#lang racket

(require rackunit)

(require "base.rkt")

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))

; интерпретатор FlowChart (на Racket)
(provide int_flow_racket)

(define
  (int_flow_racket program data)
  (run_block
   (head (second program))
   (tail program)
   (zip (tail (head program)) data)
   ))

(define
  (run_block block_name blocks variables)
  (define block (assoc block_name blocks))
  (run_commands (tail block) blocks variables)
  )

(define
  (update_variable variables name value)
  (if
   (null? variables)
   (list (list name value))
   (if
    (equal? (head (head variables)) name)
    (cons (list name value) (tail variables))
    (cons (head variables) (update_variable (tail variables) name value))
    ))
  )

(define
  (eval-with-variables expr variables)
  (define assigments (map (lambda (entry) (list (first entry) (list 'quote (second entry)))) variables))
  (define expr2 `(let ,assigments ,expr))
  (eval expr2 ns)
  )

(define
  (run_commands commands blocks variables)
  (match (head commands)
    [(list ':= variable expr) (run_commands (tail commands) blocks (update_variable variables variable (eval-with-variables expr variables)))]
    [(list 'goto label) (run_block label blocks variables)]
    [(list 'if expr label1 label2) (run_block (if (eval-with-variables expr variables) label1 label2) blocks variables)]
    [(list 'return expr) (eval-with-variables expr variables)]
    [(list 'writeln expr)
     (writeln (eval-with-variables expr variables))
     (run_commands (tail commands) blocks variables)
     ]
    ))

(define find_name
  '((read name namelist valuelist)
    (search (if (equal? name (head namelist)) found cont))
    (cont (:= valuelist (tail valuelist))
          (:= namelist (tail namelist))
          (goto search))
    (found (return (head valuelist)))
    ))

(define test1 (int_flow_racket find_name '(y (x y z) (1 2 3))))
(check-equal? test1 '2)
