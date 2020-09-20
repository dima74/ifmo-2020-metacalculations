#lang racket

(require racket/trace)
(require rackunit)

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))

(define head car)
(define tail cdr)
(define (zip l1 l2) (map list l1 l2))

(define first car)
(define second cadr)
(define third caddr)

; интерпретатор FlowChart (на Racket)
(define
  (int program data)
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
    (search (if (equal? name (car namelist)) found cont))
    (cont (:= valuelist (cdr valuelist))
          (:= namelist (cdr namelist))
          (goto search))
    (found (return (car valuelist)))
    ))

;(trace run_block)
;(trace run_commands)
;(trace eval-with-variables)
(define test1 (int find_name '(y (x y z) (1 2 3))))
(check-equal? test1 '2)

; интерпретатор машины Тюьринга (на FlowChart)
(define (mtHead list) (if (null? list) '(_) (head list)))
(define (mtTail list) (if (null? list) '() (tail list)))
(define tm-int
  '((read Commands Right)
    (init
     (:= Left '())
     (:= I 0)
     (goto step)
     )
    (loop
     (:= I (+ I 1))
     (goto step)
     )
    (step
     ;(writeln (list I Left Right))
     (if (equal? I (length Commands)) exit cases)
     )
    (cases
        (:= Command (tail (list-ref Commands I)))
      (:= CommandType (head Command))
      (:= CommandArguments (tail Command))
      (goto case1)
      )
    (case1 (if (equal? CommandType 'left)  command-left  case2))
    (case2 (if (equal? CommandType 'right) command-right case3))
    (case3 (if (equal? CommandType 'write) command-write case4))
    (case4 (if (equal? CommandType 'goto)  command-goto  case5))
    (case5 (if (equal? CommandType 'if)    command-if    error))

    (command-left
     (:= Left (mtTail Left))
     (:= Right (cons (mtHead Left) Right))
     (goto loop)
     )
    (command-right
     (:= Left (cons (mtHead Right) Left))
     (:= Right (mtTail Right))
     (goto loop)
     )
    (command-write
     (:= Right (cons (head CommandArguments) (mtTail Right)))
     (goto loop)
     )
    (command-goto
     (:= I (head CommandArguments))
     (goto step)
     )
    (command-if
     (if (equal? (head CommandArguments) (mtHead Right)) command-if1 loop)
     )
    (command-if1
     (:= I (third CommandArguments))
     (goto step)
     )
    (exit (return (append (reverse Left) Right)))
    ))

(define tm-example '((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1)))
(define test2 (int tm-int `(,tm-example (1 1 1 0 1 0 1))))
(check-equal? test2 '(1 1 1 1 1 0 1))
