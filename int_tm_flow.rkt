#lang racket

(require rackunit)
(require "base.rkt")
(require "int_flow_racket.rkt")

; интерпретатор машины Тьюринга (на FlowChart)
(define int_tm_flow
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
     (:= Left (int_tm_flow_tail Left))
     (:= Right (cons (int_tm_flow_head Left) Right))
     (goto loop)
     )
    (command-right
     (:= Left (cons (int_tm_flow_head Right) Left))
     (:= Right (int_tm_flow_tail Right))
     (goto loop)
     )
    (command-write
     (:= Right (cons (head CommandArguments) (int_tm_flow_tail Right)))
     (goto loop)
     )
    (command-goto
     (:= I (head CommandArguments))
     (goto step)
     )
    (command-if
     (if (equal? (head CommandArguments) (int_tm_flow_head Right)) command-if1 loop)
     )
    (command-if1
     (:= I (third CommandArguments))
     (goto step)
     )
    (exit (return (append (reverse Left) Right)))
    ))

(define tm-example '((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1)))
(define test2 (int_flow_racket int_tm_flow `(,tm-example (1 1 1 0 1 0 1))))
(check-equal? test2 '(1 1 1 1 1 0 1))
