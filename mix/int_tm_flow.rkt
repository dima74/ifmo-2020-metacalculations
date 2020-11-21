#lang racket

(require rackunit)
(require "base.rkt")
(require "int_flow_racket.rkt")

; интерпретатор машины Тьюринга (на FlowChart)
(provide int_tm_flow int_tm_flow_gen_vs)
; (define int_tm_flow_static '(Commands CommandsTail Command CommandType CommandArguments))
(define
  (int_tm_flow_gen_vs Commands)
  (list
   (list 'Commands Commands)
   '(CommandsTail 0)
   '(Command 0)
   '(CommandType 0)
   '(CommandArguments 0)
   '(Label 0)
   )
  )
(define int_tm_flow
  '((read Commands Right)
    (init
     (:= Left '())
     (:= CommandsTail Commands)
     (goto step)
     )
    (loop
     (:= CommandsTail (tail CommandsTail))
     (goto step)
     )
    (step
     (if (empty? CommandsTail) exit cases)
     )
    (cases
      (:= Command (tail (head CommandsTail)))  ; tail чтобы пропустить label
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
     (:= Right (cons (first CommandArguments) (int_tm_flow_tail Right)))
     (goto loop)
     )
    (command-goto
     (:= Label (first CommandArguments))
     (:= CommandsTail (int_tm_flow_lookup Commands Label))
     (:= Label 0)  ; обнуление локальной переменной
     (goto step)
     )
    (command-if
     (if (equal? (head CommandArguments) (int_tm_flow_head Right)) command-if1 loop)
     )
    (command-if1
     (:= Label (third CommandArguments))
     (:= CommandsTail (int_tm_flow_lookup Commands Label))
     (:= Label 0)  ; обнуление локальной переменной
     (goto step)
     )
    (exit (return (append (reverse Left) Right)))
    ))

(define (int_tm_flow_test)
  (define tm_program '((A if 0 goto D) (B right) (C goto A) (D write 1)))
  (define tm_program_input '(1 1 1 0 1 0 1))
  (define test (int_flow_racket int_tm_flow (list tm_program tm_program_input)))
  (check-equal? test '(1 1 1 1 1 0 1))
  )
;(int_tm_flow_test)