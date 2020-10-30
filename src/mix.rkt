#lang racket

(require rackunit)
(require racket/trace)
(require "base.rkt")
(require "int_flow_racket.rkt")
(require "int_tm_flow.rkt")

; `program` - basic blocks. First basic block is special - `(read ...)`
; `vs0` - pairs (name, value) for static variables
(define mix
  '((read program vs0)
    (init
     (:= pp0 (head (head (tail program))))
     (:= pending (list (list pp0 vs0)))  ; (pp0, vs0) - composite label
     (:= marked (list))
     (:= residual (list (mix_generate_read_block (head program) vs0)))
     (goto loop_pending)
     )
    (loop_pending
     (if (null? pending) loop_pending_end loop_pending_body)
     )
    (loop_pending_body
     (:= pp_and_vs (head pending))
     (:= pending (tail pending))
     (:= pp (first pp_and_vs))
     (:= vs (second pp_and_vs))
     (:= marked (cons pp_and_vs marked))
     (:= bb (tail (lookup_notnull pp program)))  ; skip name of basic block
     (:= code (list pp_and_vs))  ; using `pp_and_vs` as label
     (goto loop_bb)
     )
    (loop_bb
     (if (null? bb) loop_bb_end loop_bb_body1)
     )
    (loop_bb_body1
     (:= command (head bb))
     (:= bb (tail bb))
     (goto loop_bb_case1)
     )
    (loop_bb_case1 (if (equal? (head command) ':=)     loop_bb_assign loop_bb_case2))
    (loop_bb_case2 (if (equal? (head command) 'goto)   loop_bb_goto   loop_bb_case3))
    (loop_bb_case3 (if (equal? (head command) 'if)     loop_bb_if     loop_bb_case4))
    (loop_bb_case4 (if (equal? (head command) 'return) loop_bb_return error))
    (loop_bb_assign
     (:= X (second command))
     (:= expr (third command))
     (if (contains-key? vs X) loop_bb_assign_static loop_bb_assign_dynamic)
     )
    (loop_bb_assign_static
     (:= X_value (mix_eval expr vs))
     (:= vs (mix_replace vs X X_value))
     (goto loop_bb)
     )
    (loop_bb_assign_dynamic
     (:= code1 (list ':= X (mix_reduce expr vs)))
     (:= code (append code (list code1)))
     (goto loop_bb)
     )
    (loop_bb_goto
     (:= pp2 (second command))
     (:= bb (tail (lookup_notnull pp2 program)))
     (goto loop_bb)
     )
    (loop_bb_if
     (:= if_expr (second command))
     (:= if_then (third command))
     (:= if_else (forth command))
     (:= if_expr_value (mix_reduce if_expr vs))
     (if (can-static-eval? if_expr_value) loop_bb_if_static loop_bb_if_dynamic)
     )
    (loop_bb_if_static
     (:= pp2 (if (eval if_expr_value) if_then if_else))
     (:= bb (tail (lookup_notnull pp2 program)))
     (goto loop_bb)
     )
    (loop_bb_if_dynamic
     (:= if_then_label (list if_then vs))
     (:= if_else_label (list if_else vs))
     (:= pending (mix_add_to_pending pending marked if_then_label))
     (:= pending (mix_add_to_pending pending marked if_else_label))
     (:= code1 (list 'if if_expr_value if_then_label if_else_label))
     (:= code (append code (list code1)))
     (goto loop_bb)
     )
    (loop_bb_return
     (:= expr (second command))
     (:= code1 (list 'return (mix_reduce expr vs)))
     (:= code (append code (list code1)))
     (goto loop_bb)
     )
    (loop_bb_end
     (:= residual (append residual (list code)))
     (goto loop_pending)
     )
    (loop_pending_end
     (return residual)
     )
    ))

(define (relabel blocks0)
  (define blocks (tail blocks0))  ; First basic block is special - `(read ...)`
  (define labels_old (remove-duplicates (map head blocks)))
  (define labels_new (map (lambda (i) (~a "label" i)) (stream->list (in-range (length labels_old)))))
  (define labels_map (zip labels_old labels_new))
  (cons
   (head blocks0)
   (map
    (lambda (commands)
      (define label_old (head commands))
      (define label_new (lookup_value label_old labels_map))
      (define tail_new
        (map
         (lambda (command)
           (match command
             [(list 'goto label) (list 'goto (lookup_value label labels_map))]
             [(list 'if expr label1 label2) (list 'if expr (lookup_value label1 labels_map) (lookup_value label2 labels_map))]
             [other other]
             )
           )
         (tail commands)
         ))
      (cons label_new tail_new)
      )
    blocks
    ))
  )

(define replace0_tm '((A if 0 goto D) (B right) (C goto A) (D write 1)))
(define replace0_vs (int_tm_flow_gen_vs replace0_tm))
(define replace0_input '(1 1 1 0 1 0 1))
(define replace0_flow_original (int_flow_racket mix (list int_tm_flow replace0_vs)))
(define replace0_flow (relabel replace0_flow_original))
(pretty-print replace0_flow)

(define replace0_output (int_flow_racket replace0_flow (list replace0_input)))
(writeln (list "replace0_output:" replace0_output))
(check-equal? replace0_output '(1 1 1 1 1 0 1))
