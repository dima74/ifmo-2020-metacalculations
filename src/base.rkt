#lang racket

(require racket/trace)
(provide (all-defined-out))

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))

; base helper functions
(define head car)
(define tail cdr)
(define (zip l1 l2) (map list l1 l2))

(define first car)
(define second cadr)
(define third caddr)
(define forth cadddr)

; (define (list-contains? list value) (if (member value list) #t #f))
(define (contains-key? list_of_pairs key) (if (assoc key list_of_pairs) #t #f))

; usage: (lookup key list), where `list` contains pairs
; return pair (key, _)
(define lookup_nullable assoc)
(define (lookup_notnull key lst)
  (define entry (assoc key lst))
  (if entry entry (raise 'lookup_not_found))
  )
(define (lookup_value key lst)
  (second (lookup_notnull key lst))
  )
;(trace lookup_value)

; for int_tm_flow
(define (int_tm_flow_head list) (if (null? list) '(_) (head list)))
(define (int_tm_flow_tail list) (if (null? list) '() (tail list)))
(define
  (int_tm_flow_lookup commands label)
  (memf
   (lambda (command) (equal? (head command) label))
   commands
   ))

; for mix
(define (mix_substitute expr vs)
  (if
   (list? expr)
   (if
    (equal? (head expr) 'quote)
    expr
    (let ([head2 (mix_substitute1 (head expr) vs)]
          [tail2
           (map
            (lambda (arg) (mix_substitute arg vs))
            (tail expr)
            )])
      (cons head2 tail2))
    )
   ; `expr` is string
   (mix_substitute1 expr vs)
   )
  )
(define (mix_substitute1 name vs)
  (let ([entry (lookup_nullable name vs)])
    (if entry (list 'quote (second entry)) name)
    )
  )
(define (mix_eval expr vs)
  (mix_eval_impl (mix_substitute expr vs))
  )
(define (mix_eval_impl expr)  ; separate function for better debugging
  ;(writeln (head expr))
  ;(writeln (tail expr))
  (eval expr ns)
  )
(define (mix_reduce expr vs)
  (mix_reduce_impl (mix_substitute expr vs))
  )
(define (mix_reduce_impl expr)
  (if
   (pair? expr)  ; list? вернёт #t на пустом списке
   (with-handlers
       ([exn:fail:contract:variable?
         (lambda (_)
           (define tail2
             (map
              (lambda (arg) (mix_reduce_impl arg))
              (tail expr)
              ))
           (cons (head expr) tail2)
           )])
     (list 'quote (eval expr ns)))
   expr
   ))
(define (can-static-eval? expr)
  (with-handlers
      ([exn:fail:contract:variable?
        (lambda (_) false)])
    (eval expr ns)
    true
    )
  )
;(trace mix_substitute)
;(trace mix_eval)
;(trace mix_eval_impl)
;(trace mix_reduce)
;(trace mix_reduce_impl)

(define (mix_generate_read_block block vs)
  (define variables_all (tail block))
  (define variables_dynamic (filter (lambda (v) (not (contains-key? vs v))) variables_all))
  (cons 'read variables_dynamic)
  )
(define (mix_replace pairs key value)
  (define pair (head pairs))
  (if
   (equal? key (head pair))
   (cons (list (head pair) value) (tail pairs))
   (cons pair (mix_replace (tail pairs) key value))
   ))
(define (mix_add_to_pending pending marked label)
  (if
   (or (member label marked) (member label pending))
   pending
   (cons label pending)
   )
  )
