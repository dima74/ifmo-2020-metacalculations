#lang racket

(define
  (int program data)
  (int_impl program 0 null (car data) (cdr data))
)
(define
  (int_impl program i data_l0 data_curr data_r0)
  (let (
        [data_l (if (null? data_l0) '(_) data_l0)]
        [data_r (if (null? data_r0) '(_) data_r0)]
        )
    (if
     (= i (length program))
     (append (reverse data_l) (cons data_curr data_r))
     (match (list-ref program i) 
       [(list 'left) (int_impl program (+ 1 i) (cdr data_l) (car data_l) (cons data_curr data_r))]
       [(list 'right) (int_impl program (+ 1 i) (cons data_curr data_l) (car data_r) (cdr data_r))]
       [(list 'write a) (int_impl program (+ 1 i) data_l a data_r)]
       [(list 'goto i2) (int_impl program i2 data_l data_curr data_r)]
       [(list 'if a 'goto i2)
        (if
         (= data_curr a)
         (int_impl program i2 data_l data_curr data_r)
         (int_impl program (+ 1 i) data_l data_curr data_r)
         )]
       )
     )
    )
  )

(define program1
  '(
    (if 0 goto 3)
    (right)
    (goto 0)
    (write 1)
    )
  )
(int program1 '(1 1 0 1 0 0))
