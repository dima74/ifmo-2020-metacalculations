#lang racket

(provide (all-defined-out))

; base helper functions
(define head car)
(define tail cdr)
(define (zip l1 l2) (map list l1 l2))

(define first car)
(define second cadr)
(define third caddr)

; for int_tm_flow
(define (int_tm_flow_head list) (if (null? list) '(_) (head list)))
(define (int_tm_flow_tail list) (if (null? list) '() (tail list)))