#lang racket
(require eopl)
(provide (all-defined-out))
(require "op.rkt")

;;Ast : num | primApp | id | b | ifte | assume 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype Ast Ast?
  [num (n number?)]
  [id (s symbol?)]
  [primApp (op op?) (rands (list-of Ast?))]
  [bool (b boolean?)]
  [ifte (test Ast?) (then Ast?) (else Ast?)]
  [assume (binds (list-of bind?)) (exp Ast?)]
  [assume& (binds (list-of bind?)) (exp Ast?)])

;;mk-bind? [symbol? Ast?] => bind?
(define mk-bind (lambda(s a)(list s a)))

(define bind? (lambda(b)(and (symbol? (first b))(Ast? (second b)))))