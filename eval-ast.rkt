#lang racket
(require eopl/eopl)
(provide (all-defined-out))
(require "ast.rkt" "op.rkt" "env.rkt" "parser.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; reduce: [(f x u)=> x ..]
(define reduce (lambda (f x ls)
    (if (null? ls) x
    (reduce f (f x (car ls)) (cdr ls)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;eval-ast : [Ast? env?]--> ans? or error
;; throws error for arity mismatch and unbound identifier
(define eval-ast
  (lambda (ast env)
    (cases Ast ast
      [num (n)  n]
      [bool(b) b]
      [id (s)  (denotable->expressible(lookup-env s env))]
      [ifte (test then else) (let ((b (eval-ast test env)))
                                  (cond
                                  [(boolean? b) (if b (eval-ast then env) (eval-ast else env))]
                                  (else (error "eval-ast: assume-ifte-test-not-boolean"))))]

      [assume (binds body) (let ((tpls (map (lambda(u)
                                                   (mk-tuple (first u)
                                                             (expressible->denotable 
                                                             (eval-ast (second u) env)))) 
                                           binds))) ;eval-astuate asts to get values to be bound to identifiers

                                  (eval-ast body (extended-env tpls env)))] ;eval-astuate the body in the extended envirnment
     
      [assume& (binds body) (let ((tpls (reduce (lambda(i u)
                                                  (extended-env (list (mk-tuple (first u)
                                                             (expressible->denotable 
                                                             (eval-ast (second u) i)))) i))
                                                env binds))) ;eval-astuate asts to get values to be bound to identifiers

                                  (eval-ast body tpls))] ;eval-astuate the body in the extended envirnment

      [primApp (s rands) (letrec ((proc (op s))     ;get the operator procedure
                                   (args (map (lambda(u)(eval-ast u env)) rands)))   ;eval-astuate operands to get actual arguments
                           (apply proc args))]
      )))