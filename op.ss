#lang racket


(provide
  *ops*
  op-name
  op-prim
  op-sig
  op-find)
  
(require eopl/eopl)
(require "ast.ss")
(require "semantic-domains.ss")


;;; nonzero? : any/c -> boolean?
(define nonzero?
  (lambda (n)
    (and (number? n)
      (not (zero? n)))))

;;; Operators
;;; =========
(define-struct op (name prim sig))
(define +op   (make-op '+  +   (list number? number? number?)))
(define -op    (make-op '-     -     (list number? number? number?)))
(define *op    (make-op '*     *     (list number? number? number?)))
(define /op    (make-op '/     /     (list number? number? nonzero?)))
(define <op    (make-op '<     <     (list boolean? number? number?)))
(define <=op   (make-op '<=    <=    (list boolean? number? number?)))
(define >op    (make-op '>     >     (list boolean? number? number?)))
(define >=op   (make-op '>=    >=    (list boolean? number? number?)))
(define notop  (make-op 'not   not   (list boolean? boolean?)))
(define eq?op  (make-op 'eq?   eq?   (list boolean? expressible-value? expressible-value?)))
(define 0?op   (make-op '0?    zero? (list boolean? number?)))

(define *ops*
  (list +op -op *op /op <op <=op >op >=op  notop eq?op 0?op))

(define op-find
  (lambda (opsym)
    (findf (lambda (op)
             (eq? opsym (op-name op)))
           *ops*)))

(require rackunit)

;;nonzero?
(check-equal? (nonzero? 1) #t)
(check-equal? (nonzero? 0) #f)

;;op-find
(check-equal? (op-name (op-find '+)) '+)
(check-equal? (op-name (op-find '*)) '*)
(check-equal? (op-name (op-find '/)) '/)
(check-equal? (op-name (op-find '-)) '-)
(check-equal? (op-name (op-find 'eq?)) 'eq?)
(check-equal? (op-name (op-find '<)) '<)
(check-equal? (op-name (op-find '>)) '>)
(check-equal? (op-name (op-find '>=)) '>=)
(check-equal? (op-name (op-find '<=)) '<=)
(check-equal? (op-name (op-find '0?)) '0?)






