#lang racket

;;; =================
;;; Semantic Domains
;;; =================


;;; Expressible Values (types of values returned by evaluating an ast)
;;; ==================================================================

;;; expressible-value ::=  number | boolean

;;; Denotable Values  (types of values denoted by identifiers)
;;; ==========================================================

;;; denotable-value ::= number | boolean

(provide
  expressible-value?
  denotable-value?)

;;; expressible-value? is the set of things that are the
;;; results of evaluation of expressions (asts).

;;; expressible-value? : any/c -> boolean?
(define expressible-value?
  (lambda (thing)
    (or (number? thing)
      (boolean? thing))))

;;; denotable-value? :any/c -> boolean?
(define denotable-value?
  (lambda (thing)
    (or (number? thing)
      (boolean? thing))))
