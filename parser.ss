#lang racket

;;; ===================================
;;; Parser for the LEXICAL language
;;; ===================================

;;; concrete syntax
;;; ===============
;;; <exp> ::= <number> |
;;;       ::= <boolean> |
;;;       ::= (<op> <exp> ...) |
;;;       ::= <symbol>  |
;;;       ::=  (ifte <exp> <exp> <exp>) | 
;;;       ::= (assume ((<symbol> <exp>) ...) <exp>)
;;; op  ::= one of *op-symbols*

;;; Note that in the above BNF,the <symbol> in the fourth
;;; clause parses to an id-ref. 

(require "ast.ss")

(provide
  parse)

;;; parse
;;; =====
;;; parse : any/c -> ast?
;;; parse : throws parse-error when input
;;;         does not parse.
;;; It does not handle assume, id-refs and ifte.   COMPLETE THIS.

(define parse
  (lambda (d)
    (cond
      [(number? d) (number d)]
      [(boolean? d) (boolean d)]
      [(id? d ) (id-ref d) ]
      [(and
         (list? d)
         (not (null? d))
         (memq (first d) *op-symbols*))
       (prim-app (first d)
         (map parse (rest d)))]
      [(and
          (list? d)
          (not (null? d)) (eq? (first d) 'assume )) (assume (map parse (cadr d)) (parse (third d)))]
        
      [(and
           (list? d)
           (not (null? d)) (eq? (first d) 'if )) (ifte (parse (second d)) (parse (third d)) (parse (fourth d)))]
      
      [(and
           (list? d)
           (not (null? d)) (eq? (first d) 'ifte )) (ifte (parse (second d)) (parse (third d)) (parse (fourth d)))]
        
      [(and 
          (eq? (length d) 2) 
          (id? (first d))) 
         (make-bind (first d) (parse (cadr d)))]
      [else (parse-error d)])))

;;; .


;;; parse-error
;;; ===========
;;; parse-error : any/c -> void?
(define parse-error
  (lambda (d)
    (error 'parse-error "invalid syntax ~a" d)))
;;; .


;;; Unit testing
;;; ============
(require rackunit)

(check-equal? (number 5)   (parse 5) "parse5")

(check-equal? (boolean #t) (parse #t) "parse#t")

(check-equal? (prim-app '+ (list (number 3)))
              (parse '(+ 3)) "parse+")

(check-equal? (prim-app '/ (list (number 3) (boolean #t)))
              (parse '(/ 3 #t)) "parse/")

(check-exn exn? (lambda () (parse '())) "parse-error-nil")






