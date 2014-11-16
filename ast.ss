#lang scheme

;;; =========================================
;;; Abstract Syntax for the language LEXICAL
;;; =========================================



;;; <ast> ::= <num-ast> | <bool-ast> |
;;;           <prim-app-ast> | <if-ast>
;;;           <assume-ast>
;;;           <id-ref-ast>

;;; <num-ast>  ::= (number <number>)
;;; <bool-ast> ::= (boolean <boolean>)
;;; <prim-app-ast> ::= (prim-app <op> <ast> ...)
;;; <assume-ast>   ::=(assume
;;;                    (<bind> ...) <ast>)
;;; <id-ref-ast>   ::= (id-ref <id>)
;;; <if-ast>      ::= (ifte <ast> <ast> <ast>)
;;; <bind>     ::= (make-bind <id> <ast>)


(require eopl/eopl)

(provide
  ast
  ast?
  bind?
  number
  boolean
  id-ref
  ifte
  id?
  prim-app
  assume
  *op-symbols*
  op-symbol?
  make-bind
  bind-id
  bind-ast)

(define-datatype ast ast?
  [number (datum number?)]
  [boolean (datum boolean?)]
  [prim-app (op op-symbol?) (rands (list-of ast?))]
  [id-ref (sym id?)]
  [assume (binds  (list-of bind?)) (body ast?)]
  [ifte (test ast?) (then ast?) (else ast?)])

(define-datatype bind bind?
  [make-bind (b-id id?) (b-ast ast?)])

;;; bind-id : bind? -> id?
(define bind-id
  (lambda (b)
    (cases bind b
      [make-bind (b-id b-ast) b-id])))

;;; bind-ast : bind? -> ast?
(define bind-ast
  (lambda (b)
    (cases bind b
      [make-bind (b-id b-ast) b-ast])))

(define id? symbol?)

(define *op-symbols*
  '(+ - * /
    < <= > >= not eq? 0?))

;;; op-symbol? : symbol? -> bool
(define op-symbol?
  (lambda (x)
    (if (memq x *op-symbols*)
      #t
      #f)))

;;; unit Testing
;;; ============

;;; Racket's unit testing framework
(require rackunit)


(define-simple-check (check-ast? thing)
   (ast? thing))

(check-ast? (number 5) "number test")
(check-ast? (boolean #t) "boolean test")

(check-ast? (prim-app '+ (list (number 5) (number 6))) "prim-app test")
(check-ast? (prim-app '+ (list (number 5))) "prim-app test")

(check-ast? (id-ref 'x) "id-ref-x test")

(check-ast? (assume
              (list
                (make-bind 'x (number 5))
                (make-bind 'y (number 6)))
              (prim-app '+ (list (id-ref 'x) (id-ref 'y))))
  "assume test")


(check-ast? (assume
              '()
              (prim-app '+ (list (id-ref 'x) (id-ref 'y))))
  "assume empty test")

(check-pred bind? (make-bind 'x (number 5)))
(check-pred bind? (make-bind 'y (prim-app '+ (list (number 5) (number 6)))))

(check-equal? (bind-id (make-bind 'y (number 6))) 'y)
(check-equal? (bind-id (make-bind 'x (prim-app '+ (list (number 5) (number 6))))) 'x)

(check-ast? (bind-ast (make-bind 'y (number 6))))
(check-ast? (bind-ast (make-bind 'y (prim-app '+ (list (number 5) (number 6))))))

(check-pred id? 'x "x-id")


(check-true (op-symbol? '<))
(check-true (op-symbol? '*))
(check-true (op-symbol? '+))
(check-true (op-symbol? '/))










