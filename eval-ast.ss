#lang scheme

;;; ==================================
;;; Evaluator for the LEXICAL language
;;; ==================================
  
(require eopl/eopl)
(require "ast.ss")
(require "op.ss")
(require "env.ss")
(require "semantic-domains.ss")


(provide
   eval-ast)

;;; eval-ast : [ast? env?]-> expressible-value?
;;; eval-ast :  throws error 
(define eval-ast
  (lambda (p env)
    (cases ast p
      [number (datum) datum]
      [boolean (datum) datum]
      [id-ref (symb) (lookup-env env symb)]
      
      [prim-app (op operand)
                          (let ([args (map
                                       (lambda (operand)
                                              (eval-ast operand
                                                    env ) )
                                       operand ) ]  )
                          (apply-prim-op op args ) )  ]
      [assume (binds body)
        (let* ([ids  (map bind-id binds)]
               [asts (map bind-ast binds)]
               [vals (map (lambda (p)
                            (eval-ast p env))
                          asts)]
               [new-env
                (extended-env ids vals env)])
          (eval-ast body new-env))]
      [ifte (test then else-ast)
        (let ([b (eval-ast test env)])
          (if (boolean? b)
            (eval-ast (if b then else-ast) env)
            (error 'eval-ast "Required boolean" p)))])))

;;; match-sig? takes a flat-contract signature sig? (like number?
;;; boolean?) and a value val and checks if the val
;;; satisfies sig?
;;; match-sig? : [flat-contract? any/c] -> boolean?
;;; Example
;;; -------
;;; (match-sig? number? 5) => #t
;;; (match-sig? boolean? 5) => #f

;;;(define match-sig? ...COMPLETE THIS...)
(define match-sig?
  (lambda (sig? val)
    (sig? val)))

;;; apply-prim-op : [op-symbol? (listof expressible-value?)] -> expressible-value?

;;; apply-prim-op : throws error when number or type of args
;;; do not match the signature of op identified by opsym.

(define apply-prim-op
  (lambda (opsym args)
    (let* ([op (op-find opsym)]
           [sig (op-sig op)]
           [args-sig (rest sig)])
      (cond
       [(and
         (= (length args-sig) (length args))
         (andmap match-sig? args-sig args))
        (apply (op-prim op)  args)]
       [#t (error 'apply-prim-op "Invalid Type (~a)" opsym)]))))


(require rackunit)

(define-simple-check (check-eval thing1 thing2)
   (eval-ast thing1 thing2))
(define e1
  (extended-env '(x y z) '(1 2 3) (empty-env)))
(define e2
  (extended-env '(w x) '(5 6) e1))

(check-equal? 11 (eval-ast (prim-app '+ (list (id-ref 'x) (id-ref 'w))) e2)) 
(check-equal? 8 (eval-ast (prim-app '+ (list (id-ref 'x) (id-ref 'y))) e2)) 
(check-equal? (eval-ast (assume (list (make-bind 'x (number 8))
                          (make-bind 'y (number 7)))
                    (prim-app '+ (list (id-ref 'x) (id-ref 'y)))) e2) 15)
(check-equal? (eval-ast (assume (list (make-bind 'x (number 8))
                          (make-bind 'y (number 7)))
                    (prim-app '+ (list (id-ref 'x) (id-ref 'w)))) e2) 13)
(check-equal?  (eval-ast (id-ref'w) e2) 5) 

