#lang scheme

;;; ============
;;; Environments
;;; ============


;; An environment is a repository of mappings from symbols
;; to values, ie., values *denoted* by identifiers.

;;; Environment abstract data type
;;;

;;; Type predicate
;;; --------------
;;; env? : any/c -> boolean?

;;; lookup-env : [env?  symbol?] -> any/c
;;; lookup-env : throws error if symbol is not in the environment

(provide
  env
  env?
  empty-env
  extended-env
  lookup-env)

(require eopl/eopl)


;;; An env is a union type of either
;;; * an empty environment OR

;;; * an extended environment consisting of a list of
;;;   symbols, a list of denotable values and an outer
;;;   environment.

;;;(define-datatype env env? ...COMPLETE THIS...)
 
(define-datatype env env?
  [empty-env]
  [extended-env
    (syms (list-of symbol?))
    (vals (list-of any/c))
    (outer-env env?)])
 
(define empty-env?
  (lambda (e)
    (cases env e
      [empty-env () #t]
      [else #f])))
 
(define extended-env?
  (lambda (e)
    (cases env e
      [empty-env () #f]
      [else #t])))
 
(define list-index
  (lambda (ls a)
    (letrec ([loop
               (lambda (ls ans)
                 (cond
                   [(null? ls) -1]
                   [(eq? (first ls) a) ans]
                   [#t (loop (rest ls) (+ 1 ans))]))])
      (loop ls 0))))
 
(define lookup-env
  (lambda (e x)
    (cases env e
      [empty-env ()
        (error
          'lookup-env
          "unbound identifier ~a" x)]
      [extended-env (syms vals outer-env)
        (let ([j (list-index syms x)])
          (cond
            [(= j -1) (lookup-env outer-env x)]
            [#t (list-ref vals j)]))])))

;;; Unit testing
;;; ============
(require rackunit)

(check-pred env? (empty-env) "env?-empty-env")
(check-pred empty-env? (empty-env) "empty-env?-empty-env")
(check-exn exn? (lambda () (lookup-env (empty-env) 'a)) "lookup-empty-env-a")

(define e1
  (extended-env '(x y z) '(1 2 3) (empty-env)))

(check-pred env?  e1 "env?-extended-env")
(check-pred extended-env? e1 "extended-env?-extended-env")

(check-equal? 1 (lookup-env e1 'x) "lookup-e1-x")
(check-equal? 2 (lookup-env e1 'y) "lookup-e1-y")
(check-exn exn? (lambda () (lookup-env e1 'a)) "lookup-e1-a")

(define e2
  (extended-env '(w x) '(5 6) e1))

(check-equal? 5 (lookup-env e2 'w) "lookup-e2-w")
(check-equal? 6 (lookup-env e2 'x) "lookup-e2-x")
(check-equal? 2 (lookup-env e2 'y) "lookup-e2-y")
(check-equal? 3 (lookup-env e2 'z) "lookup-e2-z")
(check-exn exn? (lambda () (lookup-env e2 'a)) "lookup-e2-a")







    
    
    
  

