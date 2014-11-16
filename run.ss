#lang scheme

(provide
  *init-env*
  run)

(require eopl/eopl)

(require "eval-ast.ss")
(require "ast.ss")
(require "env.ss")
(require "parser.ss")

(define *init-env*
  (extended-env '(x y) '(4 6) (empty-env)))

;;; run : concrete-syntax? -> expressible-value?
(define run
  (lambda (raw)
    (eval-ast (parse raw) *init-env*)))

;;; these should be unit tests instead!
;;; (run 5) => 5
;;; (run '(ifte (> 3 4) x y)) => 6