#!/usr/bin/env gosh

;; My experimental scripting language
(import
  (scheme base)
  (scheme read)
  (scheme write))

(define-record-type <type>
  (type name)
  type?
  (name type-name))

(define-record-type <syntax>
  (syntax op)
  syntax?
  (op syntax-op))

(define global '())

(define (add-global! name val)
  (set! global (cons (cons name val) global)))

(add-global! 'quote (syntax (lambda (expr scope)
                              (cadr expr))))
(add-global! 'lambda (syntax (lambda (expr scope)
                               (eval-lambda expr scope))))
(add-global! 'Number (type "Number"))
(add-global! 'String (type "String"))
(add-global! 'Boolean (type "Boolean"))

(define (atom? expr)
  (not (or (pair? expr) (null? expr))))

(define (eval-expr expr scope)
  (cond
   ((atom? expr)
    (if (symbol? expr)
        (let ((scope-value (assq expr scope)))
          (if scope-value
              (cdr scope-value)
              '())) ;; Should be an error
        expr))
   ((null? expr)
    '()) ;; Should be an error
   ((not (symbol? (car expr)))
    '()) ;; Should be an error
   (else
    (let ((first-index (eval-expr (car expr) scope)))
      (if (syntax? first-index)
          ((syntax-op first-index) expr scope)
          (apply-expr first-index (cdr p)))))))

(define (eval-lambda expr scope)
  ;; FORM: (lambda typ ((arg1 typ1) expr ...)
  (let ((result-type (eval-expr (cadr expr) scope)))
    (unless (type? result-type)
      (error "eval-lambda" "Not a type" result-type))
    (let ((arg-types (map (lambda (p)
                            (let ((result (eval-expr (cdr p) scope)))
                              (unless (type? result)
                                (error "eval-lambda" "Not a type" result-type))
                              result))
                          (car (cddr expr)))))
      100)))

(define (repl)
  (let ((next-expr (read)))
    (unless (eof-object? next-expr)
      (display (eval-expr next-expr global))
      (newline)
      (repl))))

(repl)

