#!/usr/bin/env gosh

;; My experimental scripting language
(import
  (scheme base)
  (scheme read)
  (scheme write))

(define-record-type <syntax>
  (syntax op)
  syntax?
  (op syntax-op))

(define-record-type <value>
  (value typ contents)
  value?
  (typ value-type)
  (contents value-contents))

(define (type? v)
  ;; A value with a null type is a type.
  (null? (value-type v)))

(define global '(#(0) . #(0)))

(define (scope-add! scope name val)
  (set-cdr! scope (cons (car scope) (cdr scope)))
  (set-car! scope (cons name val)))

(define none-type (value '() '((name . "None"))))
(define string-type (value '() '((name . "String"))))

(define (make-procedure-type output-type input-types)
  (value '() `((name . "Procedure")
               (out-type . ,output-type)
               (in-types . ,input-types))))

(scope-add! global 'quote (syntax (lambda (expr scope)
                              (cadr expr))))
(scope-add! global 'lambda (syntax (lambda (expr scope)
                               (eval-lambda expr scope))))
(scope-add! global 'var (syntax (lambda (expr scope)
                                  ;; (let NAME TYPE VALUE)
                                  (scope-add! scope
                                              (list-ref expr 1)
                                              (value (eval-expr (list-ref expr 2) scope)
                                                     (eval-expr (list-ref expr 3) scope))))))
(scope-add! global 'Number (value '() '((name . "Number"))))
(scope-add! global 'String string-type)
(scope-add! global 'Boolean (value '() '((name . "Boolean"))))
(scope-add! global 'None none-type)
(scope-add! global 'print (value (make-procedure-type none-type (list string-type))
                                 `((body . ,(lambda (scope)
                                              (lambda (s)
                                                (display s)))))))

(define (get-procedure val scope)
  ((cdr (assq 'body (value-contents val))) scope))

(define (atom? expr)
  (not (or (pair? expr) (null? expr))))

(define (eval-expr expr scope)
  (cond
   ((atom? expr)
    (if (symbol? expr)
        (let ((scope-value (assq expr scope)))
          (if scope-value
              (cdr scope-value)
              (error "Unbound variable:" expr)))
        expr))
   ((null? expr)
    (error "Empty application:" expr))
   ((not (symbol? (car expr)))
    (error "Non-procedure application" expr))
   (else
    (let ((first-index (eval-expr (car expr) scope)))
      (if (syntax? first-index)
          ((syntax-op first-index) expr scope)
          (apply apply-expr scope first-index (cdr expr)))))))

(define (apply-expr scope proc . args)
  (apply (get-procedure proc scope) args))

(define (eval-lambda expr scope)
  ;; FORM: (lambda TYP ((ARG1 TYP1) ...) EXPR ...)
  (let ((result-type (eval-expr (cadr expr) scope)))
    (unless (type? result-type)
      (error "eval-lambda" "Not a type" result-type))
    (let ((arg-types (map (lambda (p)
                            (let ((result (eval-expr (cadr p) scope)))
                              (unless (type? result)
                                (error "eval-lambda" "Not a type" result-type))
                              result))
                          (car (cddr expr)))))
      (value (make-procedure-type result-type arg-types) '()))))

(define (repl)
  (let ((next-expr (read)))
    (unless (eof-object? next-expr)
      (display (eval-expr next-expr global))
      (newline)
      (repl))))

(repl)

