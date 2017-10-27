
;; A specialized Lisp for EcmaScript programming
(import (scheme base))

(define (join-strings l join-value)
  ;; Join a list of strings l with join-value between them
  (if (null? l)
      ""
      (let loop ((in l) (out '()))
        (if (null? in)
            (apply string-append (cdr (reverse out)))
            (loop (cdr in)
                  (cons (car in)
                        (cons join-value out)))))))

(define (rewrite-line expr)
  (cond
   ((pair? expr)
    (cond
     ((eq? (car expr) 'lambda)
      `(" function ( "
        ,(join-strings (map symbol->string (cadr expr)) ",")
        " )  { "
        ,@(rewrite-multiple-lines (cddr expr))
        " } "))
     ((eq? (car expr) 'begin)
      `(" { "
        ,@(rewrite-multiple-lines (cdr expr))
        " } "))
     ((eq? (car expr) 'define)
      (if (list? (cadr expr))
          (error "rewrite-line" "not yet implemented: define function")
          `(" var "
            ,(symbol->string (cadr expr))
            " = "
            ,@(rewrite-line (list-ref expr 2))
            " ; ")))
     ((eq? (car expr) 'set!)
      `(,(symbol->string (cadr expr))
        " = "
        ,@(rewrite-line (list-ref expr 2))
        " ; "))
     ((eq? (car expr) 'if)
      `(" if ( "
        ,@(rewrite-line (cadr expr))
        " ) { "
        ,@(rewrite-line (list-ref expr 2))
        " } "
        ,@(if (< (length expr) 4)
              '()
              `(" else { "
                ,@(rewrite-line (cadr (cddr expr)))
                " } "))))
     ((eq? (car expr) 'while)
      `(" while ( "
        ,@(rewrite-line (cadr expr))
        " ) { "
        ,@(rewrite-multiple-lines (cddr expr))
        " } "))
     (else
      `(,(symbol->string (car expr))
        " ( "
        ,(join-expressions (cdr expr))
        " ) "))))
   ((symbol? expr)
    `(" " ,(symbol->string expr) " "))
   ((number? expr)
    `(" " ,(number->string expr) " "))
   ((boolean? expr)
    (if expr '(" true ") '(" false ")))
   ((string? expr)
    `(" " ,(format "~s" expr) " "))
   (else
    '())))

(define (rewrite-multiple-lines exprs)
  (apply append (map rewrite-line exprs)))

(define (join-expressions exprs)
  (join-strings (map (lambda (l) (apply string-append l))
                     (map rewrite-line exprs)) ","))

(define-syntax script
  (syntax-rules ()
    ((_ l ...)
     (display (apply string-append `("<script>" ,@(rewrite-multiple-lines '(l ...)) "</script>"))))))

