
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

(define (rewrite-cond expr)
  ;; cond is complicated enough to require a separate procedure
  (if (null? (cdr expr))
      '()
      (let loop ((in (cddr expr))
                 (out `((" if ( "
                         ,@(rewrite-line (caadr expr))
                         " ) {\n"
                         ,@(rewrite-multiple-lines (cdadr expr))
                         "\n}\n"))))
        (if (null? in)
            (apply append (reverse out))
            (loop (cdr in)
                  (cons
                   (if (eq? (caar in) 'else)
                       `(" else {\n"
                         ,@(rewrite-multiple-lines (cdar in))
                         "\n}\n")
                       `(" else if ( "
                         ,@(rewrite-line (caar in))
                         " ) {\n"
                         ,@(rewrite-multiple-lines (cdar in))
                         "\n}\n"))
                   out))))))

(define (rewrite-line expr)
  (cond
   ((pair? expr)
    (cond
     ((eq? (car expr) 'lambda)
      `(" function ( "
        ,(join-strings (map symbol->string (cadr expr)) ",")
        " )  {\n"
        ,@(rewrite-multiple-lines (cddr expr))
        " \n}\n"))
     ((eq? (car expr) 'begin)
      `(" {\n"
        ,@(rewrite-multiple-lines (cdr expr))
        " \n}\n"))
     ((eq? (car expr) 'define)
      (if (list? (cadr expr))
          (rewrite-line
           `(define ,(caadr expr)
              (lambda ,(cdadr expr)
                ,@(cddr expr))))
          `("var "
            ,(symbol->string (cadr expr))
            " = "
            ,@(rewrite-line (list-ref expr 2))
            "\n")))
     ((eq? (car expr) 'set!)
      `(,(symbol->string (cadr expr))
        " = "
        ,@(rewrite-line (list-ref expr 2))
        "\n"))
     ((eq? (car expr) 'if)
      `(" if ( "
        ,@(rewrite-line (cadr expr))
        " ) {\n"
        ,@(rewrite-line (list-ref expr 2))
        "\n}\n"
        ,@(if (< (length expr) 4)
              '()
              `(" else {\n"
                ,@(rewrite-line (cadr (cddr expr)))
                "\n}\n"))))
     ((eq? (car expr) 'while)
      `(" while ( "
        ,@(rewrite-line (cadr expr))
        " ) {\n"
        ,@(rewrite-multiple-lines (cddr expr))
        "\n}\n"))
     ((eq? (car expr) 'for)
      `(" for ( "
        ,@(rewrite-line (list-ref expr 1))
        " ; "
        ,@(rewrite-line (list-ref expr 2))
        " ; "
        ,@(rewrite-line (list-ref expr 3))
        " ) {\n"
        ,@(rewrite-multiple-lines (cddr (cddr expr)))
        "\n}\n"))
     ((eq? (car expr) 'cond)
      (rewrite-cond expr))
     ((eq? (car expr) 'vector)
      `(" [ "
        ,(join-expressions (cdr expr) ",")
        " ] "))
     ((eq? (car expr) 'object)
      `(" {\n"
        ,(join-strings (map (lambda (p)
                              (string-append (format "~s" (symbol->string (car p)))
                                             ":"
                                             (apply string-append (rewrite-line (cdr p)))))
                            (cdr expr))
                       ",")
        "\n}\n"))
     ((eq? (car expr) '+)
      `(" ( "
        ,(join-expressions (cdr expr) "+")
        " ) "))
     ((eq? (car expr) '-)
      `(" ( "
        ,@(rewrite-line (car expr))
        " - ( "
        ,(join-expressions (cddr expr) "+")
        " )) "))
     ((eq? (car expr) '*)
      `(" ( "
        ,(join-expressions (cdr expr) "*")
        " ) "))
     ((eq? (car expr) '/)
      `(" ( "
        ,@(rewrite-line (car expr))
        " / ( "
        ,(join-expressions (cddr expr) "*")
        " )) "))
     ((eq? (car expr) '%)
      `(" (( "
        ,@(rewrite-line (list-ref expr 1))
        " ) % ( "
        ,@(rewrite-line (list-ref expr 2))
        " )) "))
     ((eq? (car expr) '=)
      `(" ( "
        ,(join-expressions (cdr expr) "===")
        " ) "))
     ((eq? (car expr) '~=)
      `(" ( "
        ,(join-expressions (cdr expr) "==")
        " ) "))
     ((eq? (car expr) 'ref)
      `(" ( "
        ,@(rewrite-line (cadr expr))
        " [ "
        ,@(rewrite-line (caddr expr))
        " ] ) "))
     ((eq? (car expr) 'return)
      `(" return "
        ,@(rewrite-line (cadr expr))
        "\n"))
     ((eq? (car expr) 'dot)
      `(,(join-strings (map symbol->string (cdr expr)) ".")))
     (else
      `(,@(rewrite-line (car expr))
        " ( "
        ,(join-expressions (cdr expr) ",")
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

(define (join-expressions exprs join-value)
  (join-strings (map (lambda (l) (apply string-append l))
                     (map rewrite-line exprs)) join-value))

(define-syntax script
  (syntax-rules ()
    ((_ l ...)
     (display (apply string-append `("<script>" ,@(rewrite-multiple-lines '(l ...)) "</script>"))))))

(define (generate-ecmascript . l)
  (apply string-append (rewrite-multiple-lines l)))

