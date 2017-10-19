(define (escape-html-string s)
  ;; Escapes < > & "
  (apply string-append
         (map (lambda (c)
                (cond
                 ((eqv? c #\<) "&lt;")
                 ((eqv? c #\>) "&gt;")
                 ((eqv? c #\&) "&amp;")
                 ((eqv? c #\") "&quot;")
                 (else (string c))))
              (string->list s))))

(define (mime-html . l)
  (let ((mime-string (if (null? l)
                         "Content-type: text/html; charset=UTF-8\n\n"
                         "Content-type: text/html; charset=UTF-8\n\n")))
    (display mime-string)))

(define (declare-html)
  (display "<!DOCTYPE html>\n"))

(define (sxml->xml s)
  (cond
   ((list? s)
    (let ((tag (escape-html-string (symbol->string (car s)))))
       ;; Check if there are any attributes
       (let ((has-attribute? (and (pair? (cdr s))
                                  (pair? (cadr s))
                                  (eq? '|@| (car (cadr s))))))
         (string-append
          "<" tag
          (if has-attribute?
              (apply string-append
                     (map (lambda (p)
                            (unless (symbol? (car p))
                              (error "symbol expected, got" (car p)))
                            (string-append " "
                                           (symbol->string (car p))
                                           "=\""
                                           (escape-html-string (cadr p))
                                           "\""))
                          (cdr (cadr s))))
              "")
          ">"
          (apply string-append
                 (map sxml->xml
                      (if has-attribute? (cddr s) (cdr s))))
          "</" tag ">"))))
   ((string? s)
    (escape-html-string s))
   ((symbol? s)
    (escape-html-string (symbol->string s)))
   (else
    "")))

(define (print . l)
  (for-each (lambda (s)
              (display (sxml->xml s)))
            l)
  (newline))

(define (read-entire-file path)
  (let ((f (open-input-file path)))
    (let loop ((out '())
               (next (read-char f)))
      (cond
       ((eof-object? next)
        (let ((result (list->string (reverse out))))
          (close-input-port f)
          result))
       (else
        (loop (cons next out) (read-char f)))))))

(define (load-static path)
  (display (read-entire-file path)))
