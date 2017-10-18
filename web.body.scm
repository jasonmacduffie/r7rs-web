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

(define (mime-xhtml . l)
  (let ((mime-string (if (null? l)
                         "Content-type: application/xhtml+xml; charset=UTF-8\n\n"
                         "Content-type: application/xhtml+xml; charset=UTF-8\n\n")))
    (display mime-string)))

(define (declare-xhtml)
(display
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"))

(define (sxml->xml s)
  (cond
   ((list? s)
    (let ((tag (escape-html-string (symbol->string (car s)))))
      (string-append
       "<" tag ">" (apply string-append (map sxml->xml (cdr s))) "</" tag ">")))
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
