#!/usr/bin/env gosh

;; My experimental scripting language
(import
  (scheme base)
  (scheme read)
  (scheme write))

(define global '())

(define (repl)
  (let ((next-expr (read)))
    (unless (eof-object? next-expr)
      (display next-expr)
      (newline)
      (repl))))

(repl)

