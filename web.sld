
(define-library (macduffie web)
  (import (scheme base)
          (scheme write))
  (export print mime-html declare-html)
  (include "./web.body.scm"))
