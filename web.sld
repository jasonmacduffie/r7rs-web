
(define-library (macduffie web)
  (import (scheme base)
          (scheme write))
  (export print mime-html declare-html (rename display print-raw))
  (include "./web.body.scm"))
