
(define-library (macduffie web)
  (import (scheme base)
          (scheme write)
          (scheme process-context))
  (export print mime-html declare-html (rename display print-raw) args-ref)
  (include "./web.body.scm"))
