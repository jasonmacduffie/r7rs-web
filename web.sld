
(define-library (macduffie web)
  (import (scheme base)
          (scheme write)
          (scheme file)
          (scheme load))
  (export print mime-html declare-html (rename display print-raw) load load-static)
  (include "./web.body.scm"))
