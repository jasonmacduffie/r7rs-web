(define-library (macduffie frontend)
  (import (scheme base)
          (scheme write)
          (scheme cxr)
          (srfi 28))
  (export script generate-ecmascript)
  (include "./frontend.body.scm"))
