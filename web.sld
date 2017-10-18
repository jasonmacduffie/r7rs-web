
(define-library (macduffie web)
  (import (scheme base)
          (scheme write))
  (export print mime-html mime-xhtml declare-html declare-xhtml)
  (include "./web.body.scm"))
