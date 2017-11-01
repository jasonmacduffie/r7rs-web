
(define-library (macduffie web)
  (import (scheme base)
          (scheme write)
          (scheme file)
          (scheme process-context))
  (export print mime-html declare-html
          (rename display print-raw) include load-static
          document-root http-cookie http-host http-referer
          http-user-agent https path query-string
          remote-addr remote-host remote-port remote-user
          request-method request-uri script-filename
          script-name server-port server-software)
  (include "./web.body.scm"))
