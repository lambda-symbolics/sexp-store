(defpackage #:sexp-store
  (:use #:cl)
  (:export #:log-append
           #:log-map
           #:log-read
           #:log-write
           #:publication-conflict
           #:snapshot-read
           #:snapshot-write
           #:store-error
           #:store-error-cause
           #:store-error-operation
           #:store-error-pathname))

(defpackage #:sexp-store/tests
  (:use #:cl)
  (:import-from #:sexp-store
                #:log-append
                #:log-map
                #:log-read
                #:log-write
                #:publication-conflict
                #:snapshot-read
                #:snapshot-write
                #:store-error)
  (:export #:run-tests))
