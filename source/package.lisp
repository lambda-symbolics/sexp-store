(defpackage #:sexp-store
  (:use #:cl)
  (:export #:log-append
           #:log-map
           #:log-read
           #:log-write
           #:make-record
           #:publication-conflict
           #:record-check
           #:record-property
           #:record-property-present-p
           #:record-version
           #:snapshot-read
           #:snapshot-read-record
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
                #:make-record
                #:publication-conflict
                #:record-check
                #:record-property
                #:record-property-present-p
                #:record-version
                #:snapshot-read
                #:snapshot-read-record
                #:snapshot-write
                #:store-error)
  (:export #:run-tests))
