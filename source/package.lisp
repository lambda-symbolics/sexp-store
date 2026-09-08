(defpackage #:sexp-store
  (:use #:cl)
  (:export #:log-append
           #:log-map
           #:log-read
           #:log-write
           #:log-repair-tail
           #:segment-error
           #:segment-error-sequence
           #:segment-map
           #:segments-map
           #:segment-published-p
           #:segment-publish
           #:file-revision
           #:revision-read
           #:revision-write
           #:sidecar-read
           #:sidecar-write
           #:make-record
           #:publication-conflict
           #:record-check
           #:record-property
           #:record-property-present-p
           #:record-version
           #:record-finite-p
           #:record-shape-p
           #:transactional-store
           #:log-store
           #:snapshot-store
           #:store-read
           #:store-transact
           #:snapshot-read
           #:snapshot-read-record
           #:snapshot-write
           #:store-error
           #:store-error-message
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
