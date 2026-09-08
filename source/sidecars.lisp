(in-package #:sexp-store)

;;;; -- Revisioned Derived Snapshots --

(defun file-revision (pathname)
  "Return PATHNAME's namestring, byte size and write date as three values.

These attributes are suitable for derived caches when combined with an explicit
revision incremented before every source mutation; size and date alone are not
sufficient to detect same-size edits within one clock tick."
  (with-open-file (stream pathname :direction ':input
                                  :element-type '(unsigned-byte 8))
    (values (namestring pathname) (file-length stream)
            (or (file-write-date pathname) 0))))

(defun revision-read (pathname &key tag (version 1))
  "Read a nonnegative revision from a sole TAG record, or return zero.

Absent, malformed, or unsupported revision records are treated as cache misses.
The caller owns TAG and VERSION, as well as the mutation's writer exclusion."
  (handler-case
      (let ((record (snapshot-read-record
                     pathname :tag tag :versions (list version)
                     :fields (list (list :indicator ':value :required t
                                         :validate (lambda (value)
                                                     (typep value '(integer 0))))))))
        (record-property record ':value))
    (error ()
      0)))

(defun revision-write (pathname revision &key tag (version 1))
  "Atomically publish REVISION as a TAG record and return it.

Under the source writer lock, publish this revision before changing source data.
A failed publication must prevent that source mutation."
  (unless (typep revision '(integer 0))
    (store--fail ':validate pathname "A revision must be a nonnegative integer."))
  (snapshot-write pathname (make-record tag version ':value revision))
  revision)

(defun sidecar-read (pathname &key decode source-token value-token validate)
  "Read and decode a current derived snapshot, returning NIL on a cache miss.

DECODE converts the sole readable form into a value, returning NIL if invalid.
SOURCE-TOKEN returns the source's current revision token; VALUE-TOKEN extracts
that token from a decoded value. Tokens are compared with EQUAL before and after
reading. Optional VALIDATE receives the decoded value and rejects extra domain
constraints by returning NIL. No reader or callback failure escapes this cache
lookup. A writer must advance the source revision before mutating the source."
  (handler-case
      (let ((before (funcall source-token)))
        (multiple-value-bind (record complete-p) (snapshot-read pathname)
          (let ((value (and complete-p (funcall decode record))))
            (when (and value
                       (equal before (funcall value-token value))
                       (or (null validate) (funcall validate value))
                       (equal before (funcall source-token)))
              value))))
    (error ()
      nil)))

(defun sidecar-write (pathname value &key encode source-token value-token)
  "Atomically publish a derived VALUE only if its source token is current.

ENCODE returns the readable form. SOURCE-TOKEN and VALUE-TOKEN have the contracts
of SIDECAR-READ. Return VALUE on publication or NIL if it became stale. Errors
propagate; callers decide whether cache publication is best effort. A stale
snapshot observed during a concurrent mutation is rejected by SIDECAR-READ."
  (let ((token (funcall value-token value)))
    (when (equal token (funcall source-token))
      (snapshot-write pathname (funcall encode value))
      (when (equal token (funcall source-token))
        value))))
