(in-package #:sexp-store)

;;;; -- Revisioned Derived Snapshots --

(defun file-revision (pathname)
  "Return PATHNAME's namestring, byte size and write date as three values.

Combine these attributes with an explicit revision and the source-lock protocol
of REVISION-WRITE and SIDECAR-REBUILD. Size and date alone cannot distinguish
same-size edits within one clock tick."
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
A failed publication must prevent that source mutation. Hold that same lock
across every cache reconstruction and publication, using SIDECAR-REBUILD or an
explicit surrounding lock. Revision comparisons alone cannot distinguish old
source data observed after invalidation from the writer's completed new state."
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
lookup. Writers and cache builders must follow REVISION-WRITE's shared-lock
protocol. The lookup itself needs no lock under that protocol."
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
propagate; callers decide whether cache publication is best effort. The caller
must hold the source writer lock from before constructing VALUE through this
call, or supply an immutable committed source snapshot with its matching token.
Locking only this publication cannot validate an unsynchronized reconstruction."
  (let ((token (funcall value-token value)))
    (when (equal token (funcall source-token))
      (snapshot-write pathname (funcall encode value))
      (when (equal token (funcall source-token))
        value))))

(defun sidecar-rebuild (pathname &key lock-pathname build encode source-token value-token)
  "Reconstruct and publish a sidecar while excluding authoritative source writes.

LOCK-PATHNAME must name the lock used by every source writer, including revision
invalidation. BUILD runs with that lock held and returns a freshly reconstructed
value with its source token, or NIL to decline publication. ENCODE, SOURCE-TOKEN
and VALUE-TOKEN follow SIDECAR-WRITE. Return the published value or NIL. Lock,
construction and publication errors propagate. Do not already hold this lock."
  (unless lock-pathname
    (store--fail ':validate pathname "A sidecar rebuild requires the source lock pathname."))
  (ls-flock:call-with-file-lock
   lock-pathname
   (lambda ()
     (let ((value (funcall build)))
       (when value
         (sidecar-write pathname value :encode encode
                                       :source-token source-token
                                       :value-token value-token))))))
