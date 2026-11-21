(in-package #:sexp-store)

;;;; -- Revisioned Derived Snapshots --

(defun file-revision (pathname)
  "Return PATHNAME's namestring, byte size and write date as three values.

Combine these attributes with an explicit revision and the source-lock protocol
of REVISION-WRITE and SIDECAR-REBUILD. Size and date alone cannot distinguish
same-size edits within one clock tick."
  (with-open-file (stream pathname :direction   ':input
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
         (sidecar-write pathname value :encode       encode
                                       :source-token source-token
                                       :value-token  value-token))))))


;;;; -- Sparse Log Indexes --

(defun log-index--metadata-p (metadata stride)
  "Check generic manifest fields before indexing or seeking."
  (and (plist-schema-p metadata :keyword-keys-p t
                                :required-keys '(:epoch :stride :count :bytes :tail-position))
       (stringp (getf metadata :epoch))
       (plusp (length (getf metadata :epoch)))
       (typep stride '(integer 1))
       (eql (getf metadata :stride) stride)
       (typep (getf metadata :count) '(integer 0))
       (typep (getf metadata :bytes) '(integer 1))
       (typep (getf metadata :tail-position) '(integer 0))
       (< (getf metadata :tail-position) (getf metadata :bytes))))

(defun log-index-read (source manifest &key stride header-epoch validate)
  "Read a current sparse index manifest, or NIL on an absent or invalid sidecar.
HEADER-EPOCH derives the source epoch from its first physical form. VALIDATE,
if supplied, receives metadata, header and tail (the header for an empty log).
The source must end with the declared tail. Caller holds the source writer lock."
  (handler-case
      (multiple-value-bind (metadata complete-p) (snapshot-read manifest)
        (when (and complete-p (log-index--metadata-p metadata stride)
                   (= (getf metadata :bytes) (nth-value 1 (file-revision source))))
          (multiple-value-bind (header header-end header-status) (log-first source :physical-forms-p t)
            (let ((position (getf metadata :tail-position)))
              (when (and (eq header-status ':record)
                         (equal (getf metadata :epoch) (funcall header-epoch header))
                         (or (zerop position) (>= position header-end)))
                (multiple-value-bind (tail end status)
                    (log-first source :start-position position :physical-forms-p t)
                  (multiple-value-bind (extra eof-position eof-status)
                      (log-first source :start-position end :physical-forms-p t)
                    (declare (ignore extra))
                    (when (and (eq status ':record) (eq eof-status ':eof)
                               (= eof-position (getf metadata :bytes))
                               (or (null validate) (funcall validate metadata header tail)))
                      metadata))))))))
    (error () nil)))

(defun log-index--read-leaf (pathname metadata base count)
  "Return a leaf's validated offsets, or signal STORE-ERROR."
  (multiple-value-bind (leaf complete-p) (snapshot-read pathname)
    (let ((positions (and complete-p
                          (plist-schema-p leaf :allowed-keys '(:epoch :index :positions)
                                               :required-keys '(:epoch :index :positions))
                          (equal (getf leaf :epoch) (getf metadata :epoch))
                          (eql (getf leaf :index) base)
                          (getf leaf :positions))))
      (unless (and positions (listp positions)
                   (eql (ignore-errors (list-length positions)) count)
                   (loop with previous = -1
                         for position in positions
                         always (and (typep position '(integer 0))
                                     (< previous position (getf metadata :bytes))
                                     (<= position (getf metadata :tail-position))
                                     (setf previous position))))
        (store--fail ':read pathname "The log index leaf is malformed or stale."))
      positions)))

(defun log-index-leaf (pathname metadata block)
  "Return the validated offsets in BLOCK of the current METADATA manifest."
  (let ((stride (getf metadata :stride)))
    (unless (and (log-index--metadata-p metadata stride) (typep block '(integer 0))
                 (< (* block stride) (getf metadata :count)))
      (store--fail ':read pathname "The log index block is outside the manifest."))
    (let ((base (* block stride)))
      (log-index--read-leaf pathname metadata base (min stride (- (getf metadata :count) base))))))

(defun log-index--write-leaf (pathname metadata base positions)
  "Publish one fixed-size leaf before its referencing manifest."
  (snapshot-write pathname (list :epoch (getf metadata :epoch) :index base :positions positions)))

(defun log-index-rebuild (source manifest &key stride header-epoch leaf-pathname select metadata)
  "Explicitly rebuild a sparse index with at most one STRIDE-sized leaf in memory.
HEADER-EPOCH receives the first physical form; SELECT receives each later physical
form and says whether to index it. SELECT may update domain fields in METADATA,
which must be a writable plist and may not use the reserved fields :EPOCH,
:STRIDE, :COUNT, :BYTES or :TAIL-POSITION. LEAF-PATHNAME maps a block number to
its sidecar. Return the published metadata. The caller holds the writer lock
and must not change the selection policy without changing the source epoch."
  (unless (typep stride '(integer 1))
    (store--fail ':validate manifest "An index stride must be a positive integer."))
  (multiple-value-bind (header end status) (log-first source :physical-forms-p t)
    (let ((epoch (and (eq status ':record) (funcall header-epoch header)))
          (base 0)
          (count 0)
          (tail 0)
          (positions nil))
      (unless (and (stringp epoch) (plusp (length epoch)))
        (store--fail ':read source "The indexed log has no source epoch."))
      (let ((fields (list :epoch epoch :stride stride :count 0 :bytes 0 :tail-position 0)))
        (labels ((flush-leaf ()
                   (when positions
                     (log-index--write-leaf (funcall leaf-pathname (floor base stride)) fields
                                            base (nreverse positions))
                     (setf base count positions nil))))
          (multiple-value-bind (bytes incomplete-p)
              (log-map (lambda (form start end)
                         (declare (ignore end))
                         (setf tail start)
                         (when (funcall select form)
                           (push start positions)
                           (incf count)
                           (when (= (- count base) stride) (flush-leaf))))
                       source :start-position end :with-positions-p t :physical-forms-p t)
            (when incomplete-p
              (store--fail ':read source "The indexed log ends with an incomplete form."))
            (flush-leaf)
            (setf (getf fields :count) count (getf fields :bytes) bytes
                  (getf fields :tail-position) tail)
            (let ((result (append fields metadata)))
              (snapshot-write manifest result)
              result)))))))

(defun log-index-note (source manifest metadata &key position selected-p leaf-pathname)
  "Publish metadata after a durable append at POSITION, updating one leaf if selected.
METADATA is the previous manifest with updated domain fields. Return a new
manifest. The caller holds the source writer lock across append and publication."
  (let* ((stride (getf metadata :stride))
         (bytes  (nth-value 1 (file-revision source))))
    (unless (and (log-index--metadata-p metadata stride)
                 (typep position '(integer 0))
                 (<= (getf metadata :bytes) position)
                 (< position bytes))
      (store--fail ':validate manifest "The log index needs repair before appending."))
    (let ((result (copy-list metadata)))
      (when selected-p
        (let* ((index (getf result :count))
               (block (floor index stride))
               (base (* block stride))
               (path (funcall leaf-pathname block))
               (positions (unless (= index base)
                            (log-index--read-leaf path result base (- index base)))))
          (log-index--write-leaf path result base (append positions (list position)))
          (incf (getf result :count))))
      (setf (getf result :bytes) bytes (getf result :tail-position) position)
      (snapshot-write manifest result)
      result)))
