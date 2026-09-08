(in-package #:sexp-store)

;;;; -- Validated Transactions --

(defclass transactional-store ()
  ((pathname
    :initarg :pathname
    :reader transaction-store-pathname
    :documentation "The authoritative readable state pathname.")
   (lock-pathname
    :initarg :lock-pathname
    :reader transaction-store-lock-pathname
    :documentation "The stable adjacent advisory lock pathname.")
   (initial-state
    :initarg :initial-state
    :reader transaction-store-initial-state
    :documentation "A zero-argument function returning fresh empty state.")
   (validator
    :initarg :validator
    :reader transaction-store-validator
    :documentation "A predicate validating one complete data form.")
   (duplicate-keys
    :initarg :duplicate-keys
    :initform ':reject
    :reader transaction-store-duplicate-keys
    :documentation "Either :REJECT repeated record keys or use their :FIRST value."))
  (:documentation "Paths and application callbacks for locked readable state."))

(defclass log-store (transactional-store)
  ((header
    :initarg :header
    :reader log-store-header
    :documentation "The complete header used when creating a log.")
   (header-validator
    :initarg :header-validator
    :reader log-store-header-validator
    :documentation "A predicate accepting supported complete headers.")
   (reducer
    :initarg :reducer
    :reader log-store-reducer
    :documentation "A function of fresh state and record returning next state.")
   (finalizer
    :initarg :finalizer
    :initform #'identity
    :reader log-store-finalizer
    :documentation "A function converting folded state to the public state value."))
  (:documentation "A header followed by validated events folded into fresh state."))

(defclass snapshot-store (transactional-store)
  ((decoder
    :initarg :decoder
    :reader snapshot-store-decoder
    :documentation "A function decoding a validated snapshot into fresh state.")
   (encoder
    :initarg :encoder
    :reader snapshot-store-encoder
    :documentation "A function encoding replacement state as one readable record."))
  (:documentation "Exactly one validated record, replaced as a single publication."))

(defgeneric store--read (store)
  (:documentation "Return fresh state, complete source forms, and tail status."))

(defgeneric store--publish (store &key state change forms)
  (:documentation "Validate and persist CHANGE, returning the published state."))

(defun record-finite-p (form)
  "Return true for acyclic readable data, allowing shared substructure.

Inspect conses and arrays without recursive calls or printing malformed input.
Opaque objects are not portable record data. Symbols, numbers, characters,
strings, and pathnames are leaves."
  (let ((colors (make-hash-table :test #'eq))
        (pending (list (cons form nil))))
    (loop while pending
          for entry = (pop pending)
          for value = (first entry)
          do (cond
               ((rest entry)
                (setf (gethash value colors) ':done))
               ((or (consp value) (and (arrayp value) (not (stringp value))))
                (case (gethash value colors)
                  (:visiting
                   (return-from record-finite-p nil))
                  (:done)
                  (otherwise
                   (setf (gethash value colors) ':visiting)
                   (push (cons value t) pending)
                   (if (consp value)
                       (progn
                         (push (cons (rest value) nil) pending)
                         (push (cons (first value) nil) pending))
                       (dotimes (index (array-total-size value))
                         (push (cons (row-major-aref value index) nil) pending))))))
               ((not (or (symbolp value) (numberp value) (characterp value)
                         (stringp value) (pathnamep value)))
                (return-from record-finite-p nil))))
    t))

(defun record-shape-p (form &key (duplicate-keys ':reject))
  "Return true for a finite keyword-tagged property record.

DUPLICATE-KEYS is :REJECT or :FIRST. This structural check precedes RECORD-CHECK
and application field predicates, so neither receives circular or dotted data."
  (and (member duplicate-keys '(:reject :first))
       (record-finite-p form)
       (consp form)
       (keywordp (first form))
       (record--property-list-p (rest form))
       (loop for (key value) on (rest form) by #'cddr always (keywordp key))
       (record-check form :tag (first form) :versions (list (record-version form))
                          :allow-duplicate-keys (eq duplicate-keys ':first))
       t))

(defun store--validate-record (store form validator)
  "Validate FORM structurally before invoking the domain VALIDATOR."
  (unless (record-shape-p
           form :duplicate-keys (transaction-store-duplicate-keys store))
    (store--fail ':validate (transaction-store-pathname store)
                 "A record is not a finite keyword property list, or repeats a key."))
  (unless (funcall validator form)
    (store--fail ':validate (transaction-store-pathname store)
                 "A complete record is malformed or unsupported."))
  form)

(defmethod store--read ((store log-store))
  (let ((state (funcall (transaction-store-initial-state store)))
        (forms nil)
        (header-p t))
    (multiple-value-bind (position incomplete-p count)
        (log-map
         (lambda (form)
           (store--validate-record
            store form (if header-p
                           (log-store-header-validator store)
                           (transaction-store-validator store)))
           (if header-p
               (setf header-p nil)
               (setf state (funcall (log-store-reducer store) state form)))
           (push form forms))
         (transaction-store-pathname store))
      (declare (ignore position count))
      (when (and header-p (probe-file (transaction-store-pathname store)))
        (store--fail ':validate (transaction-store-pathname store)
                     "The log has no complete header."))
      (values state (nreverse forms) incomplete-p))))

(defmethod store--read ((store snapshot-store))
  (if (probe-file (transaction-store-pathname store))
      (multiple-value-bind (form sole-form-p)
          (snapshot-read (transaction-store-pathname store))
        (unless sole-form-p
          (store--fail ':validate (transaction-store-pathname store)
                       "The snapshot must hold exactly one complete form."))
        (store--validate-record store form (transaction-store-validator store))
        (values (funcall (snapshot-store-decoder store) form) (list form) nil))
      (values (funcall (transaction-store-initial-state store)) nil nil)))

(defmethod store--publish ((store log-store) &key state change forms)
  (unless (and (record-finite-p change)
               (handler-case (integerp (list-length change))
                 (type-error () nil)))
    (store--fail ':validate (transaction-store-pathname store)
                 "A log transaction must return a finite proper list of records."))
  (dolist (record change)
    (store--validate-record store record (transaction-store-validator store))
    (setf state (funcall (log-store-reducer store) state record)))
  (let ((require-absent (null forms)))
    (unless forms
      (store--validate-record store (log-store-header store)
                              (log-store-header-validator store))
      (setf forms (list (log-store-header store))))
    ;; One publication covers the whole batch and removes only an incomplete tail.
    (store--write-transaction store (append forms change)
                              :require-absent require-absent))
  state)

(defmethod store--publish ((store snapshot-store) &key state change forms)
  (declare (ignore state))
  (let ((form (funcall (snapshot-store-encoder store) change)))
    (store--validate-record store form (transaction-store-validator store))
    (store--write-transaction store (list form) :require-absent (null forms)))
  change)

(defun store--write-transaction (store forms &key require-absent)
  "Publish FORMS, preserving typed storage failures at the transaction boundary."
  (handler-case
      (log-write (transaction-store-pathname store) forms
                 :require-absent require-absent)
    (store-error (cause)
      (error cause))
    (error (cause)
      (store--fail ':publish (transaction-store-pathname store)
                   "Could not publish the state transaction." cause))))

(defgeneric store--finalize (store state)
  (:documentation "Return the application-facing value of fresh STATE."))

(defmethod store--finalize ((store log-store) state)
  (funcall (log-store-finalizer store) state))

(defmethod store--finalize ((store snapshot-store) state)
  state)

(defun store--call-with-lock (store function)
  "Call FUNCTION under the process-local and process-shared pathname lock."
  (handler-case
      (ls-flock:call-with-file-lock
       (transaction-store-lock-pathname store) function)
    (ls-flock:file-lock-error (cause)
      (store--fail ':lock (transaction-store-lock-pathname store)
                   "Could not acquire the state transaction lock." cause))))

(defun store-read (store &key lock-held-p)
  "Return validated fresh state and whether a log has an incomplete final form.

Read under STORE's advisory lock. LOCK-HELD-P is true only when the caller already
holds that same lock as part of a larger coordinated operation. Missing files
produce INITIAL-STATE; existing empty files and malformed complete forms signal
STORE-ERROR. Incomplete log tails are ignored without modifying any bytes.
Callback errors propagate."
  (flet ((read-state ()
           (multiple-value-bind (state forms incomplete-p) (store--read store)
             (declare (ignore forms))
             (values (store--finalize store state) incomplete-p))))
    (if lock-held-p
        (read-state)
        (store--call-with-lock store #'read-state))))

(defun store-transact (store update &key publish)
  "Read, modify, validate, and publish state while holding STORE's lock.

UPDATE receives freshly reconstructed state and returns three values: CHANGE,
RESULT, and WRITE-P. For LOG-STORE, CHANGE is a proper list of new records; for
SNAPSHOT-STORE it is replacement state. When WRITE-P is false nothing is written.
When true the entire change is validated before one atomic publication, including
repair of an incomplete log tail. PUBLISH, when supplied, receives committed
state only after successful persistence, including for a read-only transaction.
Return RESULT and committed state. Callbacks must not expose or install their
private working state before PUBLISH. Finalizers and UPDATE may modify their
working values; reconstruct the publication base independently under the same
lock. Callback errors propagate unchanged; a PUBLISH failure occurs after commit
and does not roll back durable state."
  (store--call-with-lock
   store
   (lambda ()
     (multiple-value-bind (state forms incomplete-p) (store--read store)
       (declare (ignore incomplete-p))
       (multiple-value-bind (change result write-p)
           (funcall update (store-read store :lock-held-p t))
         (when write-p
           (setf state (store--publish store :state state :change change :forms forms)))
         (let ((committed (store--finalize store state)))
           (when publish
             (funcall publish committed))
           (values result committed)))))))
