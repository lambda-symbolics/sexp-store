(in-package #:sexp-store)

;;;; -- Validated Transactions --

(defclass transactional-store ()
  ((pathname
    :initarg       :pathname
    :reader        transaction-store-pathname
    :documentation "The authoritative readable state pathname.")
   (lock-pathname
    :initarg       :lock-pathname
    :reader        transaction-store-lock-pathname
    :documentation "The stable adjacent advisory lock pathname.")
   (initial-state
    :initarg       :initial-state
    :reader        transaction-store-initial-state
    :documentation "A zero-argument function returning fresh empty state.")
   (validator
    :initarg       :validator
    :reader        transaction-store-validator
    :documentation "A predicate validating one complete data form.")
   (duplicate-keys
    :initarg       :duplicate-keys
    :initform      ':reject
    :reader        transaction-store-duplicate-keys
    :documentation "Either :REJECT repeated record keys or use their :FIRST value."))
  (:documentation "Paths and application callbacks for locked readable state."))

(defclass log-store (transactional-store)
  ((header
    :initarg       :header
    :reader        log-store-header
    :documentation "The complete header used when creating a log.")
   (header-validator
    :initarg       :header-validator
    :reader        log-store-header-validator
    :documentation "A predicate accepting supported complete headers.")
   (reducer
    :initarg       :reducer
    :reader        log-store-reducer
    :documentation "A function of fresh state and record returning next state.")
   (finalizer
    :initarg       :finalizer
    :initform      #'identity
    :reader        log-store-finalizer
    :documentation "A function converting folded state to the public state value.")
   (compactor
    :initarg       :compactor
    :initform      nil
    :reader        log-store-compactor
    :documentation "An optional function of folded state returning the fewest records that fold to it, used by STORE-COMPACT."))
  (:documentation "A header followed by validated events folded into fresh state."))

(defclass snapshot-store (transactional-store)
  ((decoder
    :initarg       :decoder
    :reader        snapshot-store-decoder
    :documentation "A function decoding a validated snapshot into fresh state.")
   (encoder
    :initarg       :encoder
    :reader        snapshot-store-encoder
    :documentation "A function encoding replacement state as one readable record.")
   (recover-read-error
    :initarg       :recover-read-error
    :initform      nil
    :reader        snapshot-store-recover-read-error
    :documentation "An optional function of STORE-ERROR returning fresh replacement state; NIL propagates read failures."))
  (:documentation "Exactly one validated record, replaced as a single publication."))

(defgeneric store--read (store)
  (:documentation "Return fresh state, source forms, tail status, and whether an unreadable snapshot is being replaced."))

(defgeneric store--publish (store &key state change forms incomplete-p replace-existing-p)
  (:documentation "Validate and persist CHANGE, returning the published state."))

(defgeneric store--working-state (store forms)
  (:documentation "Return fresh public state for an updater, independent of the publication base read as FORMS."))

(defun record-finite-p (form)
  "Return true for acyclic readable data, allowing shared substructure.

Inspect conses and arrays without recursive calls or printing malformed input.
Opaque objects are not portable record data. Symbols, numbers, characters,
strings, and pathnames are leaves."
  (let ((colors  (make-hash-table :test #'eq))
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
       (loop for key in (rest form) by #'plist--next always (keywordp key))
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
  "Fold the validated header and records of STORE's log into fresh state."
  (let ((state    (funcall (transaction-store-initial-state store)))
        (forms    nil)
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
  "Decode STORE's validated snapshot, or recover from a read failure when STORE allows it."
  (if (probe-file (transaction-store-pathname store))
      (multiple-value-bind (value recovered-p)
          (handler-case
              (multiple-value-bind (form sole-form-p)
                  (snapshot-read (transaction-store-pathname store))
                (unless sole-form-p
                  (store--fail ':validate (transaction-store-pathname store)
                               "The snapshot must hold exactly one complete form."))
                (store--validate-record store form (transaction-store-validator store))
                (values form nil))
            (store-error (condition)
              (let ((recover (snapshot-store-recover-read-error store)))
                (if recover
                    (values (funcall recover condition) t)
                    (error condition)))))
        (if recovered-p
            (values value nil nil t)
            (values (funcall (snapshot-store-decoder store) value) (list value) nil nil)))
      (values (funcall (transaction-store-initial-state store)) nil nil nil)))

(defmethod store--publish ((store log-store) &key state change forms incomplete-p replace-existing-p)
  "Publish the CHANGE records after FORMS, and return STATE with CHANGE folded in.

Empty FORMS start a new log with STORE's header. A complete log grows by CHANGE
alone: one record is appended as itself, several as one batch form, so a crash
leaves all of them or none. Only a log with an incomplete final form is
rewritten, as FORMS followed by CHANGE."
  (declare (ignore replace-existing-p))
  (dolist (record (store--validate-change store change))
    (setf state (funcall (log-store-reducer store) state record)))
  (cond
    ((null forms)
     (store--validate-record store (log-store-header store)
                             (log-store-header-validator store))
     (store--write-transaction store (cons (log-store-header store) change)
                               :require-absent t))
    (incomplete-p
     ;; Repair: one atomic rewrite removes the incomplete tail and adds CHANGE.
     (store--write-transaction store (append forms change)))
    (t
     (store--append-change store change)))
  state)

(defun store--append-change (store change)
  "Append CHANGE to STORE's complete log: one record as itself, several as one batch form."
  (cond
    ((rest change)
     (store--write-transaction store (list (store--batch-form change)) :append t))
    (change
     (store--write-transaction store change :append t)))
  nil)

(defun store--validate-change (store change)
  "Signal STORE-ERROR unless CHANGE is a finite proper list of valid records for log STORE."
  (unless (and (record-finite-p change)
               (handler-case (integerp (list-length change))
                 (type-error () nil)))
    (store--fail ':validate (transaction-store-pathname store)
                 "A log transaction must return a finite proper list of records."))
  (dolist (record change)
    (store--validate-record store record (transaction-store-validator store)))
  change)

(defmethod store--publish ((store snapshot-store) &key state change forms incomplete-p replace-existing-p)
  "Replace STORE's snapshot with the encoded CHANGE and return CHANGE."
  (declare (ignore state incomplete-p))
  (let ((form (funcall (snapshot-store-encoder store) change)))
    (store--validate-record store form (transaction-store-validator store))
    (store--write-transaction store (list form)
                              :require-absent (not (or forms replace-existing-p))))
  change)

(defun store--write-transaction (store forms &key require-absent append)
  "Publish FORMS, preserving typed storage failures at the transaction boundary.

With APPEND, FORMS extend the existing file; otherwise they replace it atomically."
  (handler-case
      (if append
          (store--write-forms (transaction-store-pathname store) forms :append t)
          (log-write (transaction-store-pathname store) forms
                     :require-absent require-absent))
    (store-error (cause)
      (error cause))
    (error (cause)
      (store--fail ':publish (transaction-store-pathname store)
                   "Could not publish the state transaction." :cause cause))))

(defmethod store--working-state ((store log-store) forms)
  "Fold a copy of FORMS, so the updater may change its state and records freely."
  (let ((state (funcall (transaction-store-initial-state store))))
    (dolist (record (rest (store--copy-form forms)))
      (setf state (funcall (log-store-reducer store) state record)))
    (store--finalize store state)))

(defmethod store--working-state ((store snapshot-store) forms)
  "Read STORE again, so a recovery callback returns a fresh value for the updater."
  (declare (ignore forms))
  (values (store-read store :lock-held-p t)))

(defun store--copy-form (form)
  "Return a copy of readable FORM with fresh conses, strings and arrays.

Shared substructure stays shared within the copy. FORM is finite, as validated
records are; list spines are copied without recursion, so long lists are safe."
  (let ((copies (make-hash-table :test #'eq)))
    (labels ((copy (value)
               (cond
                 ((not (or (consp value) (arrayp value)))
                  value)
                 ((gethash value copies))
                 ((consp value)
                  (copy-spine value))
                 (t
                  (copy-array value))))

             (copy-array (array)
               (let ((copy (make-array (array-dimensions array)
                                       :element-type (array-element-type array))))
                 (setf (gethash array copies) copy)
                 (dotimes (index (array-total-size array))
                   (setf (row-major-aref copy index) (copy (row-major-aref array index))))
                 copy))

             (copy-spine (list)
               (let* ((head    (cons nil nil))
                      (current head)
                      (source  list))
                 (setf (gethash source copies) head)
                 (loop
                   (setf (first current) (copy (first source)))
                   (let ((next (rest source)))
                     (cond
                       ((not (consp next))
                        (setf (rest current) (copy next))
                        (return head))
                       ((gethash next copies)
                        (setf (rest current) (gethash next copies))
                        (return head))
                       (t
                        (let ((cell (cons nil nil)))
                          (setf (gethash next copies) cell
                                (rest current)        cell
                                current               cell
                                source                next)))))))))
      (copy form))))

(defgeneric store--finalize (store state)
  (:documentation "Return the application-facing value of fresh STATE."))

(defmethod store--finalize ((store log-store) state)
  "Convert folded STATE with STORE's finalizer."
  (funcall (log-store-finalizer store) state))

(defmethod store--finalize ((store snapshot-store) state)
  "Return STATE unchanged."
  state)

(defun store--call-with-lock (store function)
  "Call FUNCTION under the process-local and process-shared pathname lock."
  (handler-case
      (ls-flock:call-with-file-lock
       (transaction-store-lock-pathname store) function)
    (ls-flock:file-lock-error (cause)
      (store--fail ':lock (transaction-store-lock-pathname store)
                   "Could not acquire the state transaction lock." :cause cause))))

(defun store-read (store &key lock-held-p)
  "Return validated fresh state and whether a log has an incomplete final form.

Read under STORE's advisory lock. LOCK-HELD-P is true only when the caller already
holds that same lock as part of a larger coordinated operation. Missing files
produce INITIAL-STATE; existing empty files and malformed complete forms signal
STORE-ERROR unless SNAPSHOT-STORE's explicit RECOVER-READ-ERROR callback supplies
fresh replacement state. Recovery alone does not rewrite the file. Incomplete
log tails are ignored without modifying bytes. Callback errors propagate."
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
When true the entire change is validated before it is published. A log grows by
the change alone, appended as one form, so after a crash all of it or none of it
is durable; a log with an incomplete final form is rewritten once instead,
which repairs it. The file is read once: the updater's state is folded from a
copy of what was read, so finalizers and UPDATE may modify their working values
without touching the publication base. PUBLISH, when supplied, receives committed
state only after successful persistence, including for a read-only transaction.
Return RESULT and committed state. Callbacks must not expose or install their
private working state before PUBLISH. Callback errors propagate unchanged; a
PUBLISH failure occurs after commit and does not roll back durable state."
  (store--call-with-lock
   store
   (lambda ()
     (store--transact-locked store update publish))))

(defun store--transact-locked (store update publish)
  "Run STORE-TRANSACT's read, update and publication while the caller holds STORE's lock."
  ;; One read: the updater's state is folded from a copy of the forms read.
  (multiple-value-bind (state forms incomplete-p replace-existing-p) (store--read store)
    (multiple-value-bind (change result write-p)
        (funcall update (store--working-state store forms))
      (when write-p
        (setf state (store--publish store :state state :change change :forms forms
                                    :incomplete-p       incomplete-p
                                    :replace-existing-p replace-existing-p)))
      (let ((committed (store--finalize store state)))
        (when publish
          (funcall publish committed))
        (values result committed)))))

(defun store-compact (store)
  "Rewrite LOG-STORE STORE's log as its header and compacted records, under its lock.

With a COMPACTOR, the records are those it returns for the folded state;
without one, the complete records as read, batches flattened and an incomplete
final form dropped. The records are validated and folded before one atomic
replacement, so a failure leaves the log unchanged. A missing log stays missing.
Return the number of records written and the committed public state."
  (store--call-with-lock
   store
   (lambda ()
     (multiple-value-bind (state forms) (store--read store)
       (unless forms
         (return-from store-compact (values 0 (store--finalize store state))))
       (let ((records (if (log-store-compactor store)
                          (funcall (log-store-compactor store) state)
                          (rest forms)))
             (folded  (funcall (transaction-store-initial-state store))))
         (unless (and (record-finite-p records)
                      (handler-case (integerp (list-length records))
                        (type-error () nil)))
           (store--fail ':validate (transaction-store-pathname store)
                        "A compactor must return a finite proper list of records."))
         (dolist (record records)
           (store--validate-record store record (transaction-store-validator store))
           (setf folded (funcall (log-store-reducer store) folded record)))
         (store--write-transaction store (cons (first forms) records))
         (values (length records) (store--finalize store folded)))))))
