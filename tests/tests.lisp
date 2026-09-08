(in-package #:sexp-store/tests)

(defun tests--write-text (pathname text &key append)
  "Write raw fixture TEXT to PATHNAME, optionally appending it."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists (if append :append :supersede)
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string text stream)
    (finish-output stream))
  nil)

(defun tests--file-mode (pathname)
  "Return PATHNAME's Unix permission bits."
  (logand #o777 (ls-compat.posix:file-mode pathname)))

(defun tests--snapshots (root)
  "Exercise atomic single-form snapshots beneath ROOT."
  (let ((pathname (merge-pathnames "snapshot.sexp" root)))
    (snapshot-write pathname '(:state :version 1))
    (multiple-value-bind (form sole-form-p)
        (snapshot-read pathname)
      (test-assert (and sole-form-p
                        (equal form '(:state :version 1)))
                   "a snapshot round-trips as one form"))
    (test-assert (= (tests--file-mode pathname) #o600)
                 "snapshot files default to private permissions")
    (tests--write-text pathname "(:extra t)\n" :append t)
    (multiple-value-bind (form sole-form-p)
        (snapshot-read pathname)
      (test-assert (and (equal form '(:state :version 1))
                        (not sole-form-p))
                   "snapshot reads report trailing complete forms"))
    (test-assert
     (signals publication-conflict
       (snapshot-write pathname '(:replacement t) :require-absent t))
     "require-absent publication rejects an occupied target"))
  nil)

(defun tests--logs (root)
  "Exercise append logs, incomplete tails, and reader safety beneath ROOT."
  (let ((pathname (merge-pathnames "events.sexp" root)))
    (multiple-value-bind (forms incomplete-final-form-p)
        (log-read pathname)
      (test-assert (and (null forms) (null incomplete-final-form-p))
                   "a missing log reads as empty"))
    (log-append pathname '(:event :id 1)
                :initial-forms '((:events :version 1)))
    (log-append pathname '(:event :id 2))
    (multiple-value-bind (forms incomplete-final-form-p)
        (log-read pathname)
      (test-assert
       (and (not incomplete-final-form-p)
            (equal forms
                   '((:events :version 1)
                     (:event :id 1)
                     (:event :id 2))))
       "complete forms append in order"))
    (let ((visited nil))
      (multiple-value-bind (position incomplete-final-form-p count)
          (log-map (lambda (form)
                     (push form visited))
                   pathname)
        (test-assert
         (and (not incomplete-final-form-p)
              (= count 3)
              (equal (nreverse visited)
                     '((:events :version 1)
                       (:event :id 1)
                       (:event :id 2))))
         "streaming reads visit complete forms in order")
        (log-append pathname '(:event :id 7) :repair-tail-p nil)
        (let ((tail nil))
          (multiple-value-bind (next-position tail-incomplete-p tail-count)
              (log-map (lambda (form)
                         (push form tail))
                       pathname
                       :start-position position)
            (test-assert
             (and (not tail-incomplete-p)
                  (= tail-count 1)
                  (> next-position position)
                  (equal tail '((:event :id 7))))
             "a returned position reads only subsequently appended forms")))))
    (tests--write-text pathname "(:event :id" :append t)
    (multiple-value-bind (forms incomplete-final-form-p)
        (log-read pathname)
      (test-assert
       (and incomplete-final-form-p (= (length forms) 4))
       "an incomplete final form preserves complete preceding data"))
    (log-append pathname '(:event :id 3))
    (multiple-value-bind (forms incomplete-final-form-p)
        (log-read pathname)
      (test-assert
       (and (not incomplete-final-form-p)
            (equal (first (last forms)) '(:event :id 3))
            (= (length forms) 5))
       "the next append atomically repairs an incomplete tail"))
    (tests--write-text pathname ")\n" :append t)
    (test-assert (signals store-error (log-read pathname))
                 "malformed complete reader input signals STORE-ERROR"))
  (let ((pathname (merge-pathnames "read-eval.sexp" root))
        (*read-eval-ran-p* nil))
    (declare (special *read-eval-ran-p*))
    (tests--write-text pathname
                       "#.(progn (setf *read-eval-ran-p* t) :unsafe)\n")
    (test-assert (and (signals store-error
                        (log-map (lambda (form)
                                   (declare (ignore form)))
                                 pathname))
                      (not *read-eval-ran-p*))
                 "streaming log reads disable reader evaluation"))
  (let ((pathname (merge-pathnames "callback-error.sexp" root)))
    (log-append pathname '(:event :id 1))
    (test-assert
     (signals simple-error
       (log-map (lambda (form)
                  (declare (ignore form))
                  (error "callback failure"))
                pathname))
     "streaming reads propagate callback conditions unchanged"))
  (let* ((pathname  (merge-pathnames "callback-end-of-file.sexp" root))
         (stream    (make-string-input-stream ""))
         (condition (make-condition 'end-of-file :stream stream)))
    (unwind-protect
         (progn
           (log-append pathname '(:event :id 1))
           (test-assert
            (handler-case
                (progn
                  (log-map (lambda (form)
                             (declare (ignore form))
                             (error condition))
                           pathname)
                  nil)
              (end-of-file (cause)
                (eq cause condition)))
            "streaming reads do not mistake callback EOF for an incomplete tail"))
      (close stream)))
  nil)

(defun tests--record-fields ()
  "Return the field descriptions shared by the record tests."
  (list (list :indicator ':name
              :validate (lambda (value)
                          (and (stringp value) (plusp (length value))))
              :required t)
        (list :indicator ':retries
              :validate #'integerp
              :required '(2))
        (list :indicator ':note
              :validate #'stringp)))

(defun tests--records (root)
  "Exercise versioned record validation beneath ROOT."
  (let ((fields (tests--record-fields)))
    (flet ((check (form &key (allow-other-keys t))
             (record-check form
                           :tag ':job
                           :versions '(1 2)
                           :fields fields
                           :allow-other-keys allow-other-keys)))
      (test-assert (equal (make-record ':job 2 ':name "sync")
                          '(:job :version 2 :name "sync"))
                   "records are built as tagged property lists")
      (test-assert (check (make-record ':job 1 ':name "sync"))
                   "a record with its required properties is valid")
      (test-assert (check (make-record ':job 2 ':name "sync" ':retries 3))
                   "a record may add properties required by its version")
      (test-assert (not (check '(:task :version 1 :name "sync")))
                   "records with a foreign tag are rejected")
      (test-assert (not (check '(:job :version 1 :name)))
                   "records with an incomplete property list are rejected")
      (test-assert (not (check (make-record ':job 3 ':name "sync")))
                   "records with an unsupported version are rejected")
      (test-assert (not (check (make-record ':job 2 ':name "sync")))
                   "records missing a version's required property are rejected")
      (test-assert (not (check (make-record ':job 1 ':name "")))
                   "records with an unsupported property value are rejected")
      (test-assert (not (check (make-record ':job 1 ':name "sync" ':extra t)
                               :allow-other-keys nil))
                   "strict records reject undescribed properties")
      (multiple-value-bind (valid-p reason)
          (check (make-record ':job 2 ':name "sync"))
        (test-assert (and (not valid-p)
                          (search ":RETRIES" reason))
                     "invalid records report the failing property"))
      (test-assert (and (eql (record-version '(:job :version 2)) 2)
                        (null (record-version '(:job :version)))
                        (equal (record-property
                                (make-record ':job 1 ':name "sync")
                                ':name)
                               "sync")
                        (eql (record-property '(:job :version 1) ':name 7) 7)
                        (record-property-present-p
                         (make-record ':job 1 ':name nil)
                         ':name)
                        (not (record-property-present-p
                              (make-record ':job 1)
                              ':name)))
                   "record accessors read versions and properties safely"))
    (let ((pathname (merge-pathnames "record.sexp" root)))
      (snapshot-write pathname (make-record ':job 2 ':name "sync"
                                            ':retries 3))
      (multiple-value-bind (form version)
          (snapshot-read-record pathname
                                :tag ':job
                                :versions '(1 2)
                                :fields fields)
        (test-assert (and (= version 2)
                          (equal (record-property form ':name) "sync"))
                     "snapshot records round-trip with their version"))
      (snapshot-write pathname (make-record ':job 3 ':name "sync"))
      (test-assert (signals store-error
                     (snapshot-read-record pathname
                                           :tag ':job
                                           :versions '(1 2)
                                           :fields fields))
                   "malformed snapshot records signal STORE-ERROR")))
  nil)

(defun run-tests ()
  "Run every sexp-store regression test."
  (setf *test-count* 0)
  (let ((root
          (uiop:ensure-directory-pathname
           (merge-pathnames
            (format nil "sexp-store-tests-~D-~D/"
                    (get-universal-time)
                    (random most-positive-fixnum))
            (uiop:temporary-directory)))))
    (unwind-protect
         (progn
           (tests--snapshots root)
           (tests--logs root)
           (tests--records root)
           (tests--transactions root)
           (tests--segments root)
           #+sbcl (tests--exclusive-publication root)
           (tests--sidecars root)
           #+sbcl (tests--sidecar-rebuild root)
           (tests--finite-records))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist :ignore)))
  (format t "~&~:D sexp-store tests passed.~%" *test-count*)
  nil)
