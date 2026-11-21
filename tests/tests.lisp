(in-package #:sexp-store/tests)

(defun tests--write-text (pathname text &key append)
  "Write raw fixture TEXT to PATHNAME, optionally appending it."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction         :output
                          :if-exists         (if append :append :supersede)
                          :if-does-not-exist :create
                          :external-format   :utf-8)
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
    (let ((pathname  (merge-pathnames "positions.sexp" root))
          (positions nil))
      (log-write pathname '((:event :text "žluťoučký λ") (:event :text "次")))
      (log-map (lambda (form start end) (push (list form start end) positions))
               pathname :with-positions-p t)
      (dolist (entry positions)
        (destructuring-bind (expected start end) entry
          (test-assert (< start end) "record positions span the complete UTF-8 form")
          (test-assert
           (equal expected
                  (block first-record
                    (log-map (lambda (form) (return-from first-record form))
                             pathname :start-position start)))
           "every reported position can seek directly to its record"))))
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
  (let ((pathname (merge-pathnames "streamed.sexp" root)))
    (log-write-streaming pathname
                         (lambda (emit)
                           (funcall emit '(:events :version 1))
                           (dotimes (index 3)
                             (funcall emit (list :event :id index)))))
    (test-assert (equal (log-read pathname)
                        '((:events :version 1) (:event :id 0) (:event :id 1) (:event :id 2)))
                 "a streamed log publishes every emitted form in order")
    (test-assert (signals simple-error
                   (log-write-streaming pathname
                                        (lambda (emit)
                                          (funcall emit '(:events :version 1))
                                          (error "producer failure"))))
                 "a producer's own failure propagates unchanged")
    (test-assert (= 4 (length (log-read pathname)))
                 "a failed producer publishes nothing")
    (test-assert (signals store-error
                   (log-write-streaming pathname
                                        (lambda (emit)
                                          (funcall emit (function car)))))
                 "an unreadable form signals STORE-ERROR")
    (test-assert (= 4 (length (log-read pathname)))
                 "an unreadable form publishes nothing"))
  (let ((pathname          (merge-pathnames "read-eval.sexp" root))
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
  (tests--log-batches root)
  nil)

(defun tests--log-batches (root)
  "Read transaction batches as their records, all of them or none."
  (let ((pathname (merge-pathnames "batches.sexp" root))
        (batch    "(:sexp-store-batch :version 1 :records ((:event :id 2) (:event :id 3)))"))
    (tests--write-text pathname (format nil "(:events :version 1)~%(:event :id 1)~%~A~%" batch))
    (multiple-value-bind (forms incomplete-p) (log-read pathname)
      (test-assert (and (not incomplete-p)
                        (equal forms '((:events :version 1) (:event :id 1)
                                       (:event :id 2) (:event :id 3))))
                   "a batch reads as its records in order"))
    (test-assert (= 4 (nth-value 2 (log-map (lambda (form) (declare (ignore form))) pathname)))
                 "the visit count counts a batch's records")
    (loop for cut from 1 below (length batch)
          do (tests--write-text pathname (format nil "(:events :version 1)~%(:event :id 1)~%~A"
                                                 (subseq batch 0 cut)))
             (multiple-value-bind (forms incomplete-p) (log-read pathname)
               (unless (and incomplete-p
                            (equal forms '((:events :version 1) (:event :id 1))))
                 (test-assert nil (format nil "a batch cut at ~D shows none of its records" cut)))))
    (test-assert t "a batch cut short anywhere shows none of its records")
    (dolist (text '("(:sexp-store-batch :version 2 :records ((:event :id 1)))"
                    "(:sexp-store-batch :version 1)"
                    "(:sexp-store-batch :version 1 :records (:event . 1))"
                    "(:sexp-store-batch :version 1 :records ((:event :id 1)) :extra t)"))
      (tests--write-text pathname text)
      (test-assert (signals store-error (log-read pathname))
                   (format nil "a malformed batch is an error: ~A" text))))
  nil)

(defun tests--record-fields ()
  "Return the field descriptions shared by the record tests."
  (list (list :indicator ':name
              :validate  (lambda (value)
                           (and (stringp value) (plusp (length value))))
              :required  t)
        (list :indicator ':retries
              :validate  #'integerp
              :required  '(2))
        (list :indicator ':note
              :validate  #'stringp)))

(defun tests--records (root)
  "Exercise versioned record validation beneath ROOT."
  (let ((fields (tests--record-fields)))
    (flet ((check (form &key (allow-other-keys t))
             (record-check form
                           :tag              ':job
                           :versions         '(1 2)
                           :fields           fields
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
                                :tag      ':job
                                :versions '(1 2)
                                :fields   fields)
        (test-assert (and (= version 2)
                          (equal (record-property form ':name) "sync"))
                     "snapshot records round-trip with their version"))
      (snapshot-write pathname (make-record ':job 3 ':name "sync"))
      (test-assert (signals store-error
                     (snapshot-read-record pathname
                                           :tag      ':job
                                           :versions '(1 2)
                                           :fields   fields))
                   "malformed snapshot records signal STORE-ERROR")))
  nil)

(defun tests--property-records ()
  "Exercise unversioned property schemas and their structural limits."
  (let ((fields (list (list :indicator ':name :validate #'stringp :required t))))
    (dolist (case
              (list (list '(:name "sync") t)
                    (list '(:name nil) nil)
                    (list nil nil)
                    (list 17 nil)
                    (list '(:name . "sync") nil)
                    (list '(:name "sync" :note) nil)
                    (list '(:name "sync" :name "duplicate") nil)
                    (list '(:name "sync" :name "duplicate") t :allow-duplicate-keys t)
                    (list '(:name "sync" :extra t) t)
                    (list '(:name "sync" :extra t) nil :allow-other-keys nil)
                    (list '(:name "sync" :version "incidental") t)
                    (list '(:name "sync" :version "incidental") nil :allow-other-keys nil)
                    (list '(:name "sync") nil :versions nil)
                    (list '(:name "sync" :version 1) t :versions '(1) :allow-other-keys nil)
                    (list '(:name "sync" other t) t)
                    (list '(:name "sync" other t) nil :keyword-keys-p t)
                    (list '(:name "sync") t :maximum-length 2)
                    (list '(:name "sync" :extra t) nil :maximum-length 2)))
      (multiple-value-bind (valid-p reason)
          (apply #'record-check (first case)
                 :properties-p t :fields fields (nthcdr 2 case))
        (test-assert (and (eq valid-p (second case))
                          (if valid-p (null reason) (stringp reason)))
                     "property records enforce the requested schema")))
    (test-assert (record-check '(:job :name "sync") :tag ':job :fields fields)
                 "tagged records may omit version semantics")
    (let ((circular (list :name "sync")))
      (setf (rest (last circular)) circular)
      (test-assert (not (record-check circular :properties-p t :fields fields))
                   "circular property records are rejected")))
  (test-assert (record-check nil :properties-p t :maximum-length 0)
               "an empty property schema accepts an empty body")
  nil)

(defun tests--plist-schemas ()
  "Exercise structured property-list diagnostics and bounded spine validation."
  (dolist (case '(((:name "x") () nil nil)
                  ((:unknown) (:allowed-keys (:name)) :odd nil)
                  ((:name . "x") () :improper nil)
                  ((name "x") () :non-keyword name)
                  ((:other "x") (:allowed-keys (:name)) :unknown :other)
                  ((:name "x" :name "y") () :duplicate :name)
                  ((:name "x" :name "y") (:allow-duplicate-keys t) nil nil)
                  ((:name nil) (:required-keys (:name)) nil nil)
                  (() (:required-keys (:name)) :missing :name)
                  ((:name "x") (:allowed-keys nil) :unknown :name)
                  ((:name "x") (:maximum-length 2) nil nil)
                  ((:name "x") (:maximum-length 1) :too-long nil)))
    (destructuring-bind (properties options expected-problem expected-key) case
      (multiple-value-bind (problem key)
          (apply #'sexp-store:plist-schema-problem properties options)
        (test-assert (and (eq problem expected-problem) (eq key expected-key))
                     "property schemas return structural or field diagnostics"))))
  (let ((cycle (list :name "x")))
    (setf (rest (last cycle)) cycle)
    (test-assert (eq (sexp-store:plist-schema-problem cycle) :improper)
                 "schema validation terminates on cyclic property lists"))
  (test-assert
   (eq (sexp-store:plist-schema-problem (make-list 100000) :maximum-length 2) :too-long)
   "bounded schemas stop before traversing oversized property spines")
  (test-assert (sexp-store:plist-schema-p nil :allowed-keys nil :maximum-length 0)
               "an empty schema accepts an empty property body")
  nil)

(defun tests--bounded-snapshot-records (root)
  "Exercise storage and schema validation with an explicit bounded data grammar."
  (let* ((path    (merge-pathnames "bounded-record.sexp" root))
         (grammar (sexp-config:make-source-grammar
                   :label "bounded snapshot" :maximum-depth 4 :maximum-nodes 32
                   :maximum-string-characters 8)))
    (snapshot-write path '(:name "sync"))
    (test-assert
     (equal (snapshot-read-record path :grammar grammar :maximum-octets 1024
                                      :properties-p t :keyword-keys-p t
                                      :maximum-length 2
                                      :fields         '((:indicator :name :required t :validate stringp)))
            '(:name "sync"))
     "bounded snapshot records support unversioned property lists")
    (test-assert
     (signals store-error
       (snapshot-read-record path :grammar grammar :properties-p t :versions nil))
     "an explicit empty version set accepts no version")
    (tests--write-text path "(:name \"sync\") nil")
    (test-assert (signals store-error (snapshot-read path :grammar grammar))
                 "bounded snapshots require exactly one form")
    (tests--write-text path "(:name \"too long for this grammar\")")
    (test-assert (signals store-error (snapshot-read path :grammar grammar))
                 "bounded reader errors cross the storage boundary as STORE-ERROR")
    (tests--write-text path "(:name \"sync\")")
    (test-assert (signals store-error (snapshot-read path :grammar grammar :maximum-octets 4))
                 "bounded snapshots forward the file octet limit")
    (snapshot-write path '(:job :name "sync"))
    (test-assert (equal (snapshot-read-record path :tag ':job) '(:job :name "sync"))
                 "tagged snapshot records support omitted version semantics"))
  nil)

(defun tests--guarded-publication (root)
  "Build outside a guard and commit or abandon inside the publication scope."
  (let ((pathname (merge-pathnames "guarded.sexp" root)))
    (log-write pathname '((:old)))
    (log-write-streaming
     pathname (lambda (emit) (funcall emit '(:new)))
     :call-with-publication
     (lambda (publish)
       (test-assert (equal (log-read pathname) '((:old))) "the producer has not replaced the old log")
       (funcall publish)
       (test-assert (equal (log-read pathname) '((:new))) "the guard covers actual publication")))
    (block cancelled
      (log-write-streaming pathname (lambda (emit) (funcall emit '(:cancelled)))
                           :call-with-publication (lambda (publish)
                                                    (declare (ignore publish))
                                                    (return-from cancelled))))
    (test-assert (equal (log-read pathname) '((:new))) "abandoned publication leaves the complete previous log"))
  nil)

(defun tests--log-transform (root)
  "Exercise physical batches and atomic publication of transformed forms."
  (let* ((source (merge-pathnames "transform.log" root))
         (target (merge-pathnames "transformed.log" root))
         (shared (vector "shared"))
         (batch (sexp-store::store--batch-form (list (list :old shared shared) '(:second))))
         (prepared nil))
    (log-write source (list batch))
    (tests--write-text source "(:torn" :append t)
    (test-assert (not (sexp-store:log-transform source source #'identity
                                                :prepare-function (lambda (path)
                                                                    (declare (ignore path))
                                                                    (error "Unexpected publication"))))
                 "an unchanged same-path transform does not publish")
    (test-assert (nth-value 1 (log-read source)) "no-op leaves the source bytes untouched")
    (test-assert (not (sexp-store:log-transform source target #'identity))
                 "an unchanged distinct target is published")
    (let ((record (first (getf (rest (sexp-store:log-first target :physical-forms-p t)) :records))))
      (test-assert (and (vectorp (second record)) (eq (second record) (third record)))
                   "physical transaction and shared array structure survive"))
    (test-assert
     (sexp-store:log-transform
      target target
      (lambda (form)
        (setf (first (first (getf (rest form) :records))) ':new)
        (values form t))
      :prepare-function (lambda (temporary)
                          (setf prepared (and (eq (first (sexp-store:log-first temporary)) ':new)
                                              (eq (first (sexp-store:log-first target)) ':old)))))
     "changed transform reports the rewrite")
    (test-assert prepared "preparation sees the completed staged log before replacement")
    (tests--write-text source " ))" :append t)
    (test-assert (signals store-error (sexp-store:log-transform source target #'identity))
                 "malformed source is rejected")
    (test-assert (eq (first (sexp-store:log-first target)) ':new)
                 "failed transform preserves the old target")))

(defun tests--log-index (root)
  "Exercise paged offsets, freshness and publication ordering."
  (let ((source (merge-pathnames "indexed.log" root))
        (manifest (merge-pathnames "index.sexp" root)))
    (labels ((leaf (block) (merge-pathnames (format nil "index-~D.sexp" block) root))
             (epoch (header) (getf (rest header) :epoch))
             (lookup () (sexp-store:log-index-read source manifest :stride 2 :header-epoch #'epoch))
             (rebuild (&optional (leaf-pathname #'leaf) (target manifest))
               (sexp-store:log-index-rebuild source target :stride 2 :header-epoch #'epoch
                                             :leaf-pathname leaf-pathname
                                             :select (lambda (form) (eq (first form) ':entry)))))
      (log-write source '((:log :epoch "a") (:entry 1) (:skip) (:entry 2) (:entry 3)))
      (let* ((metadata (rebuild))
             (offsets (sexp-store:log-index-leaf (leaf 0) metadata 0))
             (position (getf metadata :bytes)))
        (test-assert (equal metadata (lookup)) "a valid manifest round trips")
        (test-assert (equal '((:entry 1) (:entry 2))
                            (mapcar (lambda (offset) (sexp-store:log-first source :start-position offset)) offsets))
                     "each leaf offset seeks directly to its selected form")
        (dolist (bad (list (list :epoch "old" :index 0 :positions offsets)
                           '(:epoch "a" :index 0 :positions (4 . 5))
                           '(:epoch "a" :index 0 :positions (4 4))))
          (snapshot-write (leaf 0) bad)
          (test-assert (signals store-error (sexp-store:log-index-leaf (leaf 0) metadata 0))
                       "wrong epoch and malformed offsets cannot supply a page"))
        (rebuild)
        (log-append source '(:entry 4) :repair-tail-p nil)
        (test-assert (null (lookup)) "an appended source invalidates the old manifest")
        (test-assert (signals error (rebuild (lambda (block)
                                               (declare (ignore block)) (error "Leaf failure"))))
                     "a leaf publication failure escapes rebuild")
        (test-assert (equal (snapshot-read manifest) metadata)
                     "failed leaf publication does not publish a manifest")
        (test-assert (signals error (rebuild #'leaf root)) "manifest publication can fail after leaves")
        (test-assert (null (lookup)) "a stale manifest cannot make newly written leaves visible")
        (snapshot-write (leaf 1) (list :epoch "a" :index 2 :positions (list (getf metadata :tail-position))))
        (let ((updated (sexp-store:log-index-note source manifest metadata :position position
                                                  :selected-p t :leaf-pathname #'leaf)))
          (test-assert (and (= 4 (getf updated :count)) (equal updated (lookup)))
                       "append extends one leaf then publishes the new manifest"))
        (log-write source '((:log :epoch "b") (:entry 1) (:skip) (:entry 2) (:entry 3) (:entry 4)))
        (test-assert (null (lookup)) "same-size epoch replacement is stale")
        (log-write source '((:log :epoch "a") (:entry 1)))
        (test-assert (null (lookup)) "a truncated log invalidates the manifest")
        (tests--write-text source "(:entry" :append t)
        (test-assert (signals store-error (rebuild)) "a torn log cannot publish a fresh manifest")))))
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
           (tests--raw-snapshots root)
           (tests--logs root)
           (tests--guarded-publication root)
           (tests--log-transform root)
           (tests--log-index root)
           (tests--records root)
           (tests--property-records)
           (tests--plist-schemas)
           (tests--bounded-snapshot-records root)
           (tests--transactions root)
           (tests--segments root)
           #+sbcl (tests--exclusive-publication root)
           (tests--sidecars root)
           #+sbcl (tests--sidecar-rebuild root)
           (tests--finite-records)
           (tests--files root))
      (uiop:delete-directory-tree root
                                  :validate          t
                                  :if-does-not-exist :ignore)))
  (format t "~&~:D sexp-store tests passed.~%" *test-count*)
  nil)
