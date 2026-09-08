(in-package #:sexp-store/tests)

(defun tests--segment-header (pathname header)
  "Validate the fixture header and return its start and empty-segment policy."
  (unless (record-check header :tag ':log :versions '(1)
                        :fields (list (list :indicator ':start :required t
                                            :validate (lambda (value)
                                                        (typep value '(integer 1))))))
    (error 'store-error :pathname pathname :operation ':validate
                       :message "Invalid fixture header."))
  (values (record-property header ':start) (record-property header ':empty)))

(defun tests--segment-sequence (record)
  "Return the fixture record's sequence."
  (record-property record ':seq))

(defun tests--segments (root)
  "Exercise ordered segment boundaries, torn tails, publication and recovery."
  (let* ((first-path (merge-pathnames "first.log" root))
         (second-path (merge-pathnames "second.log" root))
         (options (list :header-function #'tests--segment-header
                        :record-sequence #'tests--segment-sequence)))
    (dolist (forms '(nil ((:log :version 1 :start 1))
                    ((:log :version 1 :start 1) (:event :seq 2))
                    ((:log :version 1 :start 1) (:event :seq 1) (:event :seq 1))
                    ((:log :version 1 :start 1) (:event :seq 1) (:event :seq 3))))
      (log-write first-path forms)
      (test-assert
       (signals sexp-store:segment-error
         (apply #'sexp-store:segment-map #'identity first-path options))
       "empty segments and sequence gaps or duplicates are rejected"))
    (log-write first-path '((:log :version 1 :start 1 :empty t)))
    (test-assert
     (zerop (nth-value 2 (apply #'sexp-store:segment-map #'identity first-path options)))
     "the header policy may explicitly allow a header-only segment")
    (log-write first-path '((:log :version 1 :start 1) (:event :seq 1)))
    (tests--write-text first-path "(:torn" :append t)
    (log-write second-path '((:log :version 1 :start 2) (:event :seq 2)))
    (let ((records nil))
      (multiple-value-bind (incomplete-p count next)
          (apply #'sexp-store:segments-map (lambda (record) (push record records))
                 (list first-path second-path) options)
        (test-assert (and (not incomplete-p) (= count 2) (= next 3)
                          (equal (nreverse records) '((:event :seq 1) (:event :seq 2))))
                     "ordered replay skips headers and retains complete sealed prefixes")))
    (tests--write-text second-path "(:torn" :append t)
    (test-assert
     (nth-value 0 (apply #'sexp-store:segments-map #'identity
                         (list first-path second-path) options))
     "only the active segment's torn tail is reported")
    (test-assert (sexp-store:log-repair-tail second-path)
                 "repair removes the incomplete final form")
    (test-assert (not (sexp-store:log-repair-tail second-path))
                 "a complete segment needs no repair")
    (log-write second-path '((:log :version 1 :start 3) (:event :seq 3)))
    (test-assert
     (signals sexp-store:segment-error
       (apply #'sexp-store:segments-map #'identity
              (list first-path second-path) options))
     "adjacent segments must be contiguous")
    (let ((condition (make-condition 'store-error :pathname first-path
                                    :operation ':callback :message "Callback failure.")))
      (test-assert
       (handler-case
           (apply #'sexp-store:segment-map (lambda (record)
                                            (declare (ignore record))
                                            (error condition))
                  first-path options)
         (store-error (caught) (eq caught condition)))
       "callback conditions retain their identity"))
    (tests--write-text first-path "))" :append t)
    (test-assert (signals store-error (sexp-store:log-repair-tail first-path))
                 "complete malformed tails are never discarded")
    (let ((path (merge-pathnames "publication.log" root))
          (header '(:log :version 1 :start 1))
          (record (list :event :seq 1 :payload (vector "Case" 1/3))))
      (sexp-store:segment-publish path header record)
      (test-assert (sexp-store:segment-published-p path header record)
                   "publication includes exactly the header and first record")
      (test-assert
       (not (sexp-store:segment-published-p
             path header (list :event :seq 1 :payload (vector "case" 1/3))))
       "publication checks compare array contents and string case exactly")
      (test-assert (signals publication-conflict
                     (sexp-store:segment-publish path header record))
                   "even an identical preexisting segment cannot be adopted")
      (tests--write-text path "(" :append t)
      (test-assert (not (sexp-store:segment-published-p path header record))
                   "torn suffixes fail publication readback")
      (delete-file path)
      (let ((original (symbol-function 'sexp-store:log-write)))
        (unwind-protect
             (progn
               (setf (symbol-function 'sexp-store:log-write)
                     (lambda (&rest arguments)
                       (apply original arguments)
                       (error 'store-error :pathname path :operation ':publish
                                          :message "Failure after rename.")))
               (sexp-store:segment-publish path header record)
               (test-assert (sexp-store:segment-published-p path header record)
                            "readback recovers a failure after durable publication"))
          (setf (symbol-function 'sexp-store:log-write) original)))
      (delete-file path)
      (let ((original (symbol-function 'sexp-store:log-write)))
        (unwind-protect
             (progn
               (setf (symbol-function 'sexp-store:log-write)
                     (lambda (&rest arguments)
                       (apply original arguments)
                       (error 'publication-conflict :pathname path :operation ':publish
                                                    :message "Another writer won.")))
               (test-assert (signals publication-conflict
                              (sexp-store:segment-publish path header record))
                            "known create-only conflicts never adopt another writer's data"))
          (setf (symbol-function 'sexp-store:log-write) original)))
      (delete-file path)
      (test-assert (signals publication-conflict
                     (sexp-store:segment-publish path header record
                                                :occupied-p (constantly t)))
                   "related occupied storage prevents publication")
      (test-assert (not (probe-file path)) "a rejected publication creates no segment")))
  nil)

(defun tests--sidecars (root)
  "Exercise revision validation, concurrent invalidation and derived snapshots."
  (let* ((path (merge-pathnames "derived.sexp" root))
         (revision-path (merge-pathnames "revision.sexp" root))
         (revision 1)
         (value (list :value :revision revision))
         (options (list :source-token (lambda () revision)
                        :value-token (lambda (value) (getf (rest value) :revision)))))
    (test-assert (zerop (sexp-store:revision-read revision-path :tag ':revision))
                 "missing revisions default to zero")
    (sexp-store:revision-write revision-path 1 :tag ':revision)
    (test-assert (= 1 (sexp-store:revision-read revision-path :tag ':revision))
                 "explicit revisions round-trip")
    (dolist (record '((:revision :version 2 :value 1)
                      (:revision :version 1 :value -1)
                      (:revision :version 1 :value 1 :value 2)))
      (snapshot-write revision-path record)
      (test-assert (zerop (sexp-store:revision-read revision-path :tag ':revision))
                   "unsupported or ambiguous revision records are cache misses"))
    (test-assert (apply #'sexp-store:sidecar-write path value :encode #'identity options)
                 "current derived state is published")
    (test-assert (equal value (apply #'sexp-store:sidecar-read path
                                    :decode #'identity options))
                 "a matching revision permits cache reuse")
    (incf revision)
    (test-assert (null (apply #'sexp-store:sidecar-read path :decode #'identity options))
                 "revision invalidation rejects a same-size source mutation")
    (test-assert (null (apply #'sexp-store:sidecar-write path value :encode #'identity options))
                 "a stale producer does not publish")
    (test-assert (equal (snapshot-read path) value)
                 "a rejected stale producer does not overwrite the snapshot")
    (setf revision 1)
    (test-assert
     (null (apply #'sexp-store:sidecar-read path :decode (lambda (record)
                                                         (incf revision) record)
                  options))
     "invalidation during decode rejects the result")
    (test-assert (null (apply #'sexp-store:sidecar-read path
                             :decode (lambda (record)
                                       (declare (ignore record))
                                       (error 'store-error :pathname path
                                                          :operation ':decode
                                                          :message "Invalid cache."))
                             options))
                 "cache decode failures permit reconstruction")
    (tests--write-text path "#.(error \"read evaluation\")")
    (test-assert (null (apply #'sexp-store:sidecar-read path :decode #'identity options))
                 "cache reads never evaluate forms"))
  nil)

(defun tests--finite-records ()
  "Reject non-finite record spines and define duplicate-property behavior."
  (dolist (body (list '(:version 1 :missing)
                     '(:version 1 . :dotted)
                     (let ((body (list :version 1)))
                       (setf (cddr body) body)
                       body)))
    (let ((form (cons :record body)))
      (test-assert (not (record-check form :tag ':record :versions '(1)))
                   "incomplete, dotted and circular properties are rejected")
      (test-assert (null (record-version form))
                   "accessors terminate on malformed record spines")))
  (test-assert (not (record-check '(:record :version 1 :version 2)
                                  :tag ':record :versions '(1 2)))
               "duplicate properties are rejected by default")
  (test-assert (record-check '(:record :version 1 :version 2)
                             :tag ':record :versions '(1) :allow-duplicate-keys t)
               "explicit duplicate compatibility follows first-value GETF semantics")
  nil)

#+sbcl
(defun tests--exclusive-publication (root)
  "Race create-only publishers and reject even a dangling target symlink."
  (let* ((pathname (merge-pathnames "exclusive.sexp" root))
         (gate (sb-thread:make-semaphore :count 0))
         (threads nil))
    (unwind-protect
         (progn
           (setf threads
                 (loop for value in '(1 2)
                       collect
                       (let ((record (list :writer value)))
                         (sb-thread:make-thread
                          (lambda ()
                            (sb-thread:wait-on-semaphore gate)
                            (handler-case
                                (progn (snapshot-write pathname record :require-absent t)
                                       :published)
                              (publication-conflict () :conflict)))))))
           (sb-thread:signal-semaphore gate 2)
           (let ((results (mapcar #'sb-thread:join-thread threads)))
             (test-assert (and (= (count :published results) 1)
                               (= (count :conflict results) 1))
                          "exactly one racing create-only writer can publish"))
           (delete-file pathname)
           (sb-posix:symlink "missing-target" (namestring pathname))
           (test-assert (signals publication-conflict
                          (snapshot-write pathname '(:replacement t) :require-absent t))
                        "a dangling target symlink is an occupied directory entry"))
      (dolist (thread threads)
        (when (sb-thread:thread-alive-p thread)
          (sb-thread:terminate-thread thread)
          (ignore-errors (sb-thread:join-thread thread))))))
  nil)
