(in-package #:sexp-store/tests)

;;;; -- Locked State Transactions --

(defun tests--transaction-log (root &key (duplicate-keys ':reject))
  "Return a generic replacement/tombstone event store beneath ROOT."
  (make-instance
   'sexp-store:log-store
   :pathname (merge-pathnames "transaction-log.sexp" root)
   :lock-pathname (merge-pathnames "transaction-log.lock" root)
   :header '(:entries :version 1)
   :header-validator (lambda (form)
                       (sexp-store:record-check form :tag ':entries :versions '(1)))
   :validator
   (lambda (form)
     (and (member (first form) '(:put :delete))
          (sexp-store:record-check
           form :tag (first form) :versions '(1)
           :allow-duplicate-keys (eq duplicate-keys ':first)
           :fields (list (list :indicator ':id :validate #'stringp :required t)))))
   :duplicate-keys duplicate-keys
   :initial-state (lambda () (make-hash-table :test #'equal))
   :reducer
   (lambda (state record)
     (let ((id (sexp-store:record-property record ':id)))
       (ecase (first record)
         (:put
          (setf (gethash id state) (sexp-store:record-property record ':value)))
         (:delete
          (remhash id state))))
     state)))

(defun tests--transaction-snapshot (root)
  "Return a generic list-valued snapshot store beneath ROOT."
  (make-instance
   'sexp-store:snapshot-store
   :pathname (merge-pathnames "transaction-snapshot.sexp" root)
   :lock-pathname (merge-pathnames "transaction-snapshot.lock" root)
   :initial-state (constantly nil)
   :validator
   (lambda (form)
     (sexp-store:record-check
      form :tag ':entries :versions '(1)
      :fields (list (list :indicator ':items :required t :validate #'listp))))
   :decoder (lambda (form) (sexp-store:record-property form ':items))
   :encoder (lambda (state) (list :entries :version 1 :items state))))

(defun tests--transaction-events (store records &key publish)
  "Commit RECORDS to STORE and return the transaction's result."
  (sexp-store:store-transact
   store (lambda (state)
           (declare (ignore state))
           (values records ':committed t))
   :publish publish))

(defun tests--transaction-replay (root)
  "Exercise replay, batch publication, incomplete tails, and tombstones."
  (let* ((store (tests--transaction-log root))
         (pathname (merge-pathnames "transaction-log.sexp" root)))
    (test-assert (zerop (hash-table-count (sexp-store:store-read store)))
                 "missing logs fold from fresh empty state")
    (tests--transaction-events
     store '((:put :version 1 :id "a" :value 1)
             (:put :version 1 :id "b" :value 2)
             (:put :version 1 :id "a" :value 3)
             (:delete :version 1 :id "b")))
    (let ((state (sexp-store:store-read store)))
      (test-assert (and (= (hash-table-count state) 1) (= (gethash "a" state) 3))
                   "ordered folds apply replacement records and tombstones"))
    (tests--write-text pathname "(:put :version" :append t)
    (let ((bytes (uiop:read-file-string pathname)))
      (multiple-value-bind (state incomplete-p) (sexp-store:store-read store)
        (test-assert (and incomplete-p (= (gethash "a" state) 3)
                          (string= bytes (uiop:read-file-string pathname)))
                     "reads ignore incomplete tails without rewriting the log")))
    (let ((published nil))
      (test-assert
       (eq ':committed
           (tests--transaction-events
            store '((:put :version 1 :id "c" :value 4))
            :publish
            (lambda (state)
              (multiple-value-bind (forms incomplete-p) (log-read pathname)
                (test-assert (and (not incomplete-p) (= (length forms) 6)
                                  (= (gethash "c" state) 4))
                             "complete durable state precedes caller publication"))
              (setf published t))))
       "transactions return the updater's result")
      (test-assert published "successful transactions publish caller state"))
    (let ((before (uiop:read-file-string pathname))
          (published nil))
      (test-assert
       (signals store-error
         (tests--transaction-events
          store '((:put :version 1 :id "d" :value 5) (:unsupported :version 1))
          :publish (lambda (state) (declare (ignore state)) (setf published t))))
       "an invalid later event rejects the complete batch")
      (test-assert (and (not published)
                        (string= before (uiop:read-file-string pathname)))
                   "batch validation failure publishes neither disk nor caller state"))
    (let ((condition (make-condition 'simple-error :format-control "updater failed")))
      (test-assert
       (handler-case
           (sexp-store:store-transact store (lambda (state)
                                             (declare (ignore state))
                                             (error condition)))
         (simple-error (caught) (eq caught condition)))
       "updater conditions retain their identity"))
    (tests--transaction-events store '((:put :version 1 :id "after-error" :value 7)))
    (test-assert (= (gethash "after-error" (sexp-store:store-read store)) 7)
                 "failed callbacks release the transaction lock")))

(defun tests--transaction-invalid-records (root)
  "Reject complete malformed input before invoking field predicates or reducers."
  (let* ((store (tests--transaction-log root))
         (pathname (merge-pathnames "transaction-log.sexp" root)))
    (dolist (text '("" "(:entries :version" "(:wrong :version 1)"
                    "(:entries :version 1 :version 1)"
                    "#1=(:entries :version 1 . #1#)"
                    "(:entries :version 1) (:put :version 1 :id . 3)"
                    "(:entries :version 1) (:put :version 1 :id 42)"
                    "(:entries :version 1) (:put :version 1 :id \"a\" :id \"b\")"
                    "(:entries :version 1) (:put :version 1 :id \"a\" :value #1=(#1#))"
                    "(:entries :version 1) (:put :version 1 :id \"a\" :value #1=#(#1#))"
                    "(:entries :version 1) #.(error \"reader evaluation\")"))
      (tests--write-text pathname text)
      (test-assert (signals store-error (sexp-store:store-read store))
                   "malformed complete records and missing headers are rejected")
      (test-assert
       (signals store-error
         (tests--transaction-events store '((:put :version 1 :id "x" :value 1))))
       "transactions refuse to overwrite malformed complete history")
      (test-assert (string= text (uiop:read-file-string pathname))
                   "failed validation preserves corrupt evidence byte for byte"))
    (tests--write-text
     pathname "(:entries :version 1) (:put :version 1 :id \"a\" :value 1 :value 2)")
    (test-assert
     (= (gethash "a" (sexp-store:store-read
                       (tests--transaction-log root :duplicate-keys ':first))) 1)
     "explicit first-value duplicate policy agrees with record-check")
    (delete-file pathname))
  (let* ((shared (list :shared))
         (form (list :event :left shared :right shared)))
    (test-assert (sexp-store:record-shape-p form)
                 "finite records may share substructure"))
  (test-assert (sexp-store:record-finite-p (make-list 100000))
               "finite validation does not recurse with list depth"))

(defun tests--transaction-reducer-failure (root)
  "Require reducer failures to abort a whole batch without publishing state."
  (let* ((pathname (merge-pathnames "reducer-failure.sexp" root))
         (condition (make-condition 'simple-error :format-control "reducer failed"))
         (published nil)
         (store
           (make-instance
            'sexp-store:log-store
            :pathname pathname
            :lock-pathname (merge-pathnames "reducer-failure.lock" root)
            :header '(:counter :version 1)
            :header-validator (constantly t)
            :validator (constantly t)
            :initial-state (constantly 0)
            :reducer (lambda (state record)
                       (if (eq (first record) ':fail)
                           (error condition)
                           (1+ state))))))
    (test-assert
     (handler-case
         (tests--transaction-events
          store '((:increment :version 1) (:fail :version 1))
          :publish (lambda (state) (declare (ignore state)) (setf published t)))
       (simple-error (caught) (eq caught condition)))
     "reducer failures retain their condition identity")
    (test-assert (and (not published) (not (probe-file pathname)))
                 "a reducer failure publishes no prefix and no caller state")
    (tests--transaction-events store '((:increment :version 1)))
    (test-assert (= (sexp-store:store-read store) 1)
                 "a retry starts from authoritative state after reducer failure")))

(defun tests--transaction-snapshots (root)
  "Exercise fresh snapshots, rollback ordering, and exclusive initial creation."
  (let* ((store (tests--transaction-snapshot root))
         (pathname (merge-pathnames "transaction-snapshot.sexp" root))
         (visible nil))
    (flet ((add-item (item)
             (sexp-store:store-transact
              store (lambda (state) (values (append state (list item)) item t))
              :publish (lambda (state) (setf visible state)))))
      (test-assert (eq (add-item ':first) ':first)
                   "snapshot transactions return the updater result")
      (setf visible nil)
      (add-item ':second)
      (test-assert (equal visible '(:first :second))
                   "stale caller snapshots cannot discard another transaction")
      (let ((original (symbol-function 'sexp-store:log-write))
            (before (uiop:read-file-string pathname))
            (old-visible visible))
        (unwind-protect
             (progn
               (setf (symbol-function 'sexp-store:log-write)
                     (lambda (&rest arguments)
                       (declare (ignore arguments))
                      (error 'file-error :pathname pathname)))
               (test-assert (signals store-error (add-item ':failed))
                            "publication failures propagate"))
          (setf (symbol-function 'sexp-store:log-write) original))
        (test-assert (and (eq visible old-visible)
                          (string= before (uiop:read-file-string pathname)))
                     "write failures preserve durable and caller-visible state")))
    (delete-file pathname)
    (test-assert
     (signals publication-conflict
       (sexp-store:store-transact
        store (lambda (state)
                (declare (ignore state))
                (snapshot-write pathname '(:entries :version 1 :items (:racer)))
                (values '(:ours) nil t))))
     "exclusive first publication detects creation during the updater")
    (test-assert (equal (sexp-store:store-read store) '(:racer))
                 "exclusive creation preserves the competing writer's snapshot")
    (delete-file pathname)))

(defun tests--transaction-process-lock (store pathname)
  "Require a forked transaction to block and then read the latest state."
  (let ((pid nil)
        (reaped-p nil)
        (ready (make-pathname :type "ready" :defaults pathname)))
    (unwind-protect
         (progn
           (ls-flock:call-with-file-lock
            pathname
            (lambda ()
              (setf pid (sb-posix:fork))
              (when (zerop pid)
                (handler-case
                    (progn
                      (ls-flock:reset-after-fork)
                      (tests--write-text ready "ready")
                      (sexp-store:store-transact
                       store (lambda (state)
                               (values (append state '(:child)) nil t)))
                      (sb-posix:_exit 0))
                  (serious-condition () (sb-posix:_exit 1))))
              (loop repeat 500 until (probe-file ready) do (sleep 0.01))
              (test-assert (probe-file ready) "the forked transaction reaches the lock")
              (sleep 0.05)
              (multiple-value-bind (waited status) (sb-posix:waitpid pid sb-posix:wnohang)
                (declare (ignore status))
                (setf reaped-p (not (zerop waited)))
                (test-assert (not reaped-p) "transactions wait for the process-shared lock"))))
           (loop repeat 500
                 do (multiple-value-bind (waited status)
                        (sb-posix:waitpid pid sb-posix:wnohang)
                      (unless (zerop waited)
                        (setf reaped-p t)
                        (test-assert (and (sb-posix:wifexited status)
                                          (zerop (sb-posix:wexitstatus status)))
                                     "the forked transaction exits successfully")
                        (return)))
                    (sleep 0.01))
           (test-assert reaped-p "the forked transaction finishes before its deadline")
           (sexp-store:store-transact
            store (lambda (state) (values (append state '(:parent)) nil t)))
           (test-assert (equal (sexp-store:store-read store) '(:child :parent))
                        "serialized processes preserve both updates"))
      (when (and pid (plusp pid) (not reaped-p))
        (ignore-errors (sb-posix:kill pid sb-posix:sigkill))
        (ignore-errors (sb-posix:waitpid pid 0)))
      (when (probe-file ready) (delete-file ready)))))

(defun tests--transactions (root)
  "Run the generic log-fold and locked transaction checks beneath ROOT."
  (tests--transaction-replay root)
  (tests--transaction-invalid-records root)
  (tests--transaction-reducer-failure root)
  (tests--transaction-snapshots root)
  (tests--transaction-process-lock
   (tests--transaction-snapshot root)
   (merge-pathnames "transaction-snapshot.lock" root))
  nil)
