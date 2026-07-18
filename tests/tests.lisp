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
  "Return PATHNAME's Unix permission bits on SBCL."
  #+sbcl (logand #o777 (sb-posix:stat-mode (sb-posix:stat pathname)))
  #-sbcl (declare (ignore pathname))
  #-sbcl nil)

(defun tests--snapshots (root)
  "Exercise atomic single-form snapshots beneath ROOT."
  (let ((pathname (merge-pathnames "snapshot.sexp" root)))
    (snapshot-write pathname '(:state :version 1))
    (multiple-value-bind (form sole-form-p)
        (snapshot-read pathname)
      (test-assert (and sole-form-p
                        (equal form '(:state :version 1)))
                   "a snapshot round-trips as one form"))
    #+sbcl
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
    (tests--write-text pathname "(:event :id" :append t)
    (multiple-value-bind (forms incomplete-final-form-p)
        (log-read pathname)
      (test-assert
       (and incomplete-final-form-p (= (length forms) 3))
       "an incomplete final form preserves complete preceding data"))
    (log-append pathname '(:event :id 3))
    (multiple-value-bind (forms incomplete-final-form-p)
        (log-read pathname)
      (test-assert
       (and (not incomplete-final-form-p)
            (equal (first (last forms)) '(:event :id 3))
            (= (length forms) 4))
       "the next append atomically repairs an incomplete tail"))
    (tests--write-text pathname ")\n" :append t)
    (test-assert (signals store-error (log-read pathname))
                 "malformed complete reader input signals STORE-ERROR"))
  (let ((pathname (merge-pathnames "read-eval.sexp" root))
        (*read-eval-ran-p* nil))
    (declare (special *read-eval-ran-p*))
    (tests--write-text pathname
                       "#.(progn (setf *read-eval-ran-p* t) :unsafe)\n")
    (test-assert (and (signals store-error (log-read pathname))
                      (not *read-eval-ran-p*))
                 "log reads disable reader evaluation"))
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
           (tests--logs root))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist :ignore)))
  (format t "~&~:D sexp-store tests passed.~%" *test-count*)
  nil)
