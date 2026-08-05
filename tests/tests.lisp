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
