(in-package #:sexp-store)

;;;; -- Conditions --

(define-condition store-error (error)
  ((message
    :initarg :message
    :reader store-error-message
    :type string
    :documentation "The human-readable description of the failure.")
   (operation
    :initarg :operation
    :reader store-error-operation
    :type keyword
    :documentation "The storage operation that failed.")
   (pathname
    :initarg :pathname
    :reader store-error-pathname
    :type pathname
    :documentation "The state file involved in the failure.")
   (cause
    :initarg :cause
    :initform nil
    :reader store-error-cause
    :type t
    :documentation "The underlying condition, when available."))
  (:report
   (lambda (condition stream)
     (format stream "~A (~A)"
             (store-error-message condition)
             (store-error-pathname condition))))
  (:documentation "A readable state file could not be read or published."))

(define-condition publication-conflict (store-error)
  ()
  (:documentation
   "A target required to be absent appeared during atomic publication."))

(defun store--fail (operation pathname message &optional cause)
  "Signal a STORE-ERROR for OPERATION on PATHNAME."
  (error 'store-error
         :message message
         :operation operation
         :pathname pathname
         :cause cause))


;;;; -- Reading --

(defun snapshot-read (pathname)
  "Read PATHNAME with evaluation disabled.

Return the first form and true only when it is the file's sole complete form.
Reader errors are wrapped in STORE-ERROR."
  (handler-case
      (with-open-file (stream pathname
                              :direction :input
                              :external-format :utf-8)
        (let* ((*read-eval* nil)
               (end-marker (cons nil nil))
               (form (read stream nil end-marker))
               (extra (read stream nil end-marker)))
          (values form
                  (and (not (eq form end-marker))
                       (eq extra end-marker)))))
    (store-error (condition)
      (error condition))
    (error (cause)
      (store--fail ':read pathname
                   (format nil "Could not read a state snapshot: ~A" cause)
                   cause))))

(defun log-map (function pathname &key (start-position 0))
  "Call FUNCTION for each complete form in PATHNAME from START-POSITION.

Evaluation is disabled while forms are read. Return the position after the last
complete form, whether reading stopped inside an incomplete final form, and the
number of forms visited. The returned position may be supplied to a later call
after forms have only been appended. Callback conditions propagate unchanged.
A missing file is empty only at position zero."
  (unless (functionp function)
    (store--fail ':read pathname "FUNCTION must be a function."))
  (unless (typep start-position '(integer 0))
    (store--fail ':read pathname
                 "START-POSITION must be a nonnegative integer."))
  (unless (probe-file pathname)
    (if (zerop start-position)
        (return-from log-map (values 0 nil 0))
        (store--fail ':read pathname
                     "A nonzero log position cannot be read from a missing file.")))
  (let ((stream
          (handler-case
              (open pathname
                    :direction :input
                    :external-format :utf-8)
            (error (cause)
              (store--fail ':read pathname
                           (format nil "Could not open a readable log: ~A" cause)
                           cause)))))
    (unwind-protect
         (let ((*read-eval* nil)
               (end-marker (cons nil nil))
               (count 0))
           (handler-case
               (unless (file-position stream start-position)
                 (store--fail ':read pathname
                              "START-POSITION is not valid for this log."))
             (store-error (condition)
               (error condition))
             (error (cause)
               (store--fail ':read pathname
                            (format nil "Could not seek in a readable log: ~A"
                                    cause)
                            cause)))
           (loop
             for form-start =
               (handler-case
                   (file-position stream)
                 (error (cause)
                   (store--fail
                    ':read pathname
                    (format nil "Could not locate a readable log form: ~A"
                            cause)
                    cause)))
             do
                (handler-case
                    (let ((form (read stream nil end-marker)))
                      (when (eq form end-marker)
                        (return
                          (values
                           (or (file-position stream) form-start)
                           nil
                           count)))
                      (funcall function form)
                      (incf count))
                  (end-of-file ()
                    (return (values form-start t count)))
                  (reader-error (cause)
                    (store--fail
                     ':read pathname
                     (format nil "Malformed readable log data: ~A" cause)
                     cause))
                  (stream-error (cause)
                    (store--fail
                     ':read pathname
                     (format nil "Could not read a readable log: ~A" cause)
                     cause)))))
      (close stream))))

(defun log-read (pathname)
  "Read every complete top-level form from PATHNAME with evaluation disabled.

The first value is the complete form list. The second is true only when reading
stopped inside an incomplete final form. A missing file returns two NIL values.
Malformed complete input signals STORE-ERROR."
  (let ((forms nil))
    (multiple-value-bind (position incomplete-final-form-p count)
        (log-map (lambda (form)
                   (push form forms))
                 pathname)
      (declare (ignore position count))
      (values (nreverse forms) incomplete-final-form-p))))


;;;; -- Publication --

(defun store--set-mode (pathname mode)
  "Set PATHNAME to integer MODE when one was requested."
  (when mode
    #+sbcl
    (handler-case
        (sb-posix:chmod (namestring pathname) mode)
      (error (cause)
        (store--fail ':permissions pathname
                     (format nil "Could not set file mode ~O: ~A" mode cause)
                     cause)))
    #-sbcl
    (store--fail ':permissions pathname
                 "File modes are not implemented on this Lisp."))
  pathname)

(defun store--temporary-pathname (pathname)
  "Return an unused temporary pathname beside PATHNAME."
  (loop
    for nonce = (random most-positive-fixnum)
    for temporary =
      (make-pathname
       :name (format nil ".~A.~D.~D"
                     (or (pathname-name pathname) "state")
                     (get-universal-time)
                     nonce)
       :type "tmp"
       :defaults pathname)
    unless (probe-file temporary)
      return temporary))

(defun store--write-forms (pathname forms &key append mode)
  "Write readable FORMS to PATHNAME, optionally appending them."
  (ensure-directories-exist pathname)
  (handler-case
      (with-open-file (stream pathname
                              :direction :output
                              :if-exists (if append :append :supersede)
                              :if-does-not-exist :create
                              :external-format :utf-8)
        (let ((*print-circle* t)
              (*print-readably* t)
              (*print-pretty* t))
          (dolist (form forms)
            (prin1 form stream)
            (terpri stream))
          (finish-output stream)))
    (store-error (condition)
      (error condition))
    (error (cause)
      (store--fail ':write pathname
                   (format nil "Could not write readable state: ~A" cause)
                   cause)))
  (store--set-mode pathname mode)
  pathname)

(defun log-write (pathname forms &key (mode #o600) require-absent)
  "Atomically publish complete readable FORMS at PATHNAME.

When REQUIRE-ABSENT is true, signal PUBLICATION-CONFLICT rather than replacing
a target that appeared while the temporary file was being written. Callers
must serialize competing writers when replacing an existing file."
  (unless (listp forms)
    (store--fail ':write pathname "FORMS must be a list."))
  (let ((temporary (store--temporary-pathname pathname)))
    (unwind-protect
         (progn
           (store--write-forms temporary forms :mode mode)
           (when (and require-absent (probe-file pathname))
             (error 'publication-conflict
                    :message "The state file appeared during publication."
                    :operation ':publish
                    :pathname pathname
                    :cause nil))
           (uiop:rename-file-overwriting-target temporary pathname)
           (store--set-mode pathname mode))
      (when (probe-file temporary)
        (delete-file temporary))))
  pathname)

(defun snapshot-write (pathname form &key (mode #o600) require-absent)
  "Atomically publish one readable FORM at PATHNAME."
  (log-write pathname (list form)
             :mode mode
             :require-absent require-absent))

(defun log-append
    (pathname form &key initial-forms (mode #o600) (repair-tail-p t))
  "Append one complete FORM to PATHNAME, atomically creating or repairing it.

INITIAL-FORMS precede FORM when the log does not exist. If a crash left an
incomplete final form, this operation atomically replaces the file with all
complete preceding forms followed by FORM. REPAIR-TAIL-P may be false only when
the caller has already verified the tail and wants a constant-time append.
Callers must serialize writers."
  (if (probe-file pathname)
      (if repair-tail-p
          (multiple-value-bind (forms incomplete-final-form-p)
              (log-read pathname)
            (if incomplete-final-form-p
                (log-write pathname (append forms (list form)) :mode mode)
                (store--write-forms pathname (list form)
                                    :append t
                                    :mode mode)))
          (store--write-forms pathname (list form) :append t :mode mode))
      (log-write pathname
                 (append (copy-list initial-forms) (list form))
                 :mode mode
                 :require-absent t))
  pathname)
