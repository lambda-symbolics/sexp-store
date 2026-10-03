(in-package #:sexp-store)

;;;; -- Multi-File Publication --

(define-condition publication-rollback-failed (store-error)
  ((failures
    :initarg :failures
    :reader publication-rollback-failed-failures
    :type list
    :documentation
    "One plist per target that could not be restored, holding :PATHNAME,
:BACKUP (the kept copy of its previous contents, or NIL) and :MESSAGE."))
  (:documentation
   "A multi-file publication failed and could not restore every target it had
already replaced. The kept backups are the only remaining copies of the
previous contents."))

(defun files-publish (writes &key check (mode #o600) (directory-mode #o700))
  "Publish every file in WRITES, or restore all of them when any one fails.

Each write is a plist of :PATHNAME, :OCTETS (the new contents) and :EXPECTED
(the octets the target must still hold, or NIL when it must not exist). Every
replacement and a copy of every previous content are first staged beside their
targets, so publication only renames and links. Each target is compared with
its expected contents immediately before it is published and signals
PUBLICATION-CONFLICT when another writer changed it.

On any failure, targets already published are restored from their copies or
removed, newly created directories are removed, and the condition propagates.
A target that changed again before it could be restored is left alone and its
copy kept; PUBLICATION-ROLLBACK-FAILED then replaces the failure while it
unwinds, reporting every such target, so callers handling the failure should
handle ERROR or STORE-ERROR rather than one specific condition.

CHECK, when supplied, is called with each target pathname before staging,
before publishing and before restoring, and may signal to refuse it. New files
get MODE and new directories DIRECTORY-MODE. Return the number of published
files."
  (files--validate-writes writes)
  (let ((random-state (make-random-state t))
        (staged nil)
        (published nil)
        (created nil)
        (complete-p nil)
        (failure nil)
        (rollback-failures nil))
    (unwind-protect
         (handler-bind ((error (lambda (condition) (setf failure condition))))
           (dolist (write writes)
             (let ((pathname (getf write :pathname))
                   (expected (getf write :expected)))
               (files--check check pathname)
               (setf created (append (files--ensure-directories pathname directory-mode)
                                     created))
               (let ((entry (list :write write :temporary nil :backup nil)))
                 (push entry staged)
                 (setf (getf entry :temporary)
                       (files--stage pathname (getf write :octets) mode random-state))
                 (when expected
                   (setf (getf entry :backup)
                         (files--stage pathname expected mode random-state))))))
           (dolist (entry (reverse staged))
             (let* ((write (getf entry :write))
                    (pathname (getf write :pathname))
                    (expected (getf write :expected)))
               (files--check check pathname)
               (unless (equalp expected (files--current-octets pathname))
                 (error 'publication-conflict
                        :message "The target changed before it could be published."
                        :operation ':publish
                        :pathname pathname))
               (files--without-interrupts
                 (lambda ()
                   (files--install (getf entry :temporary) pathname (not (null expected)))
                   (push entry published)))
               (when (probe-file (getf entry :temporary))
                 (delete-file (getf entry :temporary)))))
           (setf complete-p t)
           (length published))
      (unless complete-p
        (setf rollback-failures (files--restore published check)))
      (dolist (entry staged)
        (dolist (pathname (list (getf entry :temporary) (getf entry :backup)))
          (when (and pathname
                     (probe-file pathname)
                     (not (find (namestring pathname) rollback-failures
                                :key (lambda (failure) (getf failure :backup))
                                :test #'equal)))
            (ignore-errors (delete-file pathname)))))
      (unless complete-p
        (dolist (directory (sort (remove-duplicates created :test #'equal)
                                 #'> :key (lambda (pathname) (length (namestring pathname)))))
          (ignore-errors (uiop:delete-empty-directory directory))))
      (when rollback-failures
        (error 'publication-rollback-failed
               :message (format nil "Publication failed (~A) and ~D target~:P could not be restored."
                                failure (length rollback-failures))
               :operation ':rollback
               :pathname (pathname (getf (first rollback-failures) :pathname))
               :cause failure
               :failures rollback-failures)))))

(defun files--validate-writes (writes)
  "Signal a STORE-ERROR unless WRITES is a proper list of complete write plists."
  (unless (and (handler-case (not (null (list-length writes)))
                 (type-error () nil))
               (every (lambda (write)
                        (and (listp write)
                             (pathnamep (getf write :pathname))
                             (typep (getf write :octets) '(vector (unsigned-byte 8)))
                             (typep (getf write :expected)
                                    '(or null (vector (unsigned-byte 8))))))
                      writes))
    (error 'store-error
           :message "Writes must be plists of :PATHNAME, :OCTETS and :EXPECTED octets."
           :operation ':validate
           :pathname #p""))
  writes)

(defun files--check (check pathname)
  "Let CHECK refuse PATHNAME, when CHECK is supplied."
  (when check
    (funcall check pathname))
  nil)

(defun files--ensure-directories (pathname directory-mode)
  "Create PATHNAME's missing parents with DIRECTORY-MODE; return those created."
  (let ((created nil)
        (current (uiop:pathname-directory-pathname pathname)))
    (loop until (uiop:directory-exists-p current)
          do (push current created)
             (setf current (uiop:pathname-parent-directory-pathname current)))
    (dolist (directory created)
      (ensure-directories-exist directory)
      (store--set-mode directory directory-mode))
    created))

(defun files--stage (target octets mode random-state)
  "Write OCTETS to a new private staging file beside TARGET and return its pathname."
  (loop
    ;; MAKE-PATHNAME keeps a target directory holding wildcard characters
    ;; literal, which parsing a namestring would not.
    (let ((pathname
            (make-pathname :name (format nil ".sexp-store-~36R"
                                         (random (expt 36 12) random-state))
                           :type "staging"
                           :version nil
                           :defaults (uiop:pathname-directory-pathname target))))
      (let ((stream (handler-case
                        (open pathname
                              :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists nil
                              :if-does-not-exist :create)
                      (error (cause)
                        (store--fail ':write pathname
                                     "Could not create a staging file." cause)))))
        (when stream
          (unwind-protect
               (progn
                 (store--set-mode pathname mode)
                 (write-sequence octets stream)
                 (finish-output stream))
            (close stream))
          (return pathname))))))

(defun files--current-octets (pathname)
  "Return PATHNAME's complete contents, or NIL when it does not exist."
  (when (probe-file pathname)
    (with-open-file (stream pathname :element-type '(unsigned-byte 8))
      (let* ((octets (make-array (file-length stream) :element-type '(unsigned-byte 8)))
             (count (read-sequence octets stream)))
        (if (= count (length octets))
            octets
            (subseq octets 0 count))))))

(defun files--install (temporary pathname replace-p)
  "Move TEMPORARY to PATHNAME, replacing it when REPLACE-P and otherwise only creating it."
  (if replace-p
      (progn
        (store--make-replaceable pathname)
        (uiop:rename-file-overwriting-target temporary pathname))
      (store--publish-exclusive temporary pathname))
  pathname)

(defun files--restore (published check)
  "Return each PUBLISHED target to its previous state, newest first.

Return the failures as plists of :PATHNAME, :BACKUP and :MESSAGE."
  (let ((failures nil))
    (dolist (entry published (nreverse failures))
      (let* ((write (getf entry :write))
             (pathname (getf write :pathname))
             (backup (getf entry :backup)))
        (handler-case
            (progn
              (files--check check pathname)
              (unless (equalp (files--current-octets pathname) (getf write :octets))
                (error 'publication-conflict
                       :message "The target changed again after publication, so it was left alone."
                       :operation ':rollback
                       :pathname pathname))
              (if backup
                  (files--install backup pathname t)
                  (delete-file pathname)))
          (error (condition)
            (push (list :pathname (namestring pathname)
                        :backup (and backup (namestring backup))
                        :message (princ-to-string condition))
                  failures)))))))

(defun files--without-interrupts (function)
  "Call FUNCTION so an asynchronous interrupt cannot separate a publication from its record."
  #+sbcl
  (sb-sys:without-interrupts (funcall function))
  #-sbcl
  (funcall function))
