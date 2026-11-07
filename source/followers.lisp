(in-package #:sexp-store)

;;;; -- Log Followers --

;;; A log only grows between compactions, so a reader that keeps what it has
;;; folded needs only the records appended since its last look. A follower is
;;; such a reader for one LOG-STORE in one process. It never changes a state
;;; it has returned: new records are folded into a copy, so a caller may keep
;;; and walk an earlier state while the follower moves on.

(defclass log-follower ()
  ((store
    :initarg       :store
    :reader        log-follower-store
    :documentation "The LOG-STORE this follower reads.")
   (copier
    :initarg       :copier
    :reader        log-follower-copier
    :documentation "A function of folded state returning a copy the reducer may change.")
   (state
    :initform      nil
    :accessor      log-follower--state
    :documentation "The state folded from the log up to POSITION, never changed in place.")
   (position
    :initform      nil
    :accessor      log-follower--position
    :documentation "The byte position after the last complete form folded, or NIL before the first read.")
   (identity
    :initform      nil
    :accessor      log-follower--identity
    :documentation "The identity of the file read, so a replaced log is folded again from its start."))
  (:documentation "A process-local view of one log that folds only what was appended since it last read.

Callers serialize their use of one follower; the store's lock serializes the
processes."))

(defun log-follower-create (store &key copier)
  "Return a follower of log STORE.

COPIER receives folded state and returns a copy the reducer may change, so
states already returned stay as they were."
  (unless (typep store 'log-store)
    (store--fail ':read nil "A follower reads a LOG-STORE."))
  (unless (functionp copier)
    (store--fail ':read (transaction-store-pathname store) "A follower needs a COPIER function."))
  (make-instance 'log-follower :store store :copier copier))

(defun log-follower-read (follower &key lock-held-p)
  "Return FOLLOWER's current public state and whether its log has an incomplete final form.

Only the records appended since the last read are folded. A log that was
replaced, as by compaction, shortened or removed is folded again from its
start. LOCK-HELD-P is true only when the caller already holds the store's lock."
  (let ((store (log-follower-store follower)))
    (flet ((read-state ()
             (let ((incomplete-p (log-follower--catch-up follower)))
               (values (store--finalize store (log-follower--state follower)) incomplete-p))))
      (if lock-held-p
          (read-state)
          (store--call-with-lock store #'read-state)))))

(defun log-follower-transact (follower update &key publish)
  "Run UPDATE as STORE-TRANSACT does, against FOLLOWER's state instead of a fresh read.

UPDATE receives a copy of the current state and returns CHANGE, RESULT and
WRITE-P. The change is validated, appended and folded into a copy, and the
follower moves past it. A log with an incomplete final form, or no log yet,
takes STORE-TRANSACT's full path, which repairs or creates it. PUBLISH receives
the committed state. Return RESULT and the committed state."
  (let ((store (log-follower-store follower)))
    (store--call-with-lock
     store
     (lambda ()
       (let ((incomplete-p (log-follower--catch-up follower)))
         (if (or incomplete-p (null (log-follower--identity follower)))
             (multiple-value-prog1 (store--transact-locked store update publish)
               (log-follower--forget follower)
               (log-follower--catch-up follower))
             (log-follower--transact-appending follower update publish)))))))

(defun log-follower--transact-appending (follower update publish)
  "Append UPDATE's change to FOLLOWER's complete log, while the caller holds its lock."
  (let* ((store   (log-follower-store follower))
         (copier  (log-follower-copier follower))
         (state   (log-follower--state follower)))
    (multiple-value-bind (change result write-p)
        (funcall update (store--finalize store (funcall copier state)))
      (when write-p
        (let ((next (funcall copier state)))
          (dolist (record (store--validate-change store change))
            (setf next (funcall (log-store-reducer store) next record)))
          (store--append-change store change)
          (let ((information (log-follower--information store)))
            (setf (log-follower--state follower)    next
                  (log-follower--position follower) (ls-compat.posix:file-information-size information)
                  (log-follower--identity follower) (ls-compat.posix:file-information-identity information)
                  state                             next))))
      (let ((committed (store--finalize store state)))
        (when publish
          (funcall publish committed))
        (values result committed)))))

(defun log-follower--information (store)
  "Return the file information of STORE's log, or NIL when it does not exist."
  (let ((pathname (transaction-store-pathname store)))
    (handler-case
        (ls-compat.posix:file-information pathname :follow-links-p t)
      (ls-compat.posix:file-operation-failed ()
        nil))))

(defun log-follower--forget (follower)
  "Make FOLLOWER fold its log again from the start on its next read."
  (setf (log-follower--state follower)    nil
        (log-follower--position follower) nil
        (log-follower--identity follower) nil)
  nil)

(defun log-follower--catch-up (follower)
  "Fold what was appended to FOLLOWER's log since its last read, and return whether a final form is incomplete."
  (let* ((store       (log-follower-store follower))
         (pathname    (transaction-store-pathname store))
         (information (log-follower--information store)))
    (cond
      ((null information)
       (setf (log-follower--state follower)    (funcall (transaction-store-initial-state store))
             (log-follower--position follower) 0
             (log-follower--identity follower) nil)
       nil)
      (t
       (let ((identity (ls-compat.posix:file-information-identity information))
             (size     (ls-compat.posix:file-information-size information)))
         (unless (and (log-follower--position follower)
                      (equal identity (log-follower--identity follower))
                      (<= (log-follower--position follower) size))
           (log-follower--forget follower))
         (let ((position (or (log-follower--position follower) 0)))
           (if (and (= position size) (log-follower--state follower))
               nil
               (log-follower--fold-tail follower :pathname pathname
                                                 :position position
                                                 :identity identity))))))))

(defun log-follower--fold-tail (follower &key pathname position identity)
  "Fold FOLLOWER's log from POSITION into a fresh copy of its state, then record where it stopped.

Return whether the log ends in an incomplete form."
  (let* ((store    (log-follower-store follower))
         (header-p (zerop position))
         (state    (if header-p
                       (funcall (transaction-store-initial-state store))
                       (log-follower--state follower)))
         (copied-p header-p))
    (multiple-value-bind (next incomplete-p)
        (log-map
         (lambda (form)
           (store--validate-record
            store form (if header-p
                           (log-store-header-validator store)
                           (transaction-store-validator store)))
           (cond
             (header-p
              (setf header-p nil))
             (t
              (unless copied-p
                (setf state    (funcall (log-follower-copier follower) state)
                      copied-p t))
              (setf state (funcall (log-store-reducer store) state form)))))
         pathname
         :start-position position)
      (when header-p
        (store--fail ':validate pathname "The log has no complete header."))
      (setf (log-follower--state follower)    state
            (log-follower--position follower) next
            (log-follower--identity follower) identity)
      incomplete-p)))
