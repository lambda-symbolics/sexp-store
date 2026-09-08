(in-package #:sexp-store)

;;;; -- Segmented Logs --

(define-condition segment-error (store-error)
  ((sequence
    :initarg :sequence
    :initform nil
    :reader segment-error-sequence
    :documentation "The invalid sequence or boundary, when available."))
  (:documentation "A readable log segment violates its declared boundaries."))

(defun segment--fail (pathname message &optional sequence)
  "Signal a segment validation failure at PATHNAME."
  (error 'segment-error :pathname pathname :operation ':validate
                        :message message :sequence sequence))

(defun segment-map
    (function pathname &key header-function record-sequence validate-record
                            validate-first-record expected-start-sequence)
  "Visit complete records in one segment after validating its header and sequence.

HEADER-FUNCTION receives PATHNAME and the header and returns the positive start
sequence and whether a header-only segment is allowed. RECORD-SEQUENCE extracts a
positive sequence from each record. Optional validation callbacks receive
PATHNAME and RECORD, or PATHNAME, HEADER and RECORD for the first record.
Callback conditions propagate unchanged. An incomplete final form is tolerated;
an absent header, missing required first record, or sequence gap is an error.
Return next byte position, incomplete-tail flag, record count, start sequence,
and next sequence. Headers are not passed to FUNCTION."
  (let ((header nil)
        (header-seen-p nil)
        (allow-empty-p nil)
        (count 0)
        (start nil)
        (next nil))
    (multiple-value-bind (position incomplete-p mapped-count)
        (log-map
         (lambda (record)
           (unless (record-finite-p record)
             (segment--fail pathname "A segment contains non-finite record data."))
           (if header-seen-p
               (progn
                 (when validate-record
                   (funcall validate-record pathname record))
                 (when (and (zerop count) validate-first-record)
                   (funcall validate-first-record pathname header record))
                 (let ((sequence (funcall record-sequence record)))
                   (unless (and (typep sequence '(integer 1)) (= sequence next))
                     (segment--fail pathname "Segment sequences are not contiguous."
                                    sequence))
                   (incf next)
                   (incf count)
                   (funcall function record)))
               (multiple-value-bind (sequence empty-p)
                   (funcall header-function pathname record)
                 (unless (typep sequence '(integer 1))
                   (segment--fail pathname "A segment has an invalid start sequence."
                                  sequence))
                 (when (and expected-start-sequence
                            (/= sequence expected-start-sequence))
                   (segment--fail pathname "Segment boundaries are not contiguous."
                                  sequence))
                 (setf header record
                       header-seen-p t
                       allow-empty-p empty-p
                       start sequence
                       next sequence))))
         pathname)
      (declare (ignore mapped-count))
      (unless header-seen-p
        (segment--fail pathname "A segment has no complete header."))
      (when (and (zerop count) (not allow-empty-p))
        (segment--fail pathname "A segment has no durable record." start))
      (values position incomplete-p count start next))))

(defun segments-map
    (function pathnames &rest options &key header-function record-sequence
                                          validate-record validate-first-record)
  "Visit ordered PATHNAMES with SEGMENT-MAP, validating adjoining boundaries.

OPTIONS have the same callback contracts as SEGMENT-MAP. Complete records in a
sealed segment with a torn tail are retained; the next segment must start at the
next complete sequence. Return the active segment's incomplete-tail flag, total
record count, and next sequence. An empty pathname list returns NIL, zero, NIL."
  (declare (ignore header-function record-sequence validate-record
                   validate-first-record))
  (let ((incomplete-p nil)
        (count 0)
        (next nil))
    (dolist (pathname pathnames)
      (multiple-value-bind (position tail-p segment-count start segment-next)
          (apply #'segment-map function pathname
                 :expected-start-sequence next options)
        (declare (ignore position start))
        (incf count segment-count)
        (setf incomplete-p tail-p
              next segment-next)))
    (values incomplete-p count next)))

(defun segment--record-text (record)
  "Return RECORD's canonical readable representation for exact publication checks."
  (with-standard-io-syntax
    (write-to-string record :readably t :circle t :pretty nil)))

(defun segment-published-p (pathname header record)
  "Return true when PATHNAME holds exactly the complete HEADER and RECORD."
  (handler-case
      (multiple-value-bind (forms incomplete-p) (log-read pathname)
        (and (not incomplete-p)
             (= (length forms) 2)
             (record-finite-p forms)
             (record-finite-p header)
             (record-finite-p record)
             (string= (segment--record-text (first forms)) (segment--record-text header))
             (string= (segment--record-text (second forms)) (segment--record-text record))
             t))
    (error ()
      nil)))

(defun segment-publish
    (pathname header record &key lock-pathname occupied-p)
  "Publish HEADER and RECORD as a new segment, never adopting an existing file.

Serialize writers with LOCK-PATHNAME or an already held exclusive writer lease.
OCCUPIED-P, when supplied, is a zero-argument predicate for related storage that
also prevents publication. Publication errors are recoverable when a readback
proves that exactly these forms were published. Caller state may advance only
after this function returns. Return PATHNAME."
  (unless (and (record-finite-p header) (record-finite-p record))
    (segment--fail pathname "A segment requires finite portable records."))
  (flet ((publish ()
           (when (or (probe-file pathname)
                     (and occupied-p (funcall occupied-p)))
             (error 'publication-conflict :pathname pathname :operation ':publish
                                         :message "Segment storage is occupied."))
           (handler-case
               (log-write pathname (list header record) :require-absent t)
             (publication-conflict (condition)
               (error condition))
             (error (condition)
               (unless (segment-published-p pathname header record)
                 (error condition))))))
    (if lock-pathname
        (progn
          (ensure-directories-exist lock-pathname)
          (ls-flock:call-with-file-lock lock-pathname #'publish))
        (publish)))
  pathname)

(defun log-repair-tail (pathname)
  "Atomically remove an incomplete final form. Return true when repaired.

The caller must hold its writer lock. Complete malformed forms signal
STORE-ERROR and are not discarded."
  (multiple-value-bind (forms incomplete-p) (log-read pathname)
    (when incomplete-p
      (log-write pathname forms))
    (not (null incomplete-p))))
