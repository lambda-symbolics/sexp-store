(in-package #:sexp-store)

;;;; -- Segmented Logs --

(define-condition segment-error (store-error)
  ((sequence
    :initarg       :sequence
    :initform      nil
    :reader        segment-error-sequence
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
  (let ((header        nil)
        (header-seen-p nil)
        (allow-empty-p nil)
        (count         0)
        (start         nil)
        (next          nil))
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

(defun segment-visit
    (function pathname &key header-function record-sequence validate-record
                            validate-first-record expected-start-sequence
                            (start-position 0) next-sequence (batch-offset 0)
                            maximum-records maximum-octets)
  "Visit a bounded portion of an immutable segment, validating its actual header.

Callbacks have the contracts of SEGMENT-MAP. Return seek position, next sequence,
status (:STOPPED, :EOF or :TORN-TAIL), count and batch offset. To resume the same
immutable segment, supply the returned position, sequence and offset together.
The offset skips only already visited records in the first form at that position.
MAXIMUM-RECORDS is a hard logical record limit. MAXIMUM-OCTETS is a soft budget
checked at form boundaries after visiting at least one form. The caller must
protect the segment from replacement between visits. No callback errors are caught."
  (unless (and (typep start-position '(integer 0))
               (typep batch-offset '(integer 0))
               (or (null maximum-records) (typep maximum-records '(integer 1)))
               (or (null maximum-octets) (typep maximum-octets '(integer 1))))
    (segment--fail pathname "Invalid segment cursor or budget."))
  (multiple-value-bind (header header-end)
      (block read-header
        (log-map (lambda (form begin end)
                   (declare (ignore begin))
                   (return-from read-header (values form end)))
                 pathname :with-positions-p t :physical-forms-p t)
        (segment--fail pathname "A segment has no complete header."))
    (unless (record-finite-p header)
      (segment--fail pathname "A segment contains non-finite header data."))
    (multiple-value-bind (start allow-empty-p) (funcall header-function pathname header)
      (unless (and (typep start '(integer 1))
                   (or (null expected-start-sequence) (eql start expected-start-sequence)))
        (segment--fail pathname "Invalid segment start sequence." start))
      (let* ((begin    (if (zerop start-position) header-end start-position))
             (next     (or next-sequence start))
             (count    0)
             (offset   batch-offset))
        (unless (and (>= begin header-end)
                     (typep next '(integer 1))
                     (>= (- next offset) start)
                     (if (zerop start-position)
                         (and (= next start) (zerop offset))
                         next-sequence))
          (segment--fail pathname "Invalid segment resume position or sequence." next))
        (multiple-value-bind (position incomplete-p)
            (log-map
             (lambda (form form-start form-end)
               (let* ((records (store--batch-records form pathname))
                      (index   0)
                      (wanted  offset)
                      (sequence (- next offset)))
                 (when (and (plusp wanted) (>= wanted (length records)))
                   (segment--fail pathname "Invalid segment batch offset." next))
                 (dolist (record records)
                   (unless (record-finite-p record)
                     (segment--fail pathname "A segment contains non-finite record data."))
                   (when validate-record
                     (funcall validate-record pathname record))
                   (unless (eql sequence (funcall record-sequence record))
                     (segment--fail pathname "Segment sequences are not contiguous." sequence))
                   (when (and (= sequence start) validate-first-record)
                     (funcall validate-first-record pathname header record))
                   (unless (< index wanted)
                     (funcall function record)
                     (incf next)
                     (incf count)
                     (when (and maximum-records (= count maximum-records))
                       (if (= (1+ index) (length records))
                           (return-from segment-visit (values form-end next ':stopped count 0))
                           (return-from segment-visit
                             (values form-start next ':stopped count (1+ index))))))
                   (incf sequence)
                   (incf index))
                 (setf offset 0)
                 (when (and maximum-octets (>= (- form-end begin) maximum-octets))
                   (return-from segment-visit (values form-end next ':stopped count 0)))))
             pathname
             :start-position   begin
             :with-positions-p t
             :physical-forms-p t)
          (when (plusp offset)
            (segment--fail pathname "The resumed transaction batch is missing." next))
          (when (and (= next start) (not allow-empty-p))
            (segment--fail pathname "A segment has no durable record." start))
          (values position next (if incomplete-p ':torn-tail ':eof) count 0))))))
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
        (count        0)
        (next         nil))
    (dolist (pathname pathnames)
      (multiple-value-bind (position tail-p segment-count start segment-next)
          (apply #'segment-map function pathname
                 :expected-start-sequence next options)
        (declare (ignore position start))
        (incf count segment-count)
        (setf incomplete-p tail-p
              next segment-next)))
    (values incomplete-p count next)))

(defun segment-map-reverse
    (function pathname &rest options &key header-function record-sequence
                                          validate-record validate-first-record
                                          expected-start-sequence expected-next-sequence)
  "Visit one validated segment's complete records newest first.

OPTIONS use SEGMENT-MAP's callback contracts. EXPECTED-NEXT-SEQUENCE, when
supplied, must equal the sequence immediately after the last complete record.
Validate the whole segment and this boundary before calling FUNCTION. Buffer
only this segment's records, never adjacent segments; readable records may
contain multiline strings or transaction batches. Callback conditions and
nonlocal exits propagate unchanged. Return SEGMENT-MAP's five values."
  (declare (ignore header-function record-sequence validate-record
                   validate-first-record expected-start-sequence))
  (let ((records         nil)
        (forward-options (copy-list options)))
    (remf forward-options :expected-next-sequence)
    (multiple-value-bind (position incomplete-p count start next)
        (apply #'segment-map (lambda (record) (push record records))
               pathname forward-options)
      (when (and expected-next-sequence (/= next expected-next-sequence))
        (segment--fail pathname "Segment boundaries are not contiguous." next))
      (dolist (record records)
        (funcall function record))
      (values position incomplete-p count start next))))

(defun segments-map-reverse
    (function pathnames &rest options &key header-function record-sequence
                                          validate-record validate-first-record)
  "Visit chronological PATHNAMES in reverse record order, newest segment first.

Use SEGMENT-MAP-REVERSE's callbacks and one-segment buffering. Validate each
older segment's next sequence against the newer segment's start before yielding
its records. A callback may exit without opening older segments. Return the
active segment's incomplete-tail flag, total visited record count, and active
next sequence, matching SEGMENTS-MAP. An empty list returns NIL, zero, NIL."
  (declare (ignore header-function record-sequence validate-record
                   validate-first-record))
  (let ((incomplete-p nil)
        (count        0)
        (next         nil)
        (boundary     nil))
    (dolist (pathname (reverse pathnames))
      (multiple-value-bind (position tail-p segment-count start segment-next)
          (apply #'segment-map-reverse function pathname
                 :expected-next-sequence boundary options)
        (declare (ignore position))
        (unless boundary
          (setf incomplete-p tail-p
                next         segment-next))
        (incf count segment-count)
        (setf boundary start)))
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
