(in-package #:sexp-store)

;;;; -- Versioned Records --

;;; A versioned record is one readable form (TAG :version VERSION ...)
;;; whose rest is a property list. Field descriptions declare which
;;; properties each version must carry and what values they may hold,
;;; replacing hand-rolled per-record validators.

(defun record--property-list-p (properties)
  "Return true when PROPERTIES is a finite complete property list."
  (handler-case
      (let ((length (list-length properties)))
        (and length (evenp length)))
    (type-error ()
      nil)))

(defun record--present-p (properties indicator)
  "Return true when INDICATOR appears in the PROPERTIES plist."
  (do ((tail properties (cddr tail)))
      ((null tail) nil)
    (when (eq (first tail) indicator)
      (return t))))

(defun make-record (tag version &rest properties)
  "Return a fresh record form (TAG :version VERSION . PROPERTIES)."
  (list* tag ':version version properties))

(defun record-version (form)
  "Return FORM's :version property, or NIL when FORM has none."
  (and (consp form)
       (record--property-list-p (rest form))
       (getf (rest form) ':version)))

(defun record-property (form indicator &optional default)
  "Return INDICATOR's value in record FORM, or DEFAULT when absent."
  (if (and (consp form)
           (record--property-list-p (rest form)))
      (getf (rest form) indicator default)
      default))

(defun record-property-present-p (form indicator)
  "Return true when record FORM carries INDICATOR, even with a NIL value."
  (and (consp form)
       (record--property-list-p (rest form))
       (record--present-p (rest form) indicator)))

(defun record-check
    (form &key tag (versions nil versions-p) fields (allow-other-keys t)
               allow-duplicate-keys properties-p keyword-keys-p maximum-length)
  "Check FORM against a property record schema.

FORM is (TAG . PROPERTIES), or PROPERTIES itself when PROPERTIES-P is true.
Supplying VERSIONS requires its :version value to belong to that list; omitting
VERSIONS leaves the record unversioned. Each entry in FIELDS is a plist:
:INDICATOR names a property, :VALIDATE optionally checks present values, and
:REQUIRED is T or a list of versions that must carry it. ALLOW-OTHER-KEYS NIL
rejects undescribed properties, except :version when VERSIONS is supplied.
The body must be a finite plist. KEYWORD-KEYS-P requires keyword indicators,
and MAXIMUM-LENGTH bounds the number of plist elements. Duplicate indicators
are rejected unless ALLOW-DUPLICATE-KEYS is true; then the first value wins.
Return true and NIL for a valid record, otherwise NIL and a reason."
  (block check
    (flet ((fail (reason &rest arguments)
             (let ((*print-circle* t))
               (return-from check
                 (values nil (apply #'format nil reason arguments))))))
      (unless (or properties-p (and (consp form) (eq (first form) tag)))
        (fail "the form is not a ~S record" tag))
      (let ((properties (if properties-p form (rest form))))
        (unless (record--property-list-p properties)
          (fail "the record body is not a property list"))
        (when (and maximum-length (> (length properties) maximum-length))
          (fail "the record body exceeds ~D property list elements" maximum-length))
        (let ((seen (unless allow-duplicate-keys (make-hash-table :test #'eq))))
          (loop for indicator in properties by #'cddr
                do (when (and keyword-keys-p (not (keywordp indicator)))
                     (fail "the property ~S is not a keyword" indicator))
                   (when seen
                     (when (gethash indicator seen)
                       (fail "the property ~S is repeated" indicator))
                     (setf (gethash indicator seen) t))))
        (let ((version (and versions-p (getf properties ':version))))
          (when (and versions-p (not (member version versions :test #'eql)))
            (fail "record version ~S is not supported" version))
          (dolist (field fields)
            (let* ((indicator (getf field ':indicator))
                   (validate (getf field ':validate))
                   (required (getf field ':required))
                   (present-p (record--present-p properties indicator)))
              (when (and (not present-p)
                         (or (eq required t)
                             (and versions-p (member version required :test #'eql))))
                (fail "the required property ~S is missing" indicator))
              (when (and present-p
                         validate
                         (not (handler-case
                                  (and (funcall validate
                                                (getf properties indicator))
                                       t)
                                (error () nil))))
                (fail "the property ~S holds an unsupported value"
                      indicator))))
          (unless allow-other-keys
            (do ((tail properties (cddr tail)))
                ((null tail))
              (let ((indicator (first tail)))
                (unless (or (and versions-p (eq indicator ':version))
                            (member indicator fields
                                    :key (lambda (field)
                                           (getf field ':indicator))))
                  (fail "the property ~S is not part of this record"
                        indicator)))))
          (values t nil))))))

(defun snapshot-read-record
    (pathname &key tag versions fields (allow-other-keys t) allow-duplicate-keys)
  "Read PATHNAME as exactly one versioned TAG record.

TAG, VERSIONS, FIELDS, ALLOW-OTHER-KEYS and ALLOW-DUPLICATE-KEYS are interpreted
as by RECORD-CHECK. Return the record form and its version. Signal
STORE-ERROR when the file does not hold exactly one valid record."
  (multiple-value-bind (form sole-form-p)
      (snapshot-read pathname)
    (unless sole-form-p
      (store--fail ':validate pathname
                   "The snapshot must hold exactly one form."))
    (multiple-value-bind (valid-p reason)
        (record-check form
                      :tag tag
                      :versions versions
                      :fields fields
                      :allow-other-keys allow-other-keys
                      :allow-duplicate-keys allow-duplicate-keys)
      (unless valid-p
        (store--fail ':validate pathname
                     (format nil "Malformed ~S record: ~A." tag reason))))
    (values form (record-version form))))
