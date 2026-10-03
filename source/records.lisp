(in-package #:sexp-store)

;;;; -- Versioned Records --

(defun record--property-list-p (properties)
  "Return true when PROPERTIES is a finite complete property list."
  (plist-schema-p properties :keyword-keys-p nil :allow-duplicate-keys t))

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
Return true and NIL for a valid record, otherwise NIL and a reason.
The property-list shape and bound are checked before field validation."
  (block check
    (flet ((fail (reason &rest arguments)
             (let ((*print-circle* t))
               (return-from check
                 (values nil (apply #'format nil reason arguments))))))
      (unless (or properties-p (and (consp form) (eq (first form) tag)))
        (fail "the form is not a ~S record" tag))
      (let* ((properties (if properties-p form (rest form)))
             (allowed-keys
               (and (not allow-other-keys)
                    (append (mapcar (lambda (field)
                                      (getf field ':indicator))
                                    fields)
                            (and versions-p (list ':version)))))
             (required-keys
               (mapcar (lambda (field) (getf field ':indicator))
                       (remove-if-not (lambda (field)
                                       (eq (getf field ':required) t))
                                     fields))))
        (multiple-value-bind (problem key)
            (if allow-other-keys
                (plist-schema-problem
                 properties
                 :required-keys required-keys
                 :keyword-keys-p keyword-keys-p
                 :maximum-length maximum-length
                 :allow-duplicate-keys allow-duplicate-keys)
                (plist-schema-problem
                 properties
                 :allowed-keys allowed-keys
                 :required-keys required-keys
                 :keyword-keys-p keyword-keys-p
                 :maximum-length maximum-length
                 :allow-duplicate-keys allow-duplicate-keys))
          (when problem
            (fail "the record property list has problem ~S~@[ at ~S~]"
                  problem key)))
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
                             (and versions-p
                                  (member version required :test #'eql))))
                (fail "the required property ~S is missing" indicator))
              (when (and present-p validate
                         (not (handler-case
                                  (and (funcall validate
                                                (getf properties indicator))
                                       t)
                                (error () nil))))
                (fail "the property ~S holds an unsupported value" indicator)))))
        (values t nil)))))

(defun snapshot-read-record
    (pathname &key tag (versions nil versions-p) fields (allow-other-keys t)
                         allow-duplicate-keys properties-p keyword-keys-p
                         maximum-length grammar maximum-octets)
  "Read PATHNAME as exactly one record and validate its property schema."
  (multiple-value-bind (form sole-form-p)
      (snapshot-read pathname :grammar grammar :maximum-octets maximum-octets)
    (unless sole-form-p
      (store--fail ':validate pathname
                   "The snapshot must hold exactly one form."))
    (multiple-value-bind (valid-p reason)
        (apply #'record-check form
               :tag tag :fields fields :allow-other-keys allow-other-keys
               :allow-duplicate-keys allow-duplicate-keys :properties-p properties-p
               :keyword-keys-p keyword-keys-p :maximum-length maximum-length
               (when versions-p (list :versions versions)))
      (unless valid-p
        (store--fail ':validate pathname
                     (format nil "Malformed ~S record: ~A." tag reason))))
    (values form (if properties-p (getf form ':version) (record-version form)))))
