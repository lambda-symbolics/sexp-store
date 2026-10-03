(in-package #:sexp-store)

;;;; -- Property List Schemas --

(defun plist-schema-problem
    (properties &key (allowed-keys nil allowed-keys-p) required-keys
                         (keyword-keys-p t) maximum-length
                         allow-duplicate-keys)
  "Return a schema problem and its key, or two NIL values when valid.

Problems are :IMPROPER, :ODD, :TOO-LONG, :NON-KEYWORD, :UNKNOWN, :DUPLICATE
and :MISSING. Validation follows property order; required keys follow
REQUIRED-KEYS order. An omitted ALLOWED-KEYS accepts any keyword key."
  (block check
    (let ((seen-conses (make-hash-table :test #'eq))
          (seen-keys (make-hash-table :test #'eq))
          (tail properties)
          (count 0))
      (loop while tail
            do (unless (consp tail)
                 (return-from check (values ':improper nil)))
               (when (gethash tail seen-conses)
                 (return-from check (values ':improper nil)))
               (when (and maximum-length (>= count maximum-length))
                 (return-from check (values ':too-long nil)))
               (setf (gethash tail seen-conses) t
                     tail (rest tail))
               (incf count))
      (when (oddp count)
        (return-from check (values ':odd nil)))
      (loop for key in properties by #'cddr
            do (when (and keyword-keys-p (not (keywordp key)))
                 (return-from check (values ':non-keyword key)))
               (when (and allowed-keys-p (not (member key allowed-keys :test #'eq)))
                 (return-from check (values ':unknown key)))
               (when (and (not allow-duplicate-keys) (gethash key seen-keys))
                 (return-from check (values ':duplicate key)))
               (setf (gethash key seen-keys) t))
      (dolist (key required-keys)
        (unless (gethash key seen-keys)
          (return-from check (values ':missing key))))
      (values nil nil))))

(defun plist-schema-p
    (properties &rest options &key allowed-keys required-keys keyword-keys-p
                                  maximum-length allow-duplicate-keys)
  "Return true when PROPERTIES satisfies the supplied schema options."
  (declare (ignore allowed-keys required-keys keyword-keys-p maximum-length
                   allow-duplicate-keys))
  (null (apply #'plist-schema-problem properties options)))
