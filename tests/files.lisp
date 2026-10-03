(in-package #:sexp-store/tests)

(defun tests--octets (text)
  "Return TEXT's character codes as an octet vector."
  (map '(vector (unsigned-byte 8)) #'char-code text))

(defun tests--file-octets (pathname)
  "Return PATHNAME's contents as octets, or NIL when it does not exist."
  (when (probe-file pathname)
    (with-open-file (stream pathname :element-type '(unsigned-byte 8))
      (let ((octets (make-array (file-length stream) :element-type '(unsigned-byte 8))))
        (read-sequence octets stream)
        octets))))

(defun tests--write-octets (pathname text)
  "Replace PATHNAME's contents with TEXT's octets."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname :direction :output :element-type '(unsigned-byte 8)
                                   :if-exists :supersede :if-does-not-exist :create)
    (write-sequence (tests--octets text) stream))
  pathname)

(defun tests--directory-files (directory)
  "Return the names of every file directly in DIRECTORY, hidden ones included."
  (sort (mapcar #'file-namestring
                (uiop:directory-files directory))
        #'string<))

(defun tests--files (root)
  "Exercise transactional multi-file publication beneath ROOT."
  (let* ((directory (merge-pathnames "files/" root))
         (existing (tests--write-octets (merge-pathnames "existing.txt" directory) "old"))
         (fresh (merge-pathnames "nested/deeper/fresh.txt" directory)))
    (test-assert
     (= 2 (files-publish (list (list :pathname existing
                                     :octets (tests--octets "new")
                                     :expected (tests--octets "old"))
                               (list :pathname fresh
                                     :octets (tests--octets "fresh")
                                     :expected nil))))
     "a publication reports every published file")
    (test-assert (and (equalp (tests--file-octets existing) (tests--octets "new"))
                      (equalp (tests--file-octets fresh) (tests--octets "fresh")))
                 "replacements and new files are both published")
    (test-assert (equal (tests--directory-files directory) '("existing.txt"))
                 "no staging files remain after success")
    (test-assert (= (tests--file-mode fresh) #o600)
                 "new files are private")
    (let ((wild (uiop:parse-native-namestring
                 (concatenate 'string
                              (uiop:native-namestring directory)
                              "odd [dir]*/name *?[x].sexp"))))
      (test-assert (and (= 1 (files-publish (list (list :pathname wild
                                                        :octets (tests--octets "wild")
                                                        :expected nil))))
                        (equalp (tests--file-octets wild) (tests--octets "wild")))
                   "targets whose names hold wildcard characters publish literally")
      (delete-file wild)
      (uiop:delete-empty-directory (uiop:pathname-directory-pathname wild)))
    (let ((another (merge-pathnames "other/another.txt" directory)))
      (test-assert
       (handler-case
           (progn
             (files-publish (list (list :pathname existing
                                        :octets (tests--octets "newer")
                                        :expected (tests--octets "new"))
                                  (list :pathname another
                                        :octets (tests--octets "another")
                                        :expected nil)
                                  (list :pathname fresh
                                        :octets (tests--octets "lost")
                                        :expected (tests--octets "not what is there"))))
             nil)
         (publication-conflict ()
           t))
       "a changed target signals a publication conflict")
      (test-assert (and (equalp (tests--file-octets existing) (tests--octets "new"))
                        (null (probe-file another))
                        (null (uiop:directory-exists-p
                               (merge-pathnames "other/" directory)))
                        (equalp (tests--file-octets fresh) (tests--octets "fresh")))
                   "a conflict restores published files and removes new ones and their directories")
      (test-assert (equal (tests--directory-files directory) '("existing.txt"))
                   "a failed publication removes its staging files"))
    (test-assert
     (handler-case
         (progn
           (files-publish (list (list :pathname existing
                                      :octets (tests--octets "refused")
                                      :expected (tests--octets "new")))
                          :check (lambda (pathname)
                                   (declare (ignore pathname))
                                   (error "Refused by the caller.")))
           nil)
       (error (condition)
         (search "Refused by the caller." (princ-to-string condition))))
     "the check callback can refuse a target")
    (let ((calls 0)
          (later (merge-pathnames "later.txt" directory)))
      (test-assert
       (handler-case
           (progn
             (files-publish (list (list :pathname existing
                                        :octets (tests--octets "replaced")
                                        :expected (tests--octets "new"))
                                  (list :pathname later
                                        :octets (tests--octets "later")
                                        :expected nil))
                            :check (lambda (pathname)
                                     (incf calls)
                                     (cond
                                       ((and (= calls 4) (equal pathname later))
                                        (error "Second target refused."))
                                       ((= calls 5)
                                        (tests--write-octets existing "edited elsewhere")))))
             nil)
         (error (condition)
           (let ((failure (and (typep condition 'publication-rollback-failed)
                               (first (publication-rollback-failed-failures condition)))))
             (and failure
                  (equal (getf failure :pathname) (namestring existing))
                  (getf failure :backup)
                  (equalp (tests--file-octets (pathname (getf failure :backup)))
                          (tests--octets "new"))
                  (equalp (tests--file-octets existing)
                          (tests--octets "edited elsewhere"))))))
       "a target changed again during rollback is kept and its backup reported")))
  nil)
