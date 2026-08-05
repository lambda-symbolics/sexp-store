(asdf:defsystem #:sexp-store
  :description "Crash-tolerant readable Common Lisp state files"
  :author "Lukáš Hozda"
  :license "ISC"
  :version "0.2.0"
  :serial t
  :depends-on (#:ls-compat/posix)
  :components ((:module "source"
                :serial t
                :components ((:file "package")
                             (:file "store"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:sexp-store/tests))))

(asdf:defsystem #:sexp-store/tests
  :description "Tests for sexp-store"
  :depends-on (#:sexp-store)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:sexp-store/tests '#:run-tests)))
