;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
(in-package :cl-user)

(asdf:defsystem #:cl-custom-hash-table-test
    :name "CL-CUSTOM-HASH-TABLE-TEST: Test for CL-CUSTOM-HASH-TABLE"
    :depends-on (:hu.dwim.stefil :cl-custom-hash-table)
    :components ((:file "test-package")
                 (:file "test-suite" :depends-on ("test-package"))))