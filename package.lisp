(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((register-feature (feature present-p)
           (check-type feature keyword)
           (if present-p
               (pushnew feature *features*)
             (setf *features* (remove feature *features*)))))
    (register-feature :custom-hash-table-fallback
                      #+(or allegro ccl cmu lispworks sbcl) nil
                      #+ecl t
                      #-(or allegro ccl cmu lispworks sbcl ecl) (error "Unexpected implementation"))))

(defpackage #:cl-custom-hash-table
  (:use #:common-lisp)
  (:export #:define-custom-hash-table-constructor #:with-custom-hash-table)
  #+custom-hash-table-fallback
  (:export #:custom-hash-table)
  #+custom-hash-table-fallback
  (:shadow #:hash-table-p #:gethash #:remhash #:hash-table-count #:maphash
           #:with-hash-table-iterator #:clrhash #:hash-table-rehash-size
           #:hash-table-rehash-threshold #:hash-table-size))
