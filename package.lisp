;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  #-(or custom-hash-table-fallback custom-hash-table-native)
  (flet ((register-feature (feature present-p)
           (check-type feature keyword)
           (if present-p
               (pushnew feature *features*)
             (setf *features* (remove feature *features*)))))
    (let ((native (or #+(or allegro ccl cmu ecl lispworks sbcl) t)))
      (register-feature :custom-hash-table-native native)
      (register-feature :custom-hash-table-fallback (not native))))
  
  #+(and custom-hash-table-fallback custom-hash-table-native)
  (error "Cannot have both :CUSTOM-HASH-TABLE-NATIVE and :CUSTOM-HASH-TABLE-NATIVE in *FEATURES*"))


(defpackage #:cl-custom-hash-table
  (:use #:common-lisp)
  (:export #:define-custom-hash-table-constructor #:with-custom-hash-table)
  #+custom-hash-table-fallback
  (:export #:custom-hash-table)
  #+custom-hash-table-fallback
  (:shadow #:hash-table-p #:hash-table-test #:gethash #:remhash #:hash-table-count #:maphash
           #:with-hash-table-iterator #:clrhash #:hash-table-rehash-size
           #:hash-table-rehash-threshold #:hash-table-size))
