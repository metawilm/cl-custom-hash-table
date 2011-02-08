;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-CUSTOM-HASH-TABLE.TEST -*-

(in-package :cl-user)

(defpackage #:cl-custom-hash-table.test
  (:use #:cl-custom-hash-table #:common-lisp)
  #+custom-hash-table-fallback
  (:import-from #:cl-custom-hash-table #:custom-hash-table)
  (:export #:run))

(in-package :cl-custom-hash-table.test)

(defvar *foo-equal-count*)

(defun foo-equal-p (x y)
  (when (boundp '*foo-equal-count*)
    (incf *foo-equal-count*))
  (= x y))

(defun foo-hash (x)
  (mod x 10))

(define-custom-hash-table-constructor make-foo-ht
    :test foo-equal-p :hash-function foo-hash)

(defparameter *foo-ht* nil
  "Hash table using FOO-HASH and FOO-EQUAL-P")

(defun run () 
  (setf *foo-ht* (make-foo-ht))
  #+custom-hash-table-fallback
  (progn (assert (typep *foo-ht* 'custom-hash-table))
         (assert (not (typep (make-hash-table) 'custom-hash-table))))
  (with-custom-hash-table      
    (setf (gethash 1 *foo-ht*) 1
          (gethash 10 *foo-ht*) 10
          (gethash 20 *foo-ht*) 20)
    ;; (1 . 1) (10 . 10) (20 . 20)
    (let ((*foo-equal-count* 0))
      (assert (= (gethash 1 *foo-ht*) 1))
      (assert (plusp *foo-equal-count*)))
    (let ((*foo-equal-count* 0))
      (assert (null (gethash 30 *foo-ht*)))
      #+custom-hash-table-fallback
      (assert (= 2 *foo-equal-count*))) ;; tested 10, 20
    (assert (remhash 1 *foo-ht*))
    ;; (10 . 10) (20 . 20)
    (assert (not (remhash 1 *foo-ht*)))
    (assert (= (hash-table-count *foo-ht*) 2))
    (setf (gethash 1 *foo-ht*) 1)
    (let ((expected '((1 . 1) (10 . 10) (20 . 20))))
      (flet ((expected-p (set)
               (and (null (set-difference set expected :test 'equal))
                    (null (set-difference expected set :test 'equal)))))
        (let (items)
          (maphash (lambda (k v) (push (cons k v) items)) *foo-ht*)
          (assert (expected-p items)))
        (block iter-test
          (let (items)
            (with-hash-table-iterator (next *foo-ht*)
              (multiple-value-bind (entry-p key val) (next)
                (if entry-p
                    (push (cons key val) items)
                  (progn (assert (expected-p items))
                         (return-from iter-test)))))))))
    (clrhash *foo-ht*)
    (assert (zerop (hash-table-count *foo-ht*)))
    (assert (plusp (hash-table-rehash-size *foo-ht*)))
    (assert (plusp (hash-table-rehash-threshold *foo-ht*)))
    (assert (plusp (hash-table-size *foo-ht*))))
  (format t "Test success!~%")
  t)

(run)