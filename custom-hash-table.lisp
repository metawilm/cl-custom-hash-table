;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-CUSTOM-HASH-TABLE -*-

(in-package #:cl-custom-hash-table)

(defmacro checking-reader-conditionals (&whole whole &body body)
  "Break unless the body contains exactly one form. Inspired by code from Steve Haflich."
  (let ((num (length body)))
    (unless (= num 1)
      (break "A CHECKING-READER-CONDITIONALS expression returned ~r forms (instead of one): ~s"
             num whole))
    (car body)))

(defmacro define-custom-hash-table-constructor (make &key test hash-function)
  (check-type make symbol)
  (check-type test symbol)
  (check-type hash-function symbol)
  (let (#+cmu (hash-table-test-sym (intern (format nil "custom-hash-table ~A ~A" test hash-function)
                                           #.*package*)))
    `(progn #+cmu (extensions:define-hash-table-test ',hash-table-test-sym
                      (function ,test) (function ,hash-function))
            (defun ,make (&rest options)
              (checking-reader-conditionals
               #+(or allegro ccl lispworks)
               (apply #'make-hash-table :test ',test :hash-function ',hash-function options)
               #+cmu 
               (apply #'make-hash-table :test ',hash-table-test-sym options)
               #+sbcl
               (apply #'make-hash-table :test ',test :hash-function ',hash-function options)
               #+ecl
               (list :custom-hash-table
                     ',test
                     ',hash-function
                     (make-hash-table :test 'eql)))))))

(defmacro with-custom-hash-table (&body body)
  #-custom-hash-table-fallback
  `(progn ,@body)
  #+custom-hash-table-fallback
  `(progn ,@(walk-code body)))

#+custom-hash-table-fallback
(progn

(defun walk-code (body)
  "Replace hash operators by their custom variant"
  ;; Don't destructively modify original source conses
  (setf body (copy-tree body))
  (loop for custom-sym in '(gethash remhash hash-table-count maphash
                            with-hash-table-iterator clrhash hash-table-rehash-size
                            hash-table-rehash-threshold hash-table-size)
      for cl-sym = (find-symbol (symbol-name custom-sym) '#:common-lisp)
      do (setf body (nsubst custom-sym cl-sym body))
      finally (return body)))

(deftype custom-hash-table ()
  '(satisfies custom-hash-table-fallback-p))

(defun custom-hash-table-fallback-p (x)
  (and (consp x) (eq (car x) :custom-hash-table)))

(defun gethash (key ht &optional default)
  (etypecase ht
    (hash-table (cl:gethash key ht default))
    (custom-hash-table (pop ht)
                       (let* ((test-fn (pop ht))
                              (hash-fn (pop ht))
                              (real-ht (pop ht))
                              (key.hash (funcall hash-fn key))
                              (existing-values (gethash key.hash real-ht)))
                         (loop for x-and-val in existing-values
                             for (x-key . x-val) = x-and-val
                             when (funcall test-fn key x-key)
                             do (return-from gethash (values x-val t))
                             finally (return (values default nil)))))))

(defun (setf gethash) (new-val key ht &optional default)
  (etypecase ht
    (hash-table (setf (cl:gethash key ht default) new-val))
    (custom-hash-table (pop ht)
                       (let* ((test-fn (pop ht))
                              (hash-fn (pop ht))
                              (real-ht (pop ht))
                              (key.hash (funcall hash-fn key))
                              (existing-values (gethash key.hash real-ht)))
                         (loop for x-and-val in existing-values
                             for (x-key . nil) = x-and-val
                             when (funcall test-fn key x-key)
                             do (progn (setf (cdr x-and-val) new-val) (return-from gethash))
                             finally (push (cons key new-val) (gethash key.hash real-ht)))))))

(defun remhash (key ht)
  (etypecase ht
    (hash-table (cl:remhash key ht))
    (custom-hash-table (pop ht)
                       (let* ((test-fn (pop ht))
                              (hash-fn (pop ht))
                              (real-ht (pop ht))
                              (key.hash (funcall hash-fn key))
                              (existing-values (gethash key.hash real-ht)))
                         (loop for x-and-val in existing-values
                             for (x-key . nil) = x-and-val
                             when (funcall test-fn key x-key)
                             do (let ((new-val (delete x-and-val existing-values)))
                                  (if new-val
                                      (setf (gethash key.hash real-ht) new-val)
                                    (remhash key.hash real-ht)))
                                (return-from remhash t)
                             finally (return nil))))))

(defun hash-table-count (ht)
  (etypecase ht
    (hash-table (cl:hash-table-count ht))
    (custom-hash-table (loop with real-ht = (fourth ht)
                           for entries being each hash-value in real-ht
                           sum (length entries)))))

(defun maphash (function ht)
  ;; When changing, ensure remhash and (setf gethash) on current key are supported.
  (etypecase ht
    (hash-table (cl:maphash function ht))
    (custom-hash-table (loop with real-ht = (fourth ht)
                           for entries being each hash-value in real-ht
                           do (loop while entries
                                  do (destructuring-bind (k . v) 
                                         (pop entries)
                                       (funcall function k v)))))))

(defmacro with-gensyms (list &body body)
  `(let ,(loop for x in list
             collect `(,x (gensym ,(symbol-name x))))
     ,@body))

(defmacro with-hash-table-iterator ((name hash-table) &body body)
  (with-gensyms (ht real-ht real-iter current-key-val-list k v entry-p real-key real-val)
    `(let ((,ht ,hash-table))
       (etypecase ,ht
         (hash-table (cl:with-hash-table-iterator (,name ,ht) ,@body))
         (custom-hash-table
          (let ((,real-ht (fourth ,ht)))
            (cl:with-hash-table-iterator (,real-iter ,real-ht)
              (let (,current-key-val-list)
                (macrolet
                    ((,name () '(loop named ,name
                                 do (if ,current-key-val-list
                                        (destructuring-bind (,k . ,v)
                                            (pop ,current-key-val-list)
                                          (return-from ,name (values t ,k ,v)))
                                      (multiple-value-bind (,entry-p ,real-key ,real-val)
                                          (,real-iter)
                                        (declare (ignore ,real-key))
                                        (if ,entry-p
                                            (setf ,current-key-val-list ,real-val)
                                          (return-from ,name nil)))))))
                  ,@body)))))))))

(defun clrhash (ht)
  (etypecase ht
    (hash-table (cl:clrhash ht))
    (custom-hash-table (cl:clrhash (fourth ht)))))

(defun hash-table-rehash-size (ht)
  (etypecase ht
    (hash-table (cl:hash-table-rehash-size ht))
    (custom-hash-table (cl:hash-table-rehash-size (fourth ht)))))

(defun hash-table-rehash-threshold (ht)
  (etypecase ht
    (hash-table (cl:hash-table-rehash-threshold ht))
    (custom-hash-table (cl:hash-table-rehash-threshold (fourth ht)))))

(defun hash-table-size (ht)
  (etypecase ht
    (hash-table (cl:hash-table-size ht))
    (custom-hash-table (cl:hash-table-size (fourth ht)))))

) ;; #+