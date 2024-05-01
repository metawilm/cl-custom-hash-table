;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-CUSTOM-HASH-TABLE-TEST -*-

(in-package :cl-custom-hash-table-test)

(defsuite* test-suite)

#+custom-hash-table-fallback
(format t "Using FALLBACK custom hash table implementation.~%")
#+custom-hash-table-native
(format t "Using NATIVE custom hash table implementation.~%")

(defvar *foo-equal-count*)

(defun foo-equal-p (x y)
  (when (boundp '*foo-equal-count*)
    (incf *foo-equal-count*))
  (= x y))

(defun foo-hash (x)
  (mod x 10))

(define-custom-hash-table-constructor make-foo-ht
    :test foo-equal-p :hash-function foo-hash)

(deftest basic-test ()
  
  (let ((foo-ht (make-foo-ht))
        (normal-ht (make-hash-table :test 'eql)))
    
    (is (typep normal-ht 'hash-table))
    
    #+custom-hash-table-fallback
    (progn (is (typep foo-ht 'custom-hash-table))
           (is (not (typep normal-ht 'custom-hash-table))))
    
    #+custom-hash-table-native
    (is (typep foo-ht 'hash-table))

    (with-custom-hash-table
      #+custom-hash-table-native  
      (is (member (hash-table-test foo-ht) (list 'foo-equal-p #'foo-equal-p)))
      #+custom-hash-table-fallback
      (is (eq (hash-table-test foo-ht) 'foo-equal-p))

      #-clisp ;; Does not return 'eql : http://www.clisp.org/impnotes/ht-test.html
      (is (eq (hash-table-test normal-ht) 'eql)))
    
    (dolist (ht (list foo-ht normal-ht))
      (let ((is-custom (eq ht foo-ht)))
        
      (with-custom-hash-table

        (is (hash-table-p ht))
          
        (setf (gethash 1 ht) 1
              (gethash 10 ht) 10
              (gethash 20 ht) 20)
        ;; (1 . 1) (10 . 10) (20 . 20)
        (let ((*foo-equal-count* 0))
          (is (= (gethash 1 ht) 1))
          (when is-custom
            (is (plusp *foo-equal-count*))))
        (let ((*foo-equal-count* 0))
          (is (null (gethash 30 ht)))
          (when is-custom
            #+custom-hash-table-native
            (is (<= 2 *foo-equal-count*)) ;; tested 10, 20, possibly more
            #+custom-hash-table-fallback
            (is (= 2 *foo-equal-count*)))) ;; tested 10, 20
        (is (remhash 1 ht))
        ;; (10 . 10) (20 . 20)
        (is (not (remhash 1 ht)))
        (is (= (hash-table-count ht) 2))
        (setf (gethash 1 ht) 1)
        (let ((expected '((1 . 1) (10 . 10) (20 . 20))))
          (flet ((expected-p (set)
                   (and (null (set-difference set expected :test 'equal))
                        (null (set-difference expected set :test 'equal)))))
            ;; Check MAPHASH
            (let (items)
              (maphash (lambda (k v) (push (cons k v) items)) ht)
              (is (expected-p items)))
            ;; Check WITH-HASH-TABLE-ITERATOR
            (let (items)
              (with-hash-table-iterator (next ht)
                (loop named iter-test
                    do (multiple-value-bind (entry-p key val) (next)
                         (if entry-p
                             (push (cons key val) items)
                           (return-from iter-test)))))
              (is (expected-p items)))))
      
        (clrhash ht)
        (is (zerop (hash-table-count ht)))
        (is (plusp (hash-table-rehash-size ht)))
        (is (plusp (hash-table-rehash-threshold ht)))
        (is (not (minusp (hash-table-size ht))))
      
        (dotimes (i 1000)
          (setf (gethash i ht) (* 2 i))
          (is (= (hash-table-count ht) (1+ i))))
        ;; HT: {1 -> 2; 2 -> 4; ...; 999 -> 1998}
      
        (loop for i from 400 to 600
            do (is (= (gethash i ht) (* 2 i)))
               (setf (gethash i ht) (* 3 i)))
        ;; HT: {1 -> 2; 399 -> 798;  400 -> 1200; ...; 600 -> 1800;  601 -> 1202; ...; 999 -> 1998}
      
        (is (= (hash-table-count ht) 1000))
        (dotimes (i 1000)
          (is (= (gethash i ht) (if (<= 400 i 600) (* 3 i) (* 2 i)))))
      
        (dotimes (i 1000)
          (is (remhash i ht))
          (is (not (remhash (+ i 1000) ht)))
          (is (= (hash-table-count ht) (- 999 i))))))))
  
  (signals error (eval '(with-custom-hash-table 
                         (loop for x being the hash-key in (make-hash-table)
                             do nil))))
  (signals error (eval '(with-custom-hash-table 
                         (loop for x being the hash-keys in (make-hash-table)
                             do nil))))
  (signals error (eval '(with-custom-hash-table 
                         (loop for x being the :hash-value in (make-hash-table)
                             do nil))))
  (signals error (eval '(with-custom-hash-table 
                         (loop for x being the #:hash-values in (make-hash-table)
                             do nil))))
  t)

(defun test-failures-p ()
  (when (plusp #1=(length (hu.dwim.stefil::failure-descriptions-of *last-test-result*)))
    #1#))

(defun run-without-debugging ()
  (without-debugging (run))
  (if #1=(test-failures-p)
      (error "There were ~d test failures" #1#)
    (format t "No test failures.~%")))

(defun run ()
  (prog1 (basic-test)
    (if #1=(test-failures-p)
        (format t "~%There were ~d test failures~%" #1#)
      (format t "~%Test success!~%"))))
