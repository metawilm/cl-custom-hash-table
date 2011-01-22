CL-CUSTOM-HASH-TABLE: Custom hash tables for Common Lisp
========================================================

Introduction
------------

This library allows creation and use of hash tables with arbitrary TEST/HASH functions,
in addition to the TEST functions allowed by the standard (EQ, EQL, EQUAL and EQUALP),
even in implementations that don't support this functionality directly.

Supported implementations
-------------------------

* Allegro CL 8.2
* Clozure CL 1.5
* CMUCL 20B
* ECL 11.1.1
* LispWorks 6.0
* SBCL 1.0.45

Example
-------

Custom TEST and HASH functions:

     (defun foo-equal-p (x y) (= x y))
     (defun foo-hash (x) (mod x 10))
    
Define the hash table type:

     (use-package :cl-custom-hash-table)

     (define-custom-hash-table-constructor make-foo-ht
        :test foo-equal-p :hash-function foo-hash)
    
Now MAKE-FOO-HT is a function that will create the custom hash table:

     (defparameter *foo-ht* (make-foo-ht)
       "Hash table using FOO-HASH and FOO-EQUAL-P")
    
You can trace your test/hash functions to check they are really getting called later:

     (trace foo-equal-p foo-hash)
    
Use WITH-CUSTOM-HASH-TABLE around access to the hash table.
This ensures functions GETHASH, REMHASH and MAPHASH do the right thing.
If you forget this, your code will not work in implementations 
that don't support custom TEST/HASH functions natively!
    
     (with-custom-hash-table
        (setf (gethash 1 *foo-ht*) 1
	      (gethash 10 *foo-ht*) 10
              (gethash 2 *foo-ht*) 2)
        (maphash (lambda (k v) 
                    (format t "~A: ~A~%" k v)
     	            (remhash k *foo-ht*))
                 *foo-ht*))

Implementation details
----------------------
    
Several Lisp implementations already support
custom TEST and HASH arguments for MAKE-HASH-TABLE.
This library is a small wrapper around that functionality.
(Allegro CL, CCL, CMUCL, LispWorks, SBCL) 

In other implementations a workaround is used, with an alternative
hash table implementation and a code walker that replaces GETHASH and friends
by custom functions. (ECL) 
