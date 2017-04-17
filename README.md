CL-CUSTOM-HASH-TABLE: Custom hash tables for Common Lisp
========================================================

Introduction
------------

CL-CUSTOM-HASH-TABLE allows creation and use of "custom hash tables".
Custom hash tables can use arbitrary TEST/HASH functions,
in addition to the TEST functions allowed by the standard
(EQ, EQL, EQUAL and EQUALP).

This library is primarily a compatibility layer, unifying how to create these hash table in different Lisp implementations. Furthermore this library provides a simple yet fully functional fall-back solution for implementations that don't support this functionality natively (yet).

License
-------

CL-CUSTOM-HASH-TABLE is released under a [BSD-like license](http://www.opensource.org/licenses/bsd-license.php), see the ASD file.

Compatibility
-------------

This library does not shadow symbols in the COMMON-LISP package. It does require that all access to (potential) custom hash tables is lexical wrapped in a WITH-CUSTOM-HASH-TABLE form (see example below).

The standard hash table related functions are **supported**:

* get/set: GETHASH, REMHASH, CLRHASH;
* iteration: WITH-HASH-TABLE-ITERATOR, MAPHASH;
* statistics: HASH-TABLE-COUNT, HASH-TABLE-REHASH-SIZE, HASH-TABLE-REHASH-THRESHOLD, HASH-TABLE-SIZE.

Hash table iteration using LOOP (using HASH-KEY or HASH-VALUE) is **not supported** in Lisp implementations where the fall-back solution is used, therefore cannot be used in core that is supposed to be portable.

In the fall-back solution HASH-TABLE-COUNT returns the correct number of entries, but HASH-TABLE-SIZE returns the size of the underlying helper hash table which might be lower than HASH-TABLE-COUNT. Functions HASH-TABLE-REHASH-SIZE and HASH-TABLE-REHASH-THRESHOLD also refer to that helper hash table.

The fall-back solution is not thread-safe. The native implementation may or may not be.

Supported implementations
-------------------------

See the [build status](https://travis-ci.org/metawilm/cl-custom-hash-table) on Travis-CI, and the [coverage status](https://coveralls.io/github/metawilm/cl-custom-hash-table?branch=master) on Coveralls.

| Common Lisp Implementation | Native | Fallback |
|:-:|:-:|:-:|
| [ABCL](https://common-lisp.net/project/armedbear/) | n/a                                                                                                                                                                       | ![Build Status](https://travis-build-job-badge.herokuapp.com/badge?user=metawilm&repo=cl-custom-hash-table&branch=master&envContains=abcl+FALLBACK&label=ABCL+fallback) |
| [Allegro CL](http://franz.com/products/allegrocl/) | ![Build Status](https://travis-build-job-badge.herokuapp.com/badge?user=metawilm&repo=cl-custom-hash-table&branch=master&envContains=allegro+NATIVE&label=Allegro+native) | ![Build Status](https://travis-build-job-badge.herokuapp.com/badge?user=metawilm&repo=cl-custom-hash-table&branch=master&envContains=allegro+FALLBACK&label=Allegro+fallback) |
| [Clozure CL](http://clozure.com/clozurecl.html)    | ![Build Status](https://travis-build-job-badge.herokuapp.com/badge?user=metawilm&repo=cl-custom-hash-table&branch=master&envContains=ccl+NATIVE&label=CCL+native)         | ![Build Status](https://travis-build-job-badge.herokuapp.com/badge?user=metawilm&repo=cl-custom-hash-table&branch=master&envContains=ccl+FALLBACK&label=CCL+fallback) |
| [CLISP](http://clisp.sourceforge.net)              | n/a                                                                                                                                                                       | ![Build Status](https://travis-build-job-badge.herokuapp.com/badge?user=metawilm&repo=cl-custom-hash-table&branch=master&envContains=clisp+FALLBACK&label=CCL+fallback) |
| [CMUCL](http://www.cons.org/cmucl/)                | ? | ? |
| [ECL](http://ecls.sourceforge.net/)                | n/a                                                                                                                                                                       | ![Build Status](https://travis-build-job-badge.herokuapp.com/badge?user=metawilm&repo=cl-custom-hash-table&branch=master&envContains=ecl+FALLBACK&label=ECL+fallback) |
| [LispWorks](http://www.lispworks.com/)             | ? | ? |
| [SBCL](http://sbcl.sourceforge.net/)               | ![Build Status](https://travis-build-job-badge.herokuapp.com/badge?user=metawilm&repo=cl-custom-hash-table&branch=master&envContains=sbcl+NATIVE&label=SBCL+native)       | ![Build Status](https://travis-build-job-badge.herokuapp.com/badge?user=metawilm&repo=cl-custom-hash-table&branch=master&envContains=sbcl+FALLBACK&label=SBCL+fallback) ![Coverage Status](https://coveralls.io/repos/github/metawilm/cl-custom-hash-table/badge.svg?branch=master) |

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
This library is a small wrapper around the vendor-specific extensions.
(Allegro CL, CCL, CMUCL, LispWorks, SBCL) 

In other Lisp implementations (ECL) a fall-back solution is used:

* custom hash tables are created on top of standard hash tables;
* the WITH-CUSTOM-HASH-TABLE code walker replaces GETHASH and friends by custom functions that work on both standard and "custom" hash tables.

How does this compare to [genhash](http://www.cliki.net/genhash)?
----------------------------------

* genhash is complete hash table implementation; CL-CUSTOM-HASH-TABLE is primarily a compatibility layer, and offers a simple fall-back solution built on top of standard hash tables.
* genhash comes with its own API; CL-CUSTOM-HASH-TABLE uses the standard hash table API.