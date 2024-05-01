;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-

(in-package :cl-user)

(defpackage #:cl-custom-hash-table-test
  (:use #:cl-custom-hash-table #:hu.dwim.stefil #:common-lisp)
  #+custom-hash-table-fallback
  (:import-from #:cl-custom-hash-table #:custom-hash-table)
  (:export #:run #:run-without-debugging))
