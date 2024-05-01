(in-package :cl-user)

(load "/tmp/ql-dir/quicklisp.lisp")
(quicklisp-quickstart:install)

(load "/home/runner/work/cl-custom-hash-table/cl-custom-hash-table/cl-custom-hash-table.asd")
(load "/home/runner/work/cl-custom-hash-table/cl-custom-hash-table/cl-custom-hash-table-test.asd")

(ql:quickload :cl-custom-hash-table)
(ql:quickload :cl-custom-hash-table-test)

(asdf:oos 'asdf::load-op :cl-custom-hash-table :force t)
(asdf:oos 'asdf::load-op :cl-custom-hash-table-test :force t)

(cl-custom-hash-table-test:run-without-debugging)
