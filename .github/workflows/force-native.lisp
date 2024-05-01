(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *features*
    (cons :custom-hash-table-fallback
          (remove :custom-hash-table-native *features*))))
