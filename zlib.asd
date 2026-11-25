;;; -*- lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert
;;; Created        22/03/2000 11:15:16
;;; Last Modified  <michael 2021-07-04 14:29:06>

(defsystem "zlib"
  :description "zlib bindings"
  :default-component-class cl-source-file.cl
  :depends-on ("cffi" "cl-utilities" "log2" "makros")
  :in-order-to ((test-op (test-op "zlib/tests")))
  :serial t
  :components ((:file "zlib") (:file "streaming")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsystem "zlib/tests"
  :depends-on ("zlib")
  :default-component-class cl-source-file.cl
  :components ((:file "tests/streaming-tests"))
  :perform (test-op (operation system)
             (declare (ignore operation system))
             (uiop:symbol-call :zlib-tests :run-tests)))
