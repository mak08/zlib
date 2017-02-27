;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    compress STRING to VECTOR, uncompress VECTOR to STRING
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2017-02-27 21:18:06>

(defpackage "ZLIB"
  (:use "COMMON-LISP" "CFFI")
  (:export "GZIP"
           "GUNZIP"
           "COMPRESS"
           "UNCOMPRESS"
           "ZLIB-VERSION"))

(in-package zlib)

(define-foreign-library libz
  (:linux "/lib/x86_64-linux-gnu/libz.so.1"))

(use-foreign-library libz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants

;; Flush mode
(defconstant +z_finish+ 4)

;; Return values
(defconstant +z_ok+ 0)
(defconstant +z_stream_end+ 1)
(defconstant +Z_NEED_DICT+     2)
(defconstant +Z_ERRNO+        -1)
(defconstant +Z_STREAM_ERROR+ -2)
(defconstant +Z_DATA_ERROR+   -3)
(defconstant +Z_MEM_ERROR+    -4)
(defconstant +Z_BUF_ERROR+    -5)
(defconstant +Z_VERSION_ERROR+ -6)

;; Compression strategy
(defconstant +Z_FILTERED+            1)
(defconstant +Z_HUFFMAN_ONLY+        2)
(defconstant +Z_RLE+                 3)
(defconstant +Z_FIXED+               4)
(defconstant +Z_DEFAULT_STRATEGY+    0)

;; Compression level
(defconstant +z_default_compression+ -1)
(defconstant +Z_BEST_SPEED+           1)
(defconstant +Z_BEST_COMPRESSION+     9)

;; Data_type
(defconstant +Z_BINARY+   0)
(defconstant +Z_TEXT+     1)
(defconstant +Z_ASCII+    1) ;  /* for compatibility with 1.2.2 and earlier */
(defconstant +Z_UNKNOWN+  2)

;; Compression Method
(defconstant +z_deflated+ 8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gzip

(defcfun ("zlibVersion" zlib-version) :string)

(defcfun ("deflateBound" deflate-bound)
    :unsigned-long
  (stream :pointer)
  (source-len :unsigned-long))

(defcstruct z-stream-s
  (next-in (:pointer :unsigned-char))
  (avail-in :unsigned-int)
  (total-in :unsigned-long)
  (next-out (:pointer :unsigned-char))
  (avail-out :unsigned-int)
  (total-out :unsigned-long)
  (msg (:pointer :unsigned-char))
  (interna-state-dummy (:pointer :int))
  (alloc-func :pointer)
  (free-func :pointer)
  (opaque :pointer)
  (data-type :int)
  (adler :unsigned-long)
  (reserved :unsigned-long))

(defcfun ("deflateInit_" deflate-init%)
    :int
  (stream :pointer)
  (level :int)
  (version :string)
  (stream-size :int))

(defun deflate-init (stream level)
  (with-foreign-string (version (zlib-version))
    (deflate-init% stream level version (foreign-type-size '(:struct z-stream-s)))))

(defcfun ("deflateInit2_" deflate-init2%)
    :int
  (stream :pointer)
  (level :int)
  (method :int)
  (window-bits :int)
  (memLevel :int)
  (strategy :int)
  (version :string)
  (stream_size :int))

(defun deflate-init2 (stream &key (level +z_default_compression+) (strategy +z_default_strategy+) (format :gzip))
  (let ((window-bits 15)
        (mem-level 8)
        (method +z_deflated+))
    (ecase format
      (:gzip
       (incf window-bits 16)))
    (with-foreign-string (version (zlib-version))
      (deflate-init2% stream
                      level
                      method
                      window-bits
                      mem-level
                      strategy
                      version
                      (foreign-type-size '(:struct z-stream-s))))))

(defcfun ("deflate" deflate) :int
  (stream :pointer)
  (flush :int))

(defcfun ("deflateEnd" deflate-end) :int
  (stream :pointer))

(defun gzip (string &key (level +z_default_compression+))
  (with-foreign-object (stream '(:struct z-stream-s))
    (setf (foreign-slot-value stream '(:struct z-stream-s) 'alloc-func) (null-pointer))
    (setf (foreign-slot-value stream '(:struct z-stream-s) 'free-func) (null-pointer))
    (setf (foreign-slot-value stream '(:struct z-stream-s) 'opaque) (null-pointer))
    (assert (= (deflate-init2 stream :level level) +z_ok+))
    (let* ((len-in (length string))
           (len-out (deflate-bound stream len-in)))
      (with-foreign-object
          (buf-in :unsigned-char len-in)
        (dotimes (i (length string))
          (setf (mem-ref buf-in :unsigned-char i) (char-code (aref string i))))
        (with-foreign-pointer
            (buf-out len-out)
          (setf (foreign-slot-value stream '(:struct z-stream-s) 'next-in) buf-in
                (foreign-slot-value stream '(:struct z-stream-s) 'avail-in) len-in
                (foreign-slot-value stream '(:struct z-stream-s) 'next-out) buf-out
                (foreign-slot-value stream '(:struct z-stream-s) 'avail-out) len-out)
          (assert (= (deflate stream +z_finish+) +z_stream_end+))
          (let* ((out-len (foreign-slot-value stream '(:struct z-stream-s) 'total-out))
                 (buffer (make-array out-len
                                     :element-type '(unsigned-byte 8))))
            (loop
               :for i :below out-len
               :do (setf (aref buffer i)
                         (mem-ref buf-out :unsigned-char i)))
            (deflate-end stream)
            buffer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gunzip

(defcfun ("inflateInit2_" inflate-init2%)
    :int
  (stream :pointer)
  (window-bits :int)
  (version :string)
  (stream-size :int))

(defun inflate-init2 (stream &key (format :gzip))
  ;; use maximum window-bits to avoid z_data_errors
  (let ((window-bits 15))
    (ecase format
      (:gzip
       (incf window-bits 32)))
    (inflate-init2% stream window-bits (zlib-version) (foreign-type-size '(:struct z-stream-s)))))

(defcfun ("inflate" inflate)
    :int
  (stream :pointer)
  (flush :int))

(defcfun ("inflateEnd" inflate-end)
    :int
  (stream :pointer))

(defun gunzip (octets)
  (with-foreign-object (stream '(:struct z-stream-s))
    (setf (foreign-slot-value stream '(:struct z-stream-s) 'alloc-func) (null-pointer))
    (setf (foreign-slot-value stream '(:struct z-stream-s) 'free-func) (null-pointer))
    (setf (foreign-slot-value stream '(:struct z-stream-s) 'opaque) (null-pointer))
    (assert (= (inflate-init2 stream :format :gzip) +z_ok+))
    (let* ((len-in (length octets))
           (len-out (octets-to-integer (subseq octets (- (length octets) 4)))))
      (with-foreign-object (buf-in :unsigned-char len-in)
        (loop
           :for i :from 0
           :for c :across octets
           :do (setf (mem-ref buf-in :unsigned-char i) c))
        (with-foreign-pointer
            (buf-out len-out)
          (setf (foreign-slot-value stream '(:struct z-stream-s) 'next-in) buf-in
                (foreign-slot-value stream '(:struct z-stream-s) 'avail-in) len-in
                (foreign-slot-value stream '(:struct z-stream-s) 'next-out) buf-out
                (foreign-slot-value stream '(:struct z-stream-s) 'avail-out) len-out)
          (assert (= (inflate stream +z_finish+) +z_stream_end+))
          (let* ((out-len (foreign-slot-value stream '(:struct z-stream-s) 'total-out))
                 (buffer (make-array out-len
                                     :element-type '(unsigned-byte 8))))
            (loop
               :for i :below out-len
               :do (setf (aref buffer i)
                         (mem-ref buf-out :unsigned-char i)))
            (inflate-end stream)
            buffer))))))

(defun octets-to-integer (octets &key (byte-order :BE))
  (let ((ub 0)
        (len (length octets)))
    (ecase byte-order
      (:be
       (loop
          :for i :below len
          :do (setf (ldb (byte 8 (* 8 i)) ub) (aref octets i))))
      (:le
       (loop
          :for i :below len
          :do (setf (ldb (byte 8 (* 8 (- len i))) ub) (aref octets i)))))
    (values ub len)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compress & uncompress

(defcfun ("compressBound" compress-bound)
    :unsigned-long
  (source-len :unsigned-long))

(defcfun ("compress" compress%)
    :int
  (dest :pointer)
  (dest-len (:pointer :long))
  (source :pointer)
  (source-len :long))

(defun compress (string)
  (let* ((length (length string))
         (buffer-size (compress-bound (length string))))
    (with-foreign-pointer (source (1+ length))
      (lisp-string-to-foreign string source (1+ length))
      (with-foreign-object (dest-len :long)
        (setf (mem-ref dest-len :long) buffer-size)
        (with-foreign-object (dest :unsigned-char buffer-size)
          (let* ((res (compress% dest dest-len source (1+ length)))
                 (out-len (mem-ref dest-len :long)))
            (if (= res +z_ok+)
                (let ((result (make-array out-len :element-type '(unsigned-byte 8))))
                  (loop
                     :for i :below out-len
                     :do (setf (aref result i)
                               (mem-ref dest :unsigned-char i)))
                  result)
                (values nil res))))))))

(defcfun ("uncompress" uncompress%)
    :int
  (dest :pointer)
  (dest-len (:pointer :long))
  (source :pointer)
  (source-len :long))

(defun uncompress (vector &key (uncompressed-length (* (length vector) 3)))
  (let* ((size (length vector)))
    (with-foreign-object (source :unsigned-char size)
      (loop
         :for i :from 0
         :for c :across vector
         :do (setf (mem-ref source :unsigned-char i) c))
      (with-foreign-object (dest-len :long)
        (setf (mem-ref dest-len :long) uncompressed-length)
        (with-foreign-object (dest :unsigned-char uncompressed-length)
          (let* ((res (uncompress% dest dest-len source size)))
            (if (= res +z_ok+)
                (foreign-string-to-lisp dest)
                (values nil res))))))))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
