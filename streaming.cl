;;; Owned, bounded, byte-oriented zlib contexts. No WebSocket wire conventions.
(in-package :zlib)

(defcfun ("deflateReset" deflate-reset) :int (stream :pointer))
(defcfun ("inflateSyncPoint" inflate-sync-point) :int (stream :pointer))
(defcfun ("inflateResetKeep" inflate-reset-keep) :int (stream :pointer))
(defcfun ("inflateReset" inflate-reset) :int (stream :pointer))

(define-condition zlib-error (error)
  ((code :reader zlib-error-code :initarg :code :initform nil)
   (operation :initarg :operation :reader zlib-error-operation)
   (message :initarg :message :initform "" :reader zlib-error-message))
  (:report (lambda (condition stream)
             (format stream "zlib ~a failed~@[ (~d)~]: ~a"
                     (zlib-error-operation condition) (zlib-error-code condition)
                     (zlib-error-message condition)))))
(define-condition zlib-output-limit (zlib-error) ())

(defclass codec ()
  ((pointer :initarg :pointer :accessor codec-pointer)
   (direction :initarg :direction :reader codec-direction)
   (format :initarg :format :reader codec-format)
   (state :initform :open :reader codec-state :accessor %codec-state)
   (limit :initarg :limit :reader codec-limit)
   (output-count :initform 0 :reader codec-output-count :accessor %codec-output-count)))

(defun codec-error (codec operation code message &optional (type 'zlib-error))
  (setf (%codec-state codec) :failed)
  (error type :operation operation :code code :message message))

(defun make-codec (direction format window-bits level max-output)
  (check-type max-output (integer 0))
  (let ((pointer (foreign-alloc '(:struct z-stream-s))) (initialized nil) (owned nil))
    (unwind-protect
         (progn
           (dotimes (i (foreign-type-size '(:struct z-stream-s)))
             (setf (mem-aref pointer :uint8 i) 0))
           (let ((result (ecase direction
                           (:deflate (deflate-init2 pointer :format format :window-bits window-bits :level level))
                           (:inflate (inflate-init2 pointer :format format :window-bits window-bits)))))
             (unless (= result +z_ok+)
               (error 'zlib-error :operation :initialize :code result :message "Cannot initialize codec"))
             (setf initialized t))
           (prog1 (make-instance 'codec :pointer pointer :direction direction :format format :limit max-output)
             (setf owned t)))
      (unless owned
        (when initialized (if (eq direction :deflate) (deflate-end pointer) (inflate-end pointer)))
        (foreign-free pointer)))))

(defun make-deflater (&key (format :raw) (window-bits 15) (level -1) (max-output 1048576))
  "Create a compressor. MAX-OUTPUT bounds cumulative output until RESET-CODEC."
  (check-type level (integer -1 9))
  (make-codec :deflate format window-bits level max-output))
(defun make-inflater (&key (format :raw) (window-bits 15) (max-output 1048576))
  "Create a decompressor with a cumulative decompressed-byte limit."
  (make-codec :inflate format window-bits -1 max-output))

(defun close-codec (codec)
  "Release internal zlib resources and the CFFI structure; safe to repeat."
  (unless (eq (codec-state codec) :closed)
    (unwind-protect
         ;; deflateEnd may report DATA_ERROR when abandoning an unfinished stream.
         ;; That does not prevent it from releasing its allocated resources.
         (if (eq (codec-direction codec) :deflate)
             (deflate-end (codec-pointer codec)) (inflate-end (codec-pointer codec)))
      (foreign-free (codec-pointer codec))
      (setf (codec-pointer codec) (null-pointer) (%codec-state codec) :closed)))
  nil)
(defmacro with-codec ((name initialization) &body body)
  `(let ((,name ,initialization))
     (unwind-protect (progn ,@body) (close-codec ,name))))

(defun reset-codec (codec)
  "Reset history, stream state and output accounting; preserve configuration.
Also recover a failed or finished codec. A closed codec cannot be reset."
  (when (eq (codec-state codec) :closed) (error "Cannot reset a closed codec."))
  (let ((result (if (eq (codec-direction codec) :deflate)
                    (deflate-reset (codec-pointer codec)) (inflate-reset (codec-pointer codec)))))
    (unless (= result +z_ok+) (codec-error codec :reset result "Cannot reset codec")))
  (setf (%codec-state codec) :open (%codec-output-count codec) 0)
  codec)

(defun process-codec (codec octets start end flush require-finish &optional allow-trailing)
  (unless (eq (codec-state codec) :open) (error "Codec is ~s; reset or create a new codec." (codec-state codec)))
  (check-type octets (vector (unsigned-byte 8)))
  (check-type start (integer 0)) (check-type end (integer 0))
  (unless (<= start end (length octets)) (error "Invalid input range."))
  ;; zlib avail_in is uInt. Larger data must be submitted in separate calls.
  (when (> (- end start) #xffffffff) (error "Input chunk exceeds zlib's uInt range."))
  (let ((pointer (codec-pointer codec)) (parts nil) (size 0)
        (compressing (eq (codec-direction codec) :deflate)) (successful nil) (consumed 0))
    (unwind-protect
         (with-foreign-objects ((input :uint8 (max 1 (- end start))) (output :uint8 16384))
           (dotimes (i (- end start)) (setf (mem-aref input :uint8 i) (aref octets (+ start i))))
           (setf (foreign-slot-value pointer '(:struct z-stream-s) 'next-in) input
                 (foreign-slot-value pointer '(:struct z-stream-s) 'avail-in) (- end start))
           (let ((status
                   (loop
                     for remaining = (- (codec-limit codec) (codec-output-count codec))
                     ;; One probe byte distinguishes exactly-at-limit from overflow,
                     ;; including when the last bytes only finish a compressed block.
                     for capacity = (if compressing 16384 (min 16384 (1+ remaining)))
                     for before = (foreign-slot-value pointer '(:struct z-stream-s) 'avail-in)
                     for result = (progn
                                    (setf (foreign-slot-value pointer '(:struct z-stream-s) 'next-out) output
                                          (foreign-slot-value pointer '(:struct z-stream-s) 'avail-out) capacity)
                                    (if compressing (deflate pointer flush) (inflate pointer +z_no_flush+)))
                     for produced = (- capacity (foreign-slot-value pointer '(:struct z-stream-s) 'avail-out))
                     for left = (foreign-slot-value pointer '(:struct z-stream-s) 'avail-in)
                     do
                       (unless (member result (list +z_ok+ +z_stream_end+ +z_buf_error+))
                         (codec-error codec :process result "Invalid compressed data or codec failure"))
                       (when (> produced remaining)
                         (codec-error codec :process nil "Output byte limit exceeded" 'zlib-output-limit))
                       (when (plusp produced)
                         (let ((part (make-array produced :element-type '(unsigned-byte 8))))
                           (dotimes (i produced) (setf (aref part i) (mem-aref output :uint8 i)))
                           (push part parts))
                         (incf size produced) (incf (%codec-output-count codec) produced))
                       (when (= result +z_stream_end+)
                         (unless (or allow-trailing (zerop left)) (codec-error codec :process nil "Trailing data after stream end"))
                         (setf (%codec-state codec) :finished)
                         (return :finished))
                       (when (and (zerop left) (< produced capacity))
                         (when require-finish (codec-error codec :finish nil "Truncated compressed stream"))
                         (return (if compressing :ok :need-input)))
                       (when (and (= left before) (zerop produced))
                         (codec-error codec :process result "Codec made no progress")))))
             (let ((bytes (make-array size :element-type '(unsigned-byte 8))) (offset 0))
               (dolist (part (nreverse parts))
                 (replace bytes part :start1 offset) (incf offset (length part)))
               (setf consumed (- end start (foreign-slot-value pointer '(:struct z-stream-s) 'avail-in))
                     successful t)
               (values bytes status consumed))))
      ;; No dangling pointers to temporary foreign storage survive a call.
      (setf (foreign-slot-value pointer '(:struct z-stream-s) 'next-in) (null-pointer)
            (foreign-slot-value pointer '(:struct z-stream-s) 'avail-in) 0
            (foreign-slot-value pointer '(:struct z-stream-s) 'next-out) (null-pointer)
            (foreign-slot-value pointer '(:struct z-stream-s) 'avail-out) 0)
      (unless successful (setf (%codec-state codec) :failed)))))

(defun deflate-bytes (codec octets &key (start 0) (end (length octets)) (flush :sync))
  "Consume OCTETS; return output octets and :OK or :FINISHED.
FLUSH is :NONE, :SYNC (byte-aligned, retaining history), or :FINISH. Sync output
includes the DEFLATE flush marker; protocol-specific stripping belongs to callers."
  (unless (eq (codec-direction codec) :deflate) (error "Expected a deflater."))
  (process-codec codec octets start end
                 (ecase flush (:none +z_no_flush+) (:sync +z_sync_flush+) (:finish +z_finish+))
                 (eq flush :finish)))
(defun inflate-bytes (codec octets &key (start 0) (end (length octets)) finish allow-trailing)
  "Consume OCTETS; return decompressed octets and :NEED-INPUT or :FINISHED.
FINISH requires a final DEFLATE block and rejects truncation. Sync-flushed data is
not a final stream. A third value counts consumed input bytes. ALLOW-TRAILING
leaves bytes after a final block unconsumed instead of rejecting them. Output
limits are cumulative across calls until RESET-CODEC."
  (unless (eq (codec-direction codec) :inflate) (error "Expected an inflater."))
  (process-codec codec octets start end +z_no_flush+ finish allow-trailing))

(defun inflater-sync-point-p (codec)
  "True at a byte-aligned stored-block header, before its length bytes.
This tests the boundary used by protocols which remove the four sync-flush bytes."
  (unless (and (eq (codec-direction codec) :inflate) (eq (codec-state codec) :open))
    (error "Expected an open inflater."))
  (let ((result (inflate-sync-point (codec-pointer codec))))
    (when (minusp result) (codec-error codec :sync-point result "Cannot inspect inflater"))
    (= result 1)))

(defun restart-inflater (codec)
  "Continue after a final raw DEFLATE block, retaining history and output budget.
Unlike RESET-CODEC, this does not begin a new independent message."
  (unless (and (eq (codec-direction codec) :inflate) (eq (codec-format codec) :raw)
               (eq (codec-state codec) :finished))
    (error "Expected a finished raw inflater."))
  (let ((result (inflate-reset-keep (codec-pointer codec))))
    (unless (= result +z_ok+) (codec-error codec :restart result "Cannot restart inflater")))
  (setf (%codec-state codec) :open)
  codec)
