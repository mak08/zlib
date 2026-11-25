# zlib
CFFI bindings for zlib

## Installation

1. Open zlib.cl and adapt the zlib path if necessary.

```
...
(define-foreign-library libz
  (:linux "/path/to/libz.so.1"))
...
```
2. Load with ASDF

	```
	(asdf:load-system :zlib)
	```

## Example
```
CL-USER> (let* ((len 
				 1000000)
				(s 
				 (make-string len :initial-element #\a))) 
		   (time (let ((compressed
						(zlib:gzip s :level 9)))
				   (values 
					(length compressed)
					(length (zlib:gunzip compressed))))))
Evaluation took:
  0.021 seconds of real time
  0.021170 seconds of total run time (0.021170 user, 0.000000 system)
  100.00% CPU
  50,832,051 processor cycles
  1,000,016 bytes consed
  
1004
1000000
```

## Bounded byte-oriented streaming

The new API adds owned compression contexts while preserving the legacy gzip and
compression functions. It operates on `(vector (unsigned-byte 8))` data, without
text encoding or decoding. Callers own one context per independent stream; do not
use a context concurrently from multiple threads.

```lisp
(zlib:with-codec (compressor (zlib:make-deflater :format :raw))
  (let ((compressed (zlib:deflate-bytes compressor octets :flush :sync)))
    (zlib:with-codec (decompressor
                     (zlib:make-inflater :format :raw :max-output 1048576))
      (zlib:inflate-bytes decompressor compressed))))
```

| API | Meaning |
| --- | --- |
| `make-deflater &key format window-bits level max-output` | Defaults: `:raw`, 15, -1 (zlib default), 1048576 bytes. |
| `make-inflater &key format window-bits max-output` | Defaults: `:raw`, 15, 1048576 decompressed bytes. |
| `deflate-bytes context octets &key start end flush` | Consume the selected input range; return output octets and `:ok` or `:finished`. Flush defaults to `:sync`. |
| `inflate-bytes context octets &key start end finish allow-trailing` | Consume the selected range; return output octets and `:need-input` or `:finished`. The third value is input bytes consumed. `finish` and `allow-trailing` default to NIL. |
| `inflater-sync-point-p context` | Test an open inflater for the stored-block boundary before the four sync-flush length bytes. |
| `restart-inflater context` | Restart a finished raw inflater, retaining history and cumulative output count/limit. |
| `reset-codec context` | Reset dictionary, stream state and output count; preserve configuration and limit. |
| `close-codec context` | Release native resources; safe to repeat. |
| `with-codec (variable initialization) ...` | Ensure cleanup on normal return or error. |
| `codec-state context` | `:open`, `:finished`, `:failed` or `:closed`. |
| `codec-output-count context` | Cumulative output bytes since creation/reset. |

Formats are `:raw` (DEFLATE without headers/trailers), `:zlib`, and `:gzip`.
Inflation's `:gzip` mode retains the existing wrapper's auto-detection of gzip and
zlib headers. Window bits range from 9–15 for compression and 8–15 for inflation.
An 8-bit raw compressor window is rejected explicitly because zlib cannot provide
it. Compression levels range from -1 through 9.

`start`/`end` select a half-open input range, defaulting to the entire vector.
Chunks must fit zlib's unsigned 32-bit input-length field; larger inputs must be
split by the caller. Input is copied into temporary foreign storage. No pointers
to caller data or temporary storage remain in the context between calls.

### Flushes and message boundaries

- `:none` feeds input without forcing buffered compressed output to be emitted.
- `:sync` drains output to a byte-aligned boundary and retains dictionary history.
- `:finish` completes the DEFLATE stream. Further input requires reset/new context.

The returned sync-flushed bytes include the flush marker. There is no WebSocket
trailer removal/addition, RSV1 handling, or extension negotiation here; those
belong to PolarCL. For independent messages without context takeover, reset both
contexts between messages. Without reset, both sides retain compression history.

Inflation is incremental: `:need-input` means the submitted bytes were consumed,
not that a complete protocol message has been validated. An incomplete compressed
stream cannot generally be distinguished from one awaiting its next chunk. Use
`:finish t` when a complete final DEFLATE stream is required; this rejects
truncation. Sync-flushed messages have no final DEFLATE block, so do not use
`:finish t` for them. Bytes trailing a final stream are rejected; concatenated gzip
members are not automatically decoded by this API.

Protocol adapters may pass `:allow-trailing t` and use the third return value
(the number of bytes consumed from the selected range) to handle remaining input.
`restart-inflater` continues after a final raw DEFLATE block without discarding
history or resetting the output budget, as required for RFC 7692 messages that
contain final blocks. It accepts only a finished raw inflater. For sync-flush
protocols which omit the four length bytes, `inflater-sync-point-p` checks the
boundary **before** restoring those bytes; `:need-input` alone is insufficient.
These operations do not add WebSocket framing or validate entire messages.

### Limits, errors and cleanup

`max-output` is a nonnegative, cumulative byte limit across calls, not a per-chunk
limit. For an inflater it bounds decompressed output, so fragmentation cannot
bypass it. The implementation uses fixed-size foreign output buffers and checks
output before adding it to the result. It may probe one extra decompressed byte
to distinguish a stream exactly at the limit from one exceeding it. Returned
output and temporary result chunks together use up to roughly twice the permitted
output storage, plus input storage and zlib's internal working memory.

Exceeding the limit signals `zlib-output-limit`, a subtype of `zlib-error`.
Malformed/truncated input and native errors signal `zlib-error`; its
`zlib-error-code` is the native code when available, otherwise NIL. These failures
leave the context `:failed`. Reset it to discard the failed stream or close it.
Argument errors before processing do not consume input. Output from a failed call
is not returned; output delivered by earlier calls remains the caller's concern.

`close-codec` releases both zlib's internal allocations and the CFFI context
structure, even when abandoning an unfinished or failed stream. There is no GC
finalizer: use `with-codec` or explicitly close contexts.

## Tests

```lisp
(ql:quickload "zlib")
(asdf:test-system "zlib")
```

Tests cover raw/zlib/gzip formats, empty and multi-buffer inputs, incremental feeds,
sync/finish behavior, context reuse/reset, malformed/truncated input, trailing
bytes, exact and exceeded limits, legacy gzip, and an independent Python zlib peer.
Python 3 is required for the interoperability test; no network access is needed.
