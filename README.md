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
