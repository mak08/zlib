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
				 (make-string len :initial-element #\a))
				(compressed
				 (zlib:gzip s :level 9)))
		   (values 
			(length compressed)
			(length (zlib:gunzip compressed))))
				
1004
1000000
```
