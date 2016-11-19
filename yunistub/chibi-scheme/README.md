yuniFFI module for chibi-scheme
===============================

Compile with

 chibi-ffi yuniffi.stub.c
 cc -L/usr/local/bin -shared yuniffi.stub.c yuniffi_stub.c -lchibi-scheme -o chibi-yuniffi.dll

(obviously, Linux/BSD should use /usr/local/lib etc.)
