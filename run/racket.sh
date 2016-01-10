#!/bin/sh
exec racket -I scheme/init -l- r6rs/run.rkt ++path lib-runtime/racket ++path lib-stub/racket $*

