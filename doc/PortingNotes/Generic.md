Generic-Scheme runtime (generic runtime)
========================================

Generic-Scheme runtime is a Yuni runtime that intended to run on "plain" or
"generic" Scheme interpreters that does not come with `syntax-rules` macro
support.

Generic runtime is STILL IN PROGRESS and not intended for any production use.

Unlike runtimes implemented on R5RS/R6RS/R7RS, generic runtime has several
limitations as described below. Consider it as a subset for Yuni on RnRS;
program/libraries written for generic runtime is still compatible with Yuni
on RnRS.

Licensing
---------

Currently, generic runtime uses [chibi-scheme][]'s `syntax-rules` 
implementation. Thus any program distributions must follow its license terms.

Macro
-----

The largest difference compared with R5RS/R6RS/R7RS is macro facility does not
provide any Hygiene -- in generic runtime, `syntax-rules` implemented on 
`define-macro`.

TBD: (Document gensym keyword `__1` here.)

[chibi-scheme]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/chibi-scheme.md

