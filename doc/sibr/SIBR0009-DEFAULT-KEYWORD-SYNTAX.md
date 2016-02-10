SIBR0009: Keyword syntax may be enabled by default
==================================================

Affected: `Gauche` `Sagittarius`

Some Scheme implementation would enable common-lisp styled keyword syntax by
default. It would result colon(:) prefixed symbols may not be read as symbols.


Workaround
==========

Yuni never use colon prefixed symbols except aux-keywords. New libraries should
never add colon prefixed symbols as exports.

For these implementations, Yuni build script removes colon prefixed symbols
and use them as keywords.

Reference
=========

* SRFI-88

