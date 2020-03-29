SIBR0009: Keyword syntax may be enabled by default
==================================================

Affected: `Gauche` (< 0.9.8) `Sagittarius`

Some Scheme implementation would enable common-lisp styled keyword syntax by
default. It would result colon(:) prefixed symbols may not be read as symbols.

Gauche 0.9.8 changed default behaviour that we can use keywords as same as 
symbols.


Workaround
==========

Yuni never use colon prefixed symbols except aux-keywords. New libraries should
never add colon prefixed symbols as exports.

For these implementations, Yuni build script removes colon prefixed symbols
and use them as keywords.

Reference
=========

* SRFI-88
* https://github.com/shirok/Gauche/commit/5b968f158e42b5d4575bebb175790d0e570d0fa0 -- Gauche 0.9.8 made `GAUCHE_KEYWORD_IS_SYMBOL` default
