SIBR0004: include may be implemented as a library syntax
========================================================

Affected: `Picrin`

In `Picrin`, `include` is implemented as a library syntax.

Workaround
==========

Yuni does not use R7RS library feature. 

Yuni build script reorders generated R7RS library stub.

Reference
=========

* https://github.com/okuoku/yuni/issues/5
