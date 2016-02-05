SIBR0005: C style include path behaviour
========================================

Affected: `Gauche` `Picrin`

Some R7RS implementations does not implement C-styled include path behaviour.

For example, if the library was organised as:

```
 common/theLibrary.sld
 source/body.scm
```

`common/theLibrary.sld` would have `(include "../source/body.scm")` line but it won't work on these implementations.

Workaround
==========

Yuni does not use R7RS library features.

Reference
=========

* https://github.com/okuoku/yuni/issues/6
* https://github.com/okuoku/yuni/issues/7
