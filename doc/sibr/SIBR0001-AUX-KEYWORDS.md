SIBR0001: Aux keywords are not bound in standard library
========================================================

Affected: `Chicken` `Guile`(2.0.11+ or later)

Some aux keywords/syntaxes are not bound in these implementations so we cannot rename or re-export them.

Yuni requires following aux keywords are bound in `(scheme base)`.

* `...`
* `=>`
* `else`
* `unquote`
* `unquote-splicing`

Workaround
==========

Yuni does not rename these aux keywords.

Yuni build script removes these aux keywords from library export on affected implementation.

Reference
=========

* https://github.com/okuoku/yuni/issues/11
* https://github.com/okuoku/yuni/issues/29

