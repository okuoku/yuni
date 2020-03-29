SIBR0001: Aux keywords are not bound in standard library
========================================================

Affected: `Chicken` `Guile`(2.0.11+ or later but not 3.x)

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
* http://git.savannah.gnu.org/gitweb/?p=guile.git;a=commit;h=374c1e5807a35b16170ed7686abcd5c407554d78 -- Guile 3.0 have these bindings
