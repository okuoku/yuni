Drypack serialize format (TENTATIVE)
====================================

Drypack is intended to be a simplest possible Scheme object serialize format. It is sort-of FASL but not fast nor efficient.

Drypack uses unsigned LEB128 integer to serialize integers. Thus every Drypack object is just a sequence of

- LEB128, or
- LEB128(length) Opaque byte sequence

In this document `<content>` denotes LEB128 integer and `[content]` denotes opaque byte sequence.

File structure
--------------

```
<Total> <Output> 
<4> <Chars> <Numbers> <Symbols> <Strings> <Bytevectors>
<Pairs> <Vectors> ... data ...
```

`Total` is total address count which can be referenced with pairs and vectors. Deserializer will `(define table (make-vector <Total>))` first. *Address* means index of this `table` vector.

`Output` is an address for output. Deserializer returns result of `(vector-ref table <Output>)`.

`4` is for Zone0 object count. It won't appear serialized data.

`Chars` to `Vectors` are counts for each object class. Sum of these should be equal to `Total` count.

Zone0 Objects
-------------

|Address|Object|
|:------|:-----|
|00     |null  |
|01     |`#t`  |
|02     |`#f`  |
|03     |`#!eof`|

Char objects
------------

Character object is just a `<char-number>` thus it will be decoded with `integer->char`.

Number objects
--------------

Number objects are noted with sequence of `<Proto> <Number>` or `<Proto> <Len> [utf8]`.

Protocol 1 and 2 is for integer. Protocol 3 is tentatively-reserved for `string->number` decoded number.

For negative integers, it will be encoded as `<2> <(- number)>` (ie. Signed LEB128 is not used.)

Symbol and string objects
-------------------------

`<Len> [utf8]`

Bytevectors
-----------

`<Len> [bytes]`

Pairs
-----

`<Address-car> <Address-cdr>`

Vectors
-------

`<Len> <Address> ...`


