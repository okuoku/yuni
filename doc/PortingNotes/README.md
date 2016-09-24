Port Status
===========

|impl            |Working?  |Bootstrap?|Win32?|YuniFFI?|YuniFFI impl|
|:---------------|:--------:|:--------:|:----:|:------:|:-----------|
|[gauche][]      |X         |X         |X     |X       |Module      |
|[chibi-scheme][]|X         |X         |X     |X       |Module      |
|[picrin][]      |          |          |      |X       |Compile-in  |
|[kawa][]        |X         |          |      |        |            |
|[sagittarius][] |X         |X         |X     |X       |Native      |
|[racket][]      |X         |X         |X     |X       |Native      |
|[guile][]       |X         |          |      |X       |Native      |
|[larceny][]     |X         |          |X     |Partial |Native      |
|[ironscheme][]  |X         |X         |X     |        |            |
|[chez][]        |X         |          |      |X       |Native      |
|[vicare][]      |Partial   |          |      |X       |Native      |
|[gambit][]      |Alexpander|          |Crash |X       |Module      |
|[mit-scheme][]  |Alexpander|          |X     |        |            |


* `Bootstrap?`: These implementations can be used to "bootstrap" yuni library ie.) when building Yuni from source, one of these implementation is required.
* `Win32?`: For these implementations, Yuni can be bootstrapped/run with Win32 binary distributions.
 * Gambit: Sometimes crashes on exit.
* `YuniFFI?`: YuniFFI is supported. 
 * `Module`: Requires stub module loader C extension. C extensions will be built with CMake. See `yunistub` for its sources.
 * `Compile-in`: Specially crafted interpreter required.
 * `Native`: Implemented using implementation's native FFI system.

Symbol mapping
--------------


|impl        |CMake variable|Legacy libname|Yunified name|
|:-----------|:-------------|:-------------|:------------|
|gauche      |GAUCHE        |              |gosh|
|chibi-scheme|CHIBI_SCHEME  |chibi||
|picrin      |PICRIN        |||
|kawa        |KAWA          |||
|sagittarius |SAGITTARIUS   |||
|racket      |RACKET        |||
|guile       |GUILE         |||
|larceny     |LARCENY       |||
|ironscheme  |IRON_SCHEME   |||
|chez        |CHEZ_SCHEME   |              |chez-scheme  |
|vicare      |VICARE        |||
|nmosh       |NMOSH         |||
|gambit      |GAMBIT        |              |gsi          |
|mit-scheme  |MIT_SCHEME    |||

For historical reasons, this mapping contains some unintentional inconsistencies:

* `CHEZ_SCHEME` sometimes referenced as `CHEZ` (e.g. in Yunibase)
* `chibi` library name should be `chibi-scheme`

Others
======

Yuni also have some support for pre-R6RS/R7RS Scheme implementations. 

Basic requirements are:

- `SRFI-6` Basic string ports
- `SRFI-30` Nested multi-line comments
- `SRFI-46` (Optional) Basic syntax-rules extensions - can use Alexpander 

[gauche]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/gauche.md
[chibi-scheme]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/chibi-scheme.md
[picrin]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/picrin.md
[kawa]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/kawa.md
[sagittarius]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/sagittarius.md
[racket]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/racket.md
[guile]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/guile.md
[larceny]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/larceny.md
[ironscheme]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/ironscheme.md
[chez]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/chez.md
[vicare]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/vicare.md
[gambit]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/gambit.md
[mit-scheme]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/mit-scheme.md
