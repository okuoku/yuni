Port Status
===========

|impl            |Working?  |Bootstrap?|Win32?|YuniFFI?|YuniFFI impl|
|:---------------|:--------:|:--------:|:----:|:------:|:-----------|
|[chez][]        |X         |X         |      |X       |Native      |
|[chibi-scheme][]|X         |X         |X     |X       |Module      |
|[chicken][]     |X         |X         |      |X       |Module      |
|[gambit][]      |Alexpander|          |Crash |X       |Module      |
|[gauche][]      |X         |X         |X     |X       |Module      |
|[guile][]       |X         |          |      |X       |Native      |
|[ironscheme][]  |X         |X         |X     |        |            |
|[kawa][]        |X         |          |      |        |            |
|[larceny][]     |X         |          |X     |Partial |Native      |
|[mit-scheme][]  |Alexpander|          |X     |        |            |
|[nmosh][]       |X         |          |      |X       |Native      |
|[picrin][]      |          |          |      |X       |Compile-in  |
|[racket][]      |X         |X         |X     |X       |Native      |
|[sagittarius][] |X         |X         |X     |X       |Native      |
|[vicare][]      |Partial   |          |      |X       |Native      |

* `Bootstrap?`: These implementations can be used to "bootstrap" yuni library ie.) when building Yuni from source, one of these implementation is required.
* `Win32?`: For these implementations, Yuni can be bootstrapped/run with Win32 binary distributions.
 * Gambit: Sometimes crashes on exit. Use VisualC variant instead.
* `YuniFFI?`: YuniFFI is supported. 
 * `Module`: Requires stub module loader C extension. C extensions will be built with CMake. See `yunistub` for its sources.
 * `Compile-in`: Specially crafted interpreter required.
 * `Native`: Implemented using implementation's native FFI system.

Symbol mapping
--------------


|impl        |CMake variable|CMake interp|CMake comp |CMake pkg     |
|:-----------|:-------------|:-----------|:----------|:-------------|
|chez        |CHEZ_SCHEME   |CHEZ_SCHEME |           |              |
|chibi-scheme|CHIBI_SCHEME  |CHIBI_SCHEME|           |              |
|chicken     |CHICKEN       |CHICKEN_CSI |CHICKEN_CSC|CHICKEN       |
|gambit      |GAMBIT        |GSI         |GSC        |              |
|gauche      |GAUCHE        |GOSH        |           |GAUCHE_PACKAGE|
|guile       |GUILE         |GUILE       |           |              |
|ironscheme  |IRON_SCHEME   |IRON_SCHEME |           |              |
|kawa        |KAWA          |KAWA_JAR    |           |              |
|larceny     |LARCENY       |LARCENY     |           |              |
|mit-scheme  |MIT_SCHEME    |MIT_SCHEME  |           |              |
|nmosh       |NMOSH         |NMOSH       |           |              |
|picrin      |PICRIN        |PICRIN      |           |              |
|racket      |RACKET        |RACKET      |           |RACO          |
|sagittarius |SAGITTARIUS   |SAGITTARIUS |           |              |
|vicare      |VICARE        |VICARE      |           |              |

For historical reasons, this mapping contains some unintentional inconsistencies:

* `CHEZ_SCHEME` sometimes referenced as `CHEZ` (e.g. in Yunibase)

Others
======

Yuni also have some support for pre-R6RS/R7RS Scheme implementations. 

Basic requirements are:

- `SRFI-6` Basic string ports
- `SRFI-30` Nested multi-line comments
- `SRFI-46` (Optional) Basic syntax-rules extensions - can use Alexpander 


[chez]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/chez.md
[chibi-scheme]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/chibi-scheme.md
[chicken]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/chicken.md
[gambit]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/gambit.md
[gauche]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/gauche.md
[guile]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/guile.md
[ironscheme]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/ironscheme.md
[kawa]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/kawa.md
[larceny]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/larceny.md
[mit-scheme]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/mit-scheme.md
[nmosh]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/nmosh.md
[picrin]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/picrin.md
[racket]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/racket.md
[sagittarius]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/sagittarius.md
[vicare]: https://github.com/okuoku/yuni/blob/master/doc/PortingNotes/vicare.md
