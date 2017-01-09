Command line parameters
=======================

Most of Yuni applications will not get "install"ed so we have to specify non-standard paths to Scheme implementations.

Yuni does not adopt SRFI-22 nor SRFI-138 for command-line spec.

Path specification
------------------

Command-line options format for path specifications varies between implementations.

|impl        |Type  |Libpath        |Progfile       |Arg |Remarks            |
|:-----------|:----:|:--------------|:--------------|:---|:------------------|
|gauche      |Type 1|`-I`           |               |    |`-A` for lib append|
|chibi-scheme|Type 1|`-I`           |               |    |                   |
|picrin      |Type 0|               |               |    |                   |
|chicken     |Type 0|               |               |    |                   |
|kawa        |Type 0|               |               |    |FIXME: Actually Type2|
|sagittarius |Type 1|`--loadpath=`  |               |    |                   |
|racket      |Type 2|`++path`       |               |    |                   |
|guile       |Type 1|`-L`           |               |    |                   |
|larceny     |Type 2|`-path`        |`-program`     |`--`|                   |
|ironscheme  |Type 0|               |               |    |FIXME: Actually Type1|
|chez        |Type 2|`--libdirs`    |`--program`    |    |                   |
|vicare      |Type 1|`--source-path`|`--r6rs-script`|`--`|                   |
|nmosh       |Type 2|`--loadpath=`  |               |    |                   |
|gambit      |Type 0|               |               |    |                   |
|mit-scheme  |Type 0|               |               |    |                   |

### Type 0

Some implementations do not have library-path concept at all. On these implementations, Yuniloader will parse program and locate libraries before performing actual loading.

Type 0 implementations: `chicken` `gambit` `kawa` `mit-scheme` `picrin` `ironscheme`

Kawa actually has library path and lookup feature but due to path handling issue on Win32, Yuni currently treats the implementation as Type 0. IronScheme also has library path features but Yuni wraps it to prevent returning zero exit code on failure.

### Type 1

Type 1 implementations will take a option for each library paths.

Some implementations provide 2 options to specify library path; one for append library path, one for prepend library path.

Type 1 implementations: `chibi-scheme` `gauche` `guile` `racket` `sagittarius` `vicare`

### Type 2

Type 2 implementations will take a option for multiple library paths. These implementations sometimes does not allow to specify multiple options so we have to merge and specify library path in one place.

(Actually, Yuni treats implementations as Type 1 when they accept multiple path options.)

In these implementations, a character is reserved for separator. On Unix-like platforms, it will be `:`, on Win32 it will be `;`. 

Type 2 implementations: `chez` `larceny` `nmosh`


Command line templates
======================

## Chez-scheme

```
chez-scheme --libdirs <LIBDIRS> --program <PROG> <ARGS>
```

## Chibi-scheme

```
chibi-scheme <LIBS> <PROG> <ARGS>
```

## Chicken

```
csi -b -require-extension r7rs -s <YUNILOADER> <LIBS> -MOD <YUNIFFIMOD> <PROG> <ARGS>
```

## Gauche

```
gosh -r7 <LIBS> <PROG> <ARGS>
```

FIXME: Adopt 0.9.5 spec.

## Gambit (yuniloader)

```
gsi <YUNILOADER> <LIBS> -MOD <YUNIFFIMOD> <PROG> <ARGS>
```

## Guile (yuniloader)

```
guile -l <YUNILOADER> <LIBS> <PROG> <ARGS>
```

## Kawa (yuniloader)

```
<JAVA> -classpath <KAWAJAR> kawa.repl --r7rs <YUNILOADER> <LIBS> <PROG> <ARGS>
```

## Larceny

```
larceny.bin.exe -heap <LARCENYROOT>/larceny.heap -r6rs <LIBS>
-program <PROG> -- <PROG> <ARGS>
```

## MIT-Scheme (yuniloader)

```
mit-scheme --batch-mode --load <YUNILOADER> -- -YUNIROOT <YUNIROOT>
<LIBS> -MOD <YUNIFFIMOD> <PROG> <ARGS>
```

## NMosh

```
nmosh --loadpath=<LIBDIRS> <PROG> <ARGS>
```

## Picrin (yuniloader)

```
picrin <YUNILOADER> --yuniffi-stubdir <YUNIFFIMOD> <PROG> <ARGS>
```

## Racket

```
racket -I scheme/init -l- r6rs/run.rkt <LIBS> <PROG> <ARGS>
```

## Sagittarius

```
sagittarius <LIBS> <PROG> <ARGS>
```

## Vicare

```
vicare <LIBS> --r6rs-script <PROG> -- <ARGS>
```
