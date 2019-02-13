Basic library sample
--------------------

This example shows some basic characteristics of yuni platform:

1. R6RS-lite apps. yuni allows to define R6RS-styled application 
   which can be used with wider-range of Scheme implementations,
   including R7RS(chibi-scheme) or Generic Schemes(BiwaScheme,
   s7).

2. R6RS-lite libraries. Define library with `library` form, even on
   R7RS implementations.

3. yuni's R6RS-lite still allows library-local variable definitions,
   even on Generic Schemes that only allow `load`-ing scripts.

4. `syntax-rules` macro is still available on Generic Schemes with
   some limitations.

The example defines some libraries and use it from an application.

## Files

- `app.sps` -- Application's entry point
- `A.sls` -- defines library `(A)`
- `B.sls` -- defines library `(B)`
- `sub/A.sls` -- defines library `(sub A)`

## See also

TBD.

## Running the example with interpreters

Running with "selfboot" environment is recommended as it doesn't need
any additional processes.

### selfboot runtime

This sample can run on the "selfboot" environment. Selfboot runtime is part of
yuni buildsystem so we don't need to do anything except checking out this
repository on somewhere.

- `chibi-scheme ../../lib-runtime/selfboot/chibi-scheme/selfboot-entry.scm -LIBPATH . app.sps`
- `gosh ../../lib-runtime/selfboot/gauche/selfboot-entry.scm -LIBPATH . app.sps`
- `sagittarius ../../lib-runtime/selfboot/sagittarius/selfboot-entry.sps -LIBPATH . app.sps`
- (ChezScheme) `scheme --program ../../lib-runtime/selfboot/chez/selfboot-entry.sps -LIBPATH . app.sps`


### Docker images

TBD.

yunibase ( https://github.com/okuoku/yunibase ) provides prebuilt yuni
runtime environment for several Scheme implementations with Docker.

### yuni

TBD.

## Compiling example

TBD.

