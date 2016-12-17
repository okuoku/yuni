Yuni library build system
=========================

Currently, Yuni cannot be built as "package" or "modules" or "eggs" or whatever. Instead, it will generate pre-configured command-line script.

Buildsystem will generate 2 flavours of scripts:

* `yuniboot` - script for Yuni developer. `include`s yuni source so you don't have to rebuild *unless you had changed `import` or `export` clauses*.
* `yunified` - script for Yuni applications. Buildsystem will byte-compile or other post-processes. Thus it would need to re-built before running any apps.

Bootstrap Scheme
----------------

Since CMake cannot read S-expressions directly, Yuni buildsystem will call R6RS/R7RS implementations to read yuni library and configuration. The implementation used for this purpose refered as "bootstrap Scheme".

Not every implementation can be used as bootstrap Scheme. Currently, following implementations are supported:

 * Chez scheme
 * chibi-scheme
 * Racket
 * Sagittarius
 * Gauche
 * IronScheme (`YUNI_IRON_SCHEME_ROOT`)

Implementations except IronScheme will be auto-detected by the build script. IronScheme needed to be specified with `YUNI_IRON_SCHEME_ROOT` CMake variable.

CMake variables
---------------

Buildsystem accepts following CMake variables:

 * `YUNI_BOOTSTRAP_USE`: Select bootstrap Scheme. `chibi-scheme`, `gauche`, ...
 * `YUNI_IRON_SCHEME_ROOT`: CMake path for directory which contains IronScheme-Console executable.
 * `YUNI_WITH_YUNIBASE`: CMake path to yunibase build; directory contains `stable` and `current` directories.


