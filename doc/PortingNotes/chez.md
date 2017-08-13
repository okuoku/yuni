ChezScheme support
==================

* [GitHub](https://github.com/cisco/ChezScheme)
* [Official document](http://cisco.github.io/ChezScheme/csug9.4/csug.html)

YuniFFI
-------

YuniFFI is implemented with Chez's native FFI framework. Thus, YuniFFI is not supported on Petite- (interpreted) variant of Chez.


Debugging
---------

By default, Chez does not break into debugger on CTRL+C. `debug-on-exception` paramter allows overriding this behaviour.

Say `(debug-on-exception #t)` and `(keyboard-interrupt-handler (lambda () (error "Interrupt")))` on the top level.
