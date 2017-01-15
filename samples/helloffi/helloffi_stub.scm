(stubir0
  helloffi_stub
  (config
    (stubs (c "helloffi.stub.c")))

  (prologue 
    (cpp-include "helloffi.h"))

  (types)
  (layouts)
  (exports)
  (functions
    (int testfunc ((int arg1) (int arg2)))))
