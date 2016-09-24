(stubir0
  testing_trivial
  (config
    (stubs (c   "testing.stub.c")
           (cxx "testing.stub.cpp")))
  ;; No Imports
  (prologue
    (cpp-define FOR_REFERENCE)
    (cpp-define "MUST_BE_DEFINED")
    (cpp-import-predefine __FILE__)
    ;; FIXME: Support else-if??
    ;; FIXME: Support undef??
    (cpp-if "__LINE__"
            (cpp-define "MUST_BE_DEFINED2"))
    (cpp-if "__LINE__"
            (cpp-define "MUST_BE_DEFINED3")
            (cpp-define "MUST_NOT_BE_DEFINED"))
    (cpp-if "__LINE__"
            (cpp-begin 
              (cpp-define "MUST_BE_DEFINED4")))
    (cpp-if "__LINE__"
            (cpp-begin
              (cpp-define "MUST_BE_DEFINED5"))
            (cpp-begin
              (cpp-define "MUST_NOT_BE_DEFINED2")))
    (cpp-ifdef "__FILE__"
               (cpp-define "MUST_BE_DEFINED6"))
    ;; Instantiate test code
    (cpp-include "testing.h") ;; prologue test
    (cpp-system-include "stdio.h") ;; Should define NULL
    ;; Define test interfaces which will be used below
    (cpp-include "testinterface.h"))

  (types
    ;; Define alternative types. Use them in func and export section.
    (integer altint "int" internal)
    ;(unsigned-integer uint "int" internal)
    (real altfloat "float" internal)

    ;; Type export tests
    (integer someint_t)
    (integer someenum_t)
    (integer someenum2_e c-enum)
    (blob somestruct_s c-struct)
    (blob someunion_u c-union)
    (blob someblob_t)
    (blob someopaque_t)

    ;; Pointer type tests
    (pointer zeroterminatedints
             (array-of someint_t)
             zero-terminated
             internal)
    (pointer voidptr (pointer-of void) internal)
    (pointer intptr (pointer-of int) internal)
    (pointer someblobvec 
             (array-of someblob_t) internal)
    (pointer someblobptr
             (pointer-of someblob_t) internal)
    (pointer somestructptr
             (pointer-of somestruct_s) internal)
    (pointer someunionptr
             (pointer-of someunion_u) internal)
    (pointer intvec
             (array-of altint) internal)

    ;; Enum/Flags
    (enum-group someenum_t
                (members ENUM_VAL_1
                         ENUM_VAL_2))
    (enum-group someenum2_e
                (members ENUM2_VAL_1
                         ENUM2_VAL_2))

    (flag-group flgs (members FLG_1 FLG_2))

    ;; FIXME: No local arrays??
    )

  (layouts
    (aggregate someblob_t
               (int a)
               (int b))
    (aggregate someunion_u
               (int a)
               (int b))
    (aggregate somestruct_s
               (altfloat fnum)
               (int someinteger)
               (int someintegers array)
               (int bufferlength (bytelength buffer))
               (voidptr buffer)
               (int someconst (constant FLG_1))
               (someenum2_e someenum2 (constant ENUM2_VAL_1))
               (int someintlength (count someints))
               (int someintbytelength (bytelength someints))
               (intvec someints)))

  (functions
    ;; Test: constcharptr
    (("const char*") test_constcharptr () (=> voidptr) forward-0)
    (int test_constcharptr_check ((int in)) forward-0)
    (("const void*") test_constvoidptr () (=> voidptr) forward-0)
    (int test_constvoidptr_check ((voidptr in)) forward-0)

    ;; Test: intvec
    (int test_intvec ((intvec in) (int count (count in))
                                  (int size (bytelength in))) forward-0)

    ;; Test: someblob_t (1234 5678)
    (int test_someblob_t_1 ((someblobptr in)) forward-0)
    ;; Test: someblob_t ((1234 5678) (8765 4321))
    (int test_someblob_t_2 ((someblobvec in)) forward-0)
    ;; Test: someunion_u (1234)
    (int test_someunion_u_1 ((someunionptr in)) forward-0)
    (int test_someunion_u_1a ((("union" "someunion_u*") in (=> someunionptr)))
         forward-0)
    ;; Test: someunion_u write (1234)
    ;; FIXME: We don't support struct-out param for now
    ;; (int test_someunion_u_2 ((someunionptr out)) forward-0)

    ;; Test: somestruct_s
    ;;         fnum: 0.5
    ;;         someinteger: 1234
    ;;         someintegers: 1 2 3 4
    ;;         bufflerlength: sizeof buffer
    ;;         buffer: char(1 2 3 4)
    ;;         someconst: FLG_1
    ;;         someenum2: ENUM2_VAL_1
    ;;         someintlength: length someint
    ;;         someintbytelength: length someint
    ;;         someints: #(1 2 3 4)
    ;;         intz: #(1 2 3 4) + 0
    ;;  test: input
    (int test_somestruct_1 ((somestructptr in)) forward-0)
    ;;  test: output
    (int test_somestruct_2 ((somestructptr out)) forward-0)

    ;; Test: forward-1
    (voidptr test_get_somestruct_1f_test () forward-0)
    (int test_somesturct_1f ((somestructptr in)) forward-1)

    ;; Test: varargs
    ;; FIXME: Not yet
    ;(int test_va ((int count) ...) forward-0)

    ;; Test: altint
    (int testecho_intecho ((altint a)) forward-0)

    ;; Test: backward-2
    ;; FIXME: How do we implement native callback??
    ;(voidptr test_get_backward2_stub ((voidptr ctxtest)) forward-0)
    ;(voidptr test_get_backward2_ctx ((voidptr ctxtest)) forward-0)
    ;(int test_backword2_stub ((voidptr ctx context)) backward-2)

    ;; Test: out(1234)
    (int test_outint ((int thevalue out)) forward-0))

  (exports
    ;; Constant 
    (int CONST_1 macro) ;; undefined since CONST_1 is actually a C value
    (int MINUS_CONST_1) ;; -1234
    (float REAL_1) ;; 0.5
    (altint CONST_2) ;; 1234
    (altfloat REAL_2) ;; 0.5
    (int UNDEFINED macro) ;; undefined
    
    ;; Test vectors
    (int size_of_someblob_t)
    (int size_of_someunion_u)
    (int size_of_somestruct_s)
    (int size_of_someopaque_t))
  )
