;;
;; NB: To import "generic" part, the runtime require
;;     impl-specific runtime field(even if it is an empty list)
;;
(biwascheme
  ;; Pre
  ("lib-runtime/biwascheme/prelib.scm")
  ;; Runtime
  ("lib-runtime/generic/std/vector-map.scm"
   "lib-runtime/generic/std/make-list.scm"
   "lib-runtime/generic/std/list-set_x.scm"
   "lib-runtime/generic/std/modulo.scm"
   "lib-runtime/generic/std/floor-remainder.scm"
   "lib-runtime/generic/std/floor-quotient.scm"
   "lib-runtime/generic/std/floor_div.scm"
   "lib-runtime/generic/std/quotient.scm"
   "lib-runtime/generic/std/remainder.scm"
   "lib-runtime/generic/std/truncate-remainder.scm"
   "lib-runtime/generic/std/truncate-quotient.scm"
   "lib-runtime/generic/std/truncate_div.scm"))
(s7
  ;; Pre
  ("lib-runtime/s7/prelib.scm")
  ;; Runtime
  ("lib-runtime/generic/std/vector-map.scm"))
(gambit
  ;; Pre
  ("lib-runtime/gambit/prelib.scm")
  ;; Runtime
  ())
(mit-scheme
  ;; Pre
  ("lib-runtime/mit-scheme/prelib.scm")
  ;; Runtime
  ())
(stklos
  ;; Pre
  ()
  ;; Runtime
  ())
(scm
  ;; Pre
  ("lib-runtime/scm/prelib.scm")
  ;; Runtime
  ("lib-runtime/generic/std/boolean_eqp.scm"
   "lib-runtime/generic/std/list-copy.scm"
   "lib-runtime/generic/std/list-set_x.scm"
   "lib-runtime/generic/std/quotient.scm"
   "lib-runtime/generic/std/modulo.scm"
   "lib-runtime/generic/std/remainder.scm"
   "lib-runtime/generic/std/floor-quotient.scm"
   "lib-runtime/generic/std/floor-remainder.scm"
   "lib-runtime/generic/std/floor_div.scm"
   "lib-runtime/generic/std/truncate-quotient.scm"
   "lib-runtime/generic/std/truncate-remainder.scm"
   "lib-runtime/generic/std/truncate_div.scm"
   "lib-runtime/generic/std/string-map.scm"
   "lib-runtime/generic/std/string-for-each.scm"
   "lib-runtime/generic/std/string_to_vector.scm"
   "lib-runtime/generic/std/string_to_list.scm"
   "lib-runtime/generic/std/vector_to_string.scm"
   "lib-runtime/generic/std/vector-map.scm"
   "lib-runtime/generic/std/vector-for-each.scm"
   "lib-runtime/generic/std/string-copy.scm"
   "lib-runtime/generic/std/vector-copy.scm"
   "lib-runtime/generic/std/vector_to_list.scm"
   "lib-runtime/generic/std/vector-copy_x.scm"
   "lib-runtime/generic/std/vector-append.scm"
   "lib-runtime/generic/std/vector-fill_x.scm"
   "lib-runtime/scm/hashtable.scm"
   "lib-runtime/scm/string-output-port.scm" ;; Depends hashtable
   ))
(bigloo
  ;; Pre
  ()
  ;; Runtime
  ("lib-runtime/generic/std/boolean_eqp.scm"
   "lib-runtime/generic/std/list-copy.scm"
   "lib-runtime/generic/std/list-set_x.scm"
   "lib-runtime/generic/std/quotient.scm"
   "lib-runtime/generic/std/modulo.scm"
   "lib-runtime/generic/std/remainder.scm"
   "lib-runtime/generic/std/floor-quotient.scm"
   "lib-runtime/generic/std/floor-remainder.scm"
   "lib-runtime/generic/std/floor_div.scm"
   "lib-runtime/generic/std/truncate-quotient.scm"
   "lib-runtime/generic/std/truncate-remainder.scm"
   "lib-runtime/generic/std/truncate_div.scm"
   "lib-runtime/generic/std/string-map.scm"
   "lib-runtime/generic/std/string-for-each.scm"
   "lib-runtime/generic/std/string_to_vector.scm"
   "lib-runtime/generic/std/string_to_list.scm"
   "lib-runtime/generic/std/vector_to_string.scm"
   "lib-runtime/generic/std/vector-map.scm"
   "lib-runtime/generic/std/vector-for-each.scm"
   "lib-runtime/generic/std/string-copy.scm"
   "lib-runtime/generic/std/vector-copy.scm"
   "lib-runtime/generic/std/vector_to_list.scm"
   "lib-runtime/generic/std/vector-copy_x.scm"
   "lib-runtime/generic/std/vector-append.scm"
   "lib-runtime/generic/std/vector-fill_x.scm"
   ) )
(generic
  ;; Pre
  ("external/yuni-synrules.scm"
   "lib-runtime/generic/synrules.scm")
  ;; Runtime
  ("lib-runtime/generic/verboselib.scm"
   "lib-runtime/generic/libmgr-file.scm"
   "lib-runtime/generic/libmgr-core.scm"
   "lib-runtime/generic/libmgr-macro.scm"
   ;; Do not include yuniloader-generic to runtime
   ;"lib-runtime/generic/yuniloader-generic.scm"
   ))
