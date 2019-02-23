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
(generic
  ;; Pre
  ("lib-runtime/generic/synrules.scm"
   "external/yuni-synrules.scm")
  ;; Runtime
  ("lib-runtime/generic/verboselib.scm"
   "lib-runtime/generic/libmgr-file.scm"
   "lib-runtime/generic/libmgr-core.scm"
   "lib-runtime/generic/libmgr-macro.scm"
   ;; Do not include yuniloader-generic to runtime
   ;"lib-runtime/generic/yuniloader-generic.scm"
   ))
