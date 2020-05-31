(define yuni/polyfills
  '(
    (boolean=? . "std/boolean_eqp.scm")
    (floor-quotient . "std/floor-quotient.scm")
    (floor-remainer . "std/floor-remainder.scm")
    (floor/ . "std/floor_div.scm")
    (list-copy . "std/list-copy.scm")
    (list-set! . "std/list-set_x.scm")
    (make-list . "std/make-list.scm")
    (modulo . "std/modulo.scm")
    (quotient . "std/quotient.scm")
    (remainder . "std/remainder.scm")
    (string-copy . "std/string-copy.scm")
    (string-for-each . "std/string-for-each.scm")
    (string-map . "std/string-map.scm")
    (string->list . "std/string_to_list.scm")
    (string->vector . "std/string_to_vector.scm")
    (truncate-quotient . "std/truncate-quotient.scm")
    (truncate-remainder . "std/truncate-remainder.scm")
    (truncate/ . "std/truncate_div.scm")
    (vector-append . "std/vector-append.scm")
    (vector-copy . "std/vector-copy.scm")
    (vector-copy! . "std/vector-copy_x.scm")
    (vector-fill! . "std/vector-fill_x.scm")
    (vector-for-each . "std/vector-for-each.scm")
    (vector-map . "std/vector-map.scm")
    (vector->list . "std/vector_to_list.scm")
    (vector->string . "std/vector_to_string.scm")
    ))
