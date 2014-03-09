(library (yuni binary walk genlayout)
         (export 
           sym-cstruct
           sym-csize
           generate-c-layout-src)
         (import (rnrs)
                 (srfi :26)
                 (srfi :42)
                 (srfi :48)
                 (shorten)
                 (yuni core)
                 (yuni binary walk common)
                 (yuni binary walk specfile)
                 (match))
;;

;; Layout spec format:
;;  
;;  C style construct: 
;;  (constant C-NAME)
;;  (global C-NAME type)
;;  (global C-NAME (struct type))
;;  (c-struct NAME STRUCT-SPEC ...)
;;  (struct NAME STRUCT-SPEC ...)
;;  (network TYPE NETWORK-SPEC ...)
;;  (flags TYPE FLAG-SPEC ...)
;;
;;    BUILTIN-TYPE:
;;     char
;;
;;    STRUCT-SPEC:
;;     (NAME ATTR ...)
;;   
;;    ATTR:
;;     (count CONSTANT)
;;     (count-of ENTRY)
;;     (pointer-of TYPE)
;;     (head-of TYPE NETWORK-TYPE)
;;     (array-of TYPE)
;;     (pointer-array-of TYPE)
;;     (link-of NETWORK-TYPE)
;;
;;    NETWORK:
;;     (head-first member)
;;     (prev member)
;;     (next member)
;;
;;  CPP SPEC
;;  (cpp-define SYM obj)
;;  (cpp-include INCLUDE-SPEC)
;;  (cpp-if STR (CPP-SPEC ...) [(CPP-SPEC ...)])

(define (emit-header p)
  ;; Defines some macros
  (format p "#include <stdint.h>\n")
  (format p "#include <stddef.h>\n")
  (format p "#define QQ_W0(x) (((uint64_t)(x)>>0)&0xffff)\n")
  (format p "#define QQ_W1(x) (((uint64_t)(x)>>16)&0xffff)\n")
  (format p "#define QQ_W2(x) (((uint64_t)(x)>>32)&0xffff)\n")
  (format p "#define QQ_W3(x) (((uint64_t)(x)>>48)&0xffff)\n\n"))

(define (emit-cpp-clause* p desc)
  (match desc
         ((('cpp-define SYM obj) . rest)
          (format p "#define ~a ~a\n" SYM obj)
          (emit-cpp-clause* p rest))
         ((('cpp-include name) . rest) 
          (format p "#include <~a>\n" name) 
          (emit-cpp-clause* p rest)) 
         ;; Ignore non cpp-clauses
         ((something . rest)
          (emit-cpp-clause* p rest))
         ;; Finish
         ('() 'ok)))


(define (emit-value-seed* p l)
  ;; We're using uninitialized array of char and its size
  ;; to extract C value into ELF/PE symbol table.
  ;; This approach was took from BSD's assym.

  ;; Every values are split into 16bits. We assume every values are 64bits.

  ;; FIXME: Currently, it cannot handle any negative value!
  (for-each 
    (^e
      (match e
             ((sym . exp)
              (do-ec (: i 4)
                     (format p "char QQ_M~a_~a[0x1000+QQ_W~a(~a)];\n" 
                             i sym i exp)))))
    l)) 


(define (emit-csym p str)
  (emit-value-seed* 
    p
    (list (cons str str)
          (cons (format "SIZEXX_~a" str)
                (format "sizeof(~a)" str)))))

(define (exp-cstruct/offset struct-name l)
  (format "offsetof(struct ~a, ~a)"
          struct-name
          (fold-left
            (^[cur e]
              (format "~a.~a" cur e))
            (symbol->string (car l))
            (cdr l))))

(define (exp-cstruct/size struct-name l)
  (format "sizeof(((struct ~a *)0)->~a)"
          struct-name
          (fold-left
            (^[cur e]
              (format "~a.~a" cur e))
            (symbol->string (car l))
            (cdr l)))) 

(define (emit-cstruct-entry p struct-name l)
  (emit-value-seed*
    p
    (list (cons (sym-cstruct struct-name l)
                (exp-cstruct/offset struct-name l))
          (cons (sym-csize (sym-cstruct struct-name l))
                (exp-cstruct/size struct-name l)))))

(define (emit-seed p obj)
  (write (list (is-a? obj entry) obj))(newline)
  (cond
    ;; Ignore other than entry for now...
    ((is-a? obj entry)
     ;(display (list 'entry: (~ obj 'name) 'parent: (~ obj 'parent)))(newline)
     (cond
       ((~ obj 'constant?)
        (emit-csym p (symbol->string (~ obj 'name))))
       ((not (eq? #t (~ obj 'parent)))
        (let-with obj (parent)
          (define names 
            (append (if (list? parent) parent (list parent))
                    (list (~ obj 'name))))
          (emit-cstruct-entry p (car names) (cdr names))))))))

(define (emit-seeds p entries)
  ;; FIXME: Implement network seeds
  (for-each (cut emit-seed p <>) entries))

(define (generate-c-layout-src name desc)
  ;; We have to generate 2 kinds of symbol exporter
  ;;
  ;; 1. Symbol size  SYMBOL_size
  ;; 2. Struct offset TYPE_ENTRY_offset

  ;; We have to generate 1 kind of network accessor
  (define l (parse-spec desc))
  (define e (expand-spec l))
  (define (emit p)
    (emit-header p)
    (emit-cpp-clause* p desc)
    (emit-seeds p e))
  (display (list (length l) 'specs))(newline)
  (display (list (length e) "specs (expanded)"))(newline)
  (call-with-output-file
    name
    emit))

)
