(library (yuni ffi nccc)
  (export make-nccc-call-ctx
          nccc-set-modpath0! ;; Tentative
          nccc-loadlib
          ;; nccc_corelib support
          nccc-loaddispatch)
  (import (yuni scheme)
          (yuni hashtables)
          (yuni compat ffi primitives)
          (yuni compat bitwise primitives))

  (define modpath0 '())

  (define (nccc-set-modpath0! lis)
    (set! modpath0 lis))

  (define (nccc-loadlib modname libname)
    (define ctx (make-nccc-call-ctx))
    (let ((modpath (string-append (car modpath0)
                                  "/libnccc_" modname ".so"))
          (dispatchname (string-append "lib_" libname "_dispatch_ncccv0")))
      (let* ((module (yuniffi-module-load0 modpath))
             (dispatch (and module
                            (yuniffi-module-lookup module dispatchname))))
        (and dispatch
             (make-nccc-library/dispatch ctx dispatch)))))

  (define (nccc-loaddispatch) ;; => lambda
    (define ctx (make-nccc-call-ctx))
    (let ((modpath (string-append (car modpath0)
                                  "/libnccc_" "nccc_corelib" ".so")))
      (let ((module (yuniffi-module-load0 modpath))
            (v (make-vector 8)))
        (let loop ((idx 0))
         (let ((dispatchname (string-append "nccc_dispatch_"
                                            (number->string idx))))
           (if (= idx 8)
               (let ((d0 (vector-ref v 0))
                     (d1 (vector-ref v 1))
                     (d2 (vector-ref v 2))
                     (d3 (vector-ref v 3))
                     (d4 (vector-ref v 4))
                     (d5 (vector-ref v 5))
                     (d6 (vector-ref v 6))
                     (d7 (vector-ref v 7)))
                 (case-lambda
                   ((in) (d0 in))
                   ((in p0) (d1 in p0))
                   ((in p0 p1) (d2 in p0 p1))
                   ((in p0 p1 p2) (d3 in p0 p1 p2))
                   ((in p0 p1 p2 p3) (d4 in p0 p1 p2 p3))
                   ((in p0 p1 p2 p3 p4) (d5 in p0 p1 p2 p3 p4))
                   ((in p0 p1 p2 p3 p4 p5) (d6 in p0 p1 p2 p3 p4 p5))
                   ((in p0 p1 p2 p3 p4 p5 p6) 
                    (d7 in p0 p1 p2 p3 p4 p5 p6))
                   (_ (error "Invalid arg count"))))
               (begin
                 (vector-set! v idx (yuniffi-module-lookup/dispatchN 
                                      module dispatchname idx))
                 (loop (+ idx 1)))))))))

  (define (make-nccv64-unpacker lis)
    (define (tmpread ptr off)
      ;; Deref pointer 
      (let ((p (ptr-read/ptr ptr off)))
       ;; FIXME: Upper bound..?
       (ptr-read/asciiz p 0 4096)))

    (define (sym->reader sym)       
      (case sym        
        ((s8) ptr-read/s8)
        ((u8) ptr-read/u8)    
        ((s16) ptr-read/s16)  
        ((u16) ptr-read/u16)        
        ((s32) ptr-read/s32)
        ((u32) ptr-read/u32)
        ((s64) ptr-read/s64)
        ((u64) ptr-read/u64)
        ((ptr) ptr-read/ptr) 
        ((f32) ptr-read/f32)
        ((f64) ptr-read/f64)
        ((sptr) ptr-read/sptr)
        ((uptr) ptr-read/uptr)
        ((str) tmpread)
        (else (error "Unknown sym")))) 
    (define len (length lis))
    (define readers* (map sym->reader lis))
    (lambda (buf offs) 
      (let loop ((acc '())
                 (idx offs)
                 (r readers*))
        (if (null? r)  
            (reverse acc)
            (loop (cons ((car r) buf idx) acc) (+ idx 8) (cdr r))))))

  (define (make-nccv64-packer lis)
    (define (sym->writer sym)
      (case sym        
        ((s8) ptr-write/s8!)
        ((u8) ptr-write/u8!)
        ((s16) ptr-write/s16!)
        ((u16) ptr-write/u16!)
        ((s32) ptr-write/s32!)
        ((u32) ptr-write/u32!) 
        ((s64) ptr-write/s64!) 
        ((u64) ptr-write/u64!)
        ((ptr) ptr-write/ptr!)
        ((f32) ptr-write/f32!)
        ((f64) ptr-write/f64!)
        ((sptr) ptr-write/sptr!)
        ((uptr) ptr-write/uptr!)
        (else (error "Unknown sym"))))
    (define len (length lis))
    (define writers* (map sym->writer lis))
    (lambda (buf offs . x)
      (let loop ((idx offs)
                 (w writers*)
                 (cur x))
        (cond ((null? cur)
               (unless (null? w)
                 (error "Invalid arg count"))
               'ok)    
              ((null? w)
               (error "Invalid input count"))
              (else    
                ((car w) buf idx (car cur))
                (loop (+ idx 8) (cdr w) (cdr cur)))))))

  (define (make-nccc-call-ctx)
    (cons (buf-alloc (* 8 32))
          (buf-alloc (* 8 32))))

  (define (make-nccc-caller func inlis outlis)
    (define pack (make-nccv64-packer inlis))
    (define unpack (make-nccv64-unpacker outlis))
    (lambda (ctx . x)
      (let ((inbuf (car ctx))
            (outbuf (cdr ctx)))
        (apply pack inbuf 0 x)
        (yuniffi-nccc-call func inbuf outbuf)
        (apply values (unpack outbuf 0)))))

  ;; Dispatch library
  ;; library_info
  ;;   [1:s32] => [name:ptr 0 forward_0_stub_count:s32 0 0 0]
  ;; library_export_info
  ;;   [2:s32 fnid:s32] =>
  ;;     [0:s32 idx:s32 nam:str 0:s32 0:s32 func:ptr args:s32 out:s32]
  ;;     [-1:s32] (error)
  ;; library_arg_info
  ;;   [6 id:s32] =>
  ;;     [0:s32 args:s32 out:s32 <ARGTYPE>:s32...]
  ;;     [-1:s32] (error)
  ;;   <ARGTYPE> = CITYPE_*
  (define (make-nccc-library/dispatch ctx dispatch)
    (define ht (make-string-hashtable))
    (define library_info (make-nccc-caller dispatch '(s32)                        
                                           '(str s64 s32 s64 s64 s64)))
    (define library_export_info (make-nccc-caller
                                  dispatch 
                                  '(s32 s32)
                                  '(s32 s32 str s32 s32 ptr s32 s32)))
    (define (library_arg_info ctx funcid)
      (define (citype->sym x)
        (case x
          ((0) 'u32)
          ((1) 'u64)
          ((2) 'f32)
          ((3) 'f64)
          ((4) 's32)
          ((5) 's64)
          ((6) 'ptr)
          ((7) 'uptr)
          ((8) 'sptr)
          ((9) 'u8)
          ((10) 'u16)
          ((11) 's8)
          ((12) 's16)
          (else (error "ABI error" x))))
      (define packer (make-nccv64-packer '(s32 s32)))
      (let ((inbuf (car ctx))
            (outbuf (cdr ctx)))
        (packer inbuf 0 6 funcid)
        (yuniffi-nccc-call dispatch inbuf outbuf)
        (let ((r (ptr-read/s32 outbuf 0)))
         (cond ((= r -1) #f)
               ((= r 0)
                (let ((inargs (ptr-read/s32 outbuf 8))
                      (outargs (ptr-read/s32 outbuf 16)))
                  (unless (and (<= 0 inargs) (> 15 inargs))
                    (error "Out of range inargs" inargs))
                  (unless (and (<= 0 outargs) (> 15 outargs))
                    (error "Out of range inargs" outargs))
                  (let loop ((idx 24)
                             (cnt 0)
                             (inacc '()))
                    (if (= cnt inargs)
                        (let loop ((idx idx)
                                   (outcnt 0)
                                   (outacc '()))
                          (if (= outcnt outargs)
                              (values (reverse inacc) (reverse outacc))
                              (loop (+ idx 8) 
                                    (+ outcnt 1)
                                    (cons (citype->sym (ptr-read/s32 outbuf idx)) 
                                          outacc))))
                        (loop (+ idx 8)
                              (+ cnt 1)
                              (cons (citype->sym (ptr-read/s32 outbuf idx)) 
                                    inacc))))))))))

    (define (fill-export! fnid)
      (call-with-values
        (lambda () (library_export_info ctx 2 fnid))
        (lambda (err idx nam _ __ funcptr argc outc)
          (write (list nam funcptr argc outc)) (newline)
          (let ((callable (yuniffi-nccc-ptr->callable funcptr)))
           (call-with-values 
             (lambda () (library_arg_info ctx fnid))
             (lambda (in* out*)
               (hashtable-set! ht nam 
                               (cons funcptr
                                     (make-nccc-caller callable in* out*)))
               (write (list 'IN: in* 'OUT: out*)) (newline)))))))

    (call-with-values
      (lambda () (library_info ctx 1))
      (lambda (name _ f0stubcount __ ___ ____)
        (write (list name f0stubcount)) (newline)
        (let loop ((idx 0))
         (unless (= idx f0stubcount)
           (fill-export! idx)
           (loop (+ idx 1))))))
    ht)

)
