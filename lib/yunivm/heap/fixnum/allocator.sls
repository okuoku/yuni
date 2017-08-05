(library (yunivm heap fixnum allocator)
         (export fixnum-allocator)
         (import (yuni scheme)
                 (yunivm heap fixnum bits32))

;;

(define (fixnum-allocator query)
  (define HEAPMAX (- ((query 'SIZE)) 16))
  (define iref (query 'REF))
  (define iset (query 'SET!))

  ;; TLSF ops
  (define l1 #f)
  (define l2 #f)
  (define freelist #f)

  (define (l1-chop pos)
    ;; Chop top to pos bits from l1
    (if (= pos 31)
      l1 ;; All bits allowed
      ;; Clear 31st bit first, then clear required bits
      (let ((n (if (negative? l1)
                 (+ l1 2147483648)
                 l1)))
        (modulo n (expt 2 (- pos 1))))))

  (define (search siz) ;; Calc available loc => (cls pos)
    (call-with-values 
      (lambda () (insert siz))
      (lambda (cls _)
        (let ((bt (bsr-s32 (l1-chop cls))))
         (unless bt
           (error "No block available" siz))
         ;(display (list 'SRC siz '=> bt (bsr-s32 (vector-ref l2 bt)) '<= l1 (vector-ref l2 bt))) (newline)
         (values bt
                 (bsr-s32 (vector-ref l2 bt)))))))

  (define (insert0 siz) ;; Calc loc => (cls pos)
    (cond
      ((< siz 256)
       (values 0 (quotient siz 8)))
      ((< siz 512)
       (values 1 (quotient (- siz 256) 8)))
      ((< siz 1024)
       (values 2 (quotient (- siz 512) 16)))
      ((< siz 2048)
       (values 3 (quotient (- siz 1024) 32)))
      ((< siz 4096)
       (values 4 (quotient (- siz 2048) 64)))
      ((< siz 8192)
       (values 5 (quotient (- siz 4096) 128)))
      ((< siz 16384)
       (values 6 (quotient (- siz 8192) 256)))
      ((< siz 32768)
       (values 7 (quotient (- siz 16384) 512)))
      ((< siz 65536)
       (values 8 (quotient (- siz 32768) 1024)))
      ((< siz 131072)
       (values 9 (quotient (- siz 65536) 2048)))
      ((< siz 262144)
       (values 10 (quotient (- siz 131072) 4096)))
      ((< siz 524288)
       (values 11 (quotient (- siz 262144) 8192)))
      ((< siz 1048576)
       (values 12 (quotient (- siz 524288) 16384)))
      ((< siz 2097152)
       (values 13 (quotient (- siz 1048576) 32768)))
      ((< siz 4194304)
       (values 14 (quotient (- siz 2097152) 65536)))
      ((< siz 8388608)
       (values 15 (quotient (- siz 4194304) 131072)))
      ((< siz 16777216)
       (values 16 (quotient (- siz 8388608) 262144)))
      ((< siz 33554432)
       (values 17 (quotient (- siz 16777216) 524288)))
      ((< siz 67108864)
       (values 18 (quotient (- siz 33554432) 1048576)))
      ((< siz 134217728)
       (values 19 (quotient (- siz 67108864) 2097152)))
      (else (error "Huh?"))))

  (define (insert-conv cls pos)
    (values (- 19 cls) (- 31 pos)))

  (define (insert siz)
    (call-with-values 
      (lambda () (insert0 siz)) 
      (lambda (cls pos) 
        ;(display (list 'INS: siz '=> (- 19 cls) (- 31 pos))) (newline)
        (insert-conv cls pos))))

  ;; Interfaces
  (define (init)
    ;; Initialize states
    (set! l2 (make-vector 20 0))
    (set! freelist 
      (vector-map (lambda (_) (make-vector 32 -1))
                  (make-vector 20 #f)))
    ;; Initialize the first block
    ; 0: Dummy
    (iset 0 -1)      ;; Prev(None)
    (iset 1 4)       ;; the Next block
    (iset 2 -1)
    (iset 3 -1)
    ; 4: Sentinel
    (iset 4 -1) ;; Prev(None)
    (iset 5 8)  ;; Next(8)
    (iset 6 -1)
    (iset 7 -1)
    ;; The first free block
    (iset 8 4) ;; Prev(4)
    (iset 9 HEAPMAX) ;; Next(Heapmax)
    (iset 10 -1)
    (iset 11 -1)

    ;; Initialize the last block as a Sentinel
    (iset HEAPMAX -8)       ;; Prev(Non-Free)
    (iset (+ HEAPMAX 1) -1) ;; Next(None)
    ;; Initialize bitmap
    (call-with-values 
      (lambda () (insert HEAPMAX))
      (lambda (cls pos)
        (set! l1 (bts-s32 0 cls))
        (vector-set! l2 cls (bts-s32 0 pos))
        ;; Insert the first freeblock
        (vector-set! (vector-ref freelist cls) pos 8))))

  (define (%verifyloc tgt cls pos)
    (let* ((next (iref (+ tgt 1)))
           (size (- next tgt)))
      (call-with-values
        (lambda () (insert size))
        (lambda (cls0 pos0)
          (unless (and (= cls0 cls) (= pos0 pos))
            ;(display (list 'VERIF tgt size 'IN cls pos '=> 'ACTUAL cls0 pos0))
            ;(newline)
            (error "Invalid class for target block" tgt))))))

  (define (%emptyblock tgt)
    (let* ((next (iref (+ tgt 1)))
           (size (- next tgt 4)))
      (let loop ((idx 0))
        (unless (= size idx)
          (iset (+ idx tgt 4) -9191)
          (loop (+ 1 idx))))))

  (define (pickup-freeblock0 tgt cls pos)
    ;; Remove the block from freelist
    (let ((tgt-free-prev (iref (+ tgt 2)))
          (tgt-free-next (iref (+ tgt 3))))
      ;(display (list 'UNCHAIN tgt '<= '<> cls pos 'LIS tgt-free-prev tgt-free-next)) (newline)

      (%verifyloc tgt cls pos)
       
      (cond
        ((= tgt-free-prev -1)
         (vector-set! (vector-ref freelist cls) pos tgt-free-next)
         (when (= tgt-free-next -1)
           ;(display (list 'CLR cls pos)) (newline)
           ;; Clear l2 bit
           (let* ((n (vector-ref l2 cls))
                  (nn (bcl-s32 n pos)))
             (vector-set! l2 cls nn)
             (when (= nn 0)
               ;; Clear l1 bit
               (set! l1 (bcl-s32 l1 cls))))))
        (else
          (unless (= (iref (+ tgt-free-prev 3)) tgt)
            (error "Broken free list: prev-next" 
                   tgt-free-prev (iref (+ tgt-free-prev 3))))
          (iset (+ tgt-free-prev 3) tgt-free-next)))
      ;; Unchain the block from freelist
      (when (<= 0 tgt-free-next)
        (unless (= (iref (+ tgt-free-next 2)) tgt)
          (error "Broken free list: next-prev"
                 tgt-free-next (iref (+ tgt-free-next 2))))
        (iset (+ tgt-free-next 2) tgt-free-prev))))

  (define (pickup-freeblock idx)
    (let ((next (iref (+ idx 1))))
     (when (negative? next)
       (error "huh?"))
     (call-with-values
       (lambda () (insert (- next idx)))
       (lambda (cls pos)
         (pickup-freeblock0 idx cls pos)))))

  (define (search-freeblock siz)
    (call-with-values
      (lambda () (search siz))
      (lambda (cls pos)
        (let ((tgt (vector-ref (vector-ref freelist cls) pos)))
         (when (negative? tgt)
           (error "Huh?" tgt cls pos))
         (pickup-freeblock0 tgt cls pos)
         (when (negative? (iref tgt))
           (error "Returning non-free block" tgt (iref tgt)))
         tgt))))

  (define (insert-freeblock blk siz)
    (call-with-values
      (lambda () (insert siz))
      (lambda (cls pos)
        (%verifyloc blk cls pos)
        (when (< (iref blk) -1)
          (error "Inserting non-free block!" blk (iref blk)))
        (let ((v (vector-ref freelist cls)))
         (let ((fr (vector-ref v pos)))
          (iset (+ blk 2) -1)
          (iset (+ blk 3) fr)
          ;(display (list 'CHAIN blk '<= siz cls pos 'LIS -1 fr)) (newline)
          (cond
            ((= fr -1)
             ;(display (list 'FLG cls pos))(newline)
             ;; Flag L2/L1 as we inserted a new block
             (set! l1 (bts-s32 l1 cls))
             (vector-set! l2 cls 
                          (bts-s32 (vector-ref l2 cls) pos)) 
             )
            (else
              (unless (= -1 (iref (+ fr 2)))
                (error "Something wrong" (iref fr)))
              ;(display (list 'CHAINLINK blk '=> fr)) (newline)
              (iset (+ fr 2) blk))))
         ;(display (list 'FREELIST blk '<= cls pos)) (newline)
         (vector-set! v pos blk)))))

  (define (alloc siz)
    (when (< HEAPMAX siz)
      (error "Request too large"))
    (when (> 0 siz)
      (error "Invalid request size"))
    (let ((tgt (search-freeblock siz)))
     (let ((tgt-prev (iref tgt))
           (tgt-next (iref (+ tgt 1))))
       ;; Mark the block as used
       (iset tgt (encode-prev tgt-prev))
       (let ((tgt-size (- tgt-next tgt)))
        (unless (<= siz tgt-size)
          (error "Huh?" siz tgt-size))
        (when (<= 8 (- tgt-size siz))
          ;; Split the block
          (let ((new-start (+ tgt siz))
                (new-size (- tgt-size siz)))
            ;(display (list 'SPLIT new-start new-size)) (newline)
            ;; Update nexts
            (iset (+ tgt 1) new-start)
            (iset (+ new-start 1) tgt-next)
            ;; Update prevs
            (iset new-start tgt)
            (iset tgt-next (encode-prev new-start))
            ;; Register new block as freeblock
            (insert-freeblock new-start new-size)))))
     (when (< 0 (iref tgt))
       (error "Returning free block" tgt (iref tgt)))
     tgt))

  (define (decode-prev idx)
    (cond
      ((= -1 idx) -1)
      (else 
        (unless (negative? idx)
          (error "Something wrong(double free?)" idx))
        (- idx))))
  (define (encode-prev idx)
    (cond
      ((= -1 idx) -1)
      (else 
        (when (negative? idx)
          (error "Something wrong!" idx))
        (- idx))))

  (define (free idx)
    (let ((prev (decode-prev (iref idx)))
          (next (iref (+ idx 1))))
      (let ((size (- next idx))
            (prev-free? (and (<= 0 prev) (positive? (iref prev))))
            (next-free? (and (<= 0 next) (positive? (iref next)))))
        (when prev-free?
          ;(display (list 'PREVFREE prev 'FREE idx)) (newline)
          (pickup-freeblock prev))
        (when next-free?
          ;(display (list 'NEXTFREE next 'FREE idx)) (newline)
          (pickup-freeblock next))
        ;; Connect block to the free list
        (cond
          ((and prev-free? next-free?)
           (let* ((next-next (iref (+ next 1)))
                  (newsize (- next-next prev)))
            (iset (+ prev 1) next-next)
            (iset next-next (encode-prev prev))
            (insert-freeblock prev newsize)))
          (prev-free?
            (let ((newsize (- next prev)))
             (iset (+ prev 1) next)
             (when (<= 0 next)
               (iset next (encode-prev prev)))
             (insert-freeblock prev newsize)))
          (next-free?
            (let* ((next-next (iref (+ next 1)))
                   (newsize (- next-next idx)))
              ;; Mark as free block
              (iset idx prev)
              (iset (+ idx 1) next-next)
              (when (<= 0 next-next)
                (iset next-next (encode-prev idx)))
              (insert-freeblock idx newsize)))
          (else
            ;; Mark as free block
            (iset idx prev)
            (insert-freeblock idx size)))))
    (%emptyblock idx))

  (define (dump-regions)
    (let loop ((cur '())
               (idx 8))
      (let ((hdr (iref idx))
            (next (iref (+ 1 idx))))
       (cond
         ((positive? hdr)
          (loop (cons (list hdr next #f) cur) next))
         ((= -1 next)
          (reverse cur))
         (else
           (loop (cons (list (- hdr) next #t) cur) next))))))

  (define (theAllocator sym)
    (case sym
      ((INIT) init)
      ((ALLOC) alloc)
      ((FREE) free)
      ((DUMP-REGIONS) dump-regions)
      (else (error "Unknown symbol" sym))))

  (unless (< HEAPMAX (* 1024 1024 128))
    (error "Heap size too large" HEAPMAX))
  (unless (< 32 HEAPMAX)
    (error "Heap size too small" HEAPMAX))

  theAllocator)

)
