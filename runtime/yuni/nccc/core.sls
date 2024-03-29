(library (yuni nccc core)
  (export 
    nccc-lookup
    nccc-init-embedded-bundle!)
  (import (yuni scheme)
          (yuni compat nccc)
          (yuni hashtables))

  ;; Global bundle cache <sym> => <bundle-info>
  (define %global-bundle-cache (make-symbol-hashtable))

  (define (fetch bv off typ)
    (case typ
      ((s8)
       (bv-ref/s8 bv off))
      ((s16)
       (bv-ref/s16 bv off))
      ((s32)
       (bv-ref/s32 bv off))
      ((s64)
       (bv-ref/s64 bv off))
      ((u8)
       (bv-ref/u8 bv off))
      ((u16)
       (bv-ref/u16 bv off))
      ((u32)
       (bv-ref/u32 bv off))
      ((u64)
       (bv-ref/u64 bv off))
      ((ptr)
       (bv-ref/ptr bv off))
      (else
        (error "Unrecognized type" typ))))

  (define (ptr->string ptr)
    (let loop ((cur 0)
               (acc '()))
     (let ((b (ptr-ref/u8 ptr cur)))
      (if (= 0 b)
          (list->string (reverse acc))
          (loop (+ cur 1) (cons (integer->char b) acc))))))

  (define (gen-nccc-caller/64 funcptr in* out*)
    (define inlen (length in*))
    (define outlen (length out*))
    (let* ((inbuflen (* 8 inlen))
           (outbuflen (* 8 outlen))
           (totalbuflen (+ inbuflen outbuflen)))
      (lambda inputs*
        (define buf (make-bytevector totalbuflen))
        (unless (= (length inputs*) inlen)
          (error "Invalid input count"))
        ;; Input setup loop
        (let loop ((cur 0)
                   (in-queue inputs*)
                   (queue in*))
          (let ((typ (car queue))
                (next (cdr queue)))
            (case typ
              ((s8 s16 s32 s64)
               (bv-set!/s64 buf cur (car in-queue)))
              ((u8 u16 u32 u64 ptr)
               (bv-set!/u64 buf cur (car in-queue)))
              (else
                (error "Invalid input code")))
            (if (null? next)
                (let ()
                  ;; Output
                  (nccc-call funcptr buf 0 buf inbuflen)
                  (cond ((= outlen 0) (values))
                        ((= outlen 1)
                         (fetch buf inbuflen (car out*)))
                        (else
                          (let outloop ((cur inbuflen)
                                        (out-queue out*)
                                        (acc '()))
                            (write (list 'OUTLOOP: out-queue acc)) (newline)
                            (if (null? out-queue)
                                (apply values (reverse acc))
                                (outloop (+ cur 8)
                                         (cdr out-queue)
                                         (cons
                                           (fetch buf cur (car out-queue))
                                           acc)))))))
                (loop (+ cur 8) (cdr in-queue) next)))))))

  ;; Embedded library resolver
  (define (genbundle dispatchptr count)
    (define ht-procs (make-symbol-hashtable))
    (define export-info (gen-nccc-caller/64 dispatchptr
                                            '(u64 u64)
                                            '(u64 u64 ptr u64 u64 ptr
                                                  u64 u64)))
    (define (get-argret bv offs len) ;; => list
      (let loop ((cur 0)
                 (acc '()))
        (write (list 'FETCHARG bv cur acc)) (newline)
        (if (= len cur)
            (reverse acc)
            (let ((w (bv-ref/u64 bv (+ offs (* cur 8)))))
             (loop (+ 1 cur) (cons w acc))))))

    (define (decode-citype-ref idx)
      (case idx
        ((0) bv-ref/u32)
        ((1) bv-ref/u64)
        ((4) bv-ref/s32)
        ((5) bv-ref/s64)
        ((6) bv-ref/ptr)
        (else (error "Unrecognised CITYPE" idx))))

    (define (decode-citype-set idx)
      (case idx
        ((0) bv-set!/u32)
        ((1) bv-set!/u64)
        ((4) bv-set!/s32)
        ((5) bv-set!/s64)
        ((6) bv-set!/ptr)
        (else (error "Unrecognised CITYPE" idx))))

    (define (genfunc name funcptr arg* ret*)
      (define argv (list->vector (map decode-citype-set arg*)))
      (define retv (list->vector (map decode-citype-ref ret*)))
      (define outoff (* 8 (vector-length argv)))
      (define totalbuflen (* 8 (+ (vector-length argv) (vector-length retv))))
      (define (mkbuf) (make-bytevector totalbuflen))
      (define output
        (case (vector-length retv)
          ((0) (lambda (buf) 
                 (nccc-call funcptr buf 0 buf outoff)
                 (values)))
          ((1) (lambda (buf) 
                 (nccc-call funcptr buf 0 buf outoff)
                 (let ((v ((vector-ref retv 0) buf outoff)))
                  v)))
          (else (error "UNIMPL: output"))))
      (case (vector-length argv)
        ((0) (lambda ()
               (let ((buf (mkbuf)))
                (write (list 'ARG0: name)) (newline)
                (output buf))))
        ((1) (lambda (x1)
               (let ((buf (mkbuf)))
                (write (list 'ARG1: name x1)) (newline)
                ((vector-ref argv 0) buf 0 x1)
                (output buf))))
        ((2) (lambda (x1 x2)
               (let ((buf (mkbuf)))
                (write (list 'ARG2: name x1 x2)) (newline)
                ((vector-ref argv 0) buf 0 x1)
                ((vector-ref argv 1) buf 8 x2)
                (output buf))))
        ((3) (lambda (x1 x2 x3)
               (let ((buf (mkbuf)))
                (write (list 'ARG3: name x1 x2 x3)) (newline)
                ((vector-ref argv 0) buf 0 x1)
                ((vector-ref argv 1) buf 8 x2)
                ((vector-ref argv 2) buf 16 x3)
                (output buf))))
        ((4) (lambda (x1 x2 x3 x4)
               (let ((buf (mkbuf)))
                (write (list 'ARG4: name x1 x2 x3 x4)) (newline)
                ((vector-ref argv 0) buf 0 x1)
                ((vector-ref argv 1) buf 8 x2)
                ((vector-ref argv 2) buf 16 x3)
                ((vector-ref argv 3) buf 24 x4)
                (output buf))))
        ((5) (lambda (x1 x2 x3 x4 x5)
               (let ((buf (mkbuf)))
                (write (list 'ARG5: name x1 x2 x3 x4 x5)) (newline)
                ((vector-ref argv 0) buf 0 x1)
                ((vector-ref argv 1) buf 8 x2)
                ((vector-ref argv 2) buf 16 x3)
                ((vector-ref argv 3) buf 24 x4)
                ((vector-ref argv 4) buf 32 x5)
                (output buf))))
        ((6) (lambda (x1 x2 x3 x4 x5 x6)
               (let ((buf (mkbuf)))
                (write (list 'ARG6: name x1 x2 x3 x4 x5 x6)) (newline)
                ((vector-ref argv 0) buf 0 x1)
                ((vector-ref argv 1) buf 8 x2)
                ((vector-ref argv 2) buf 16 x3)
                ((vector-ref argv 3) buf 24 x4)
                ((vector-ref argv 4) buf 32 x5)
                ((vector-ref argv 5) buf 40 x6)
                (output buf))))
        (else (lambda x
                (let ((buf (mkbuf)))
                 (let loop ((idx 0)
                            (q x))
                  (unless (= idx (vector-length argv))
                    ((vector-ref argv idx) buf (* 8 idx) (car q))
                    (loop (+ idx 1) (cdr q)))
                  (output buf)))))))

    (let loop ((cur 0))
     (unless (= cur count)
       (call-with-values 
         (lambda () (export-info 2 cur))
         (lambda (x0 objid nameptr x3 x4 stubptr narg nret)
           (let* ((name (ptr->string nameptr))
                  ;; arg buffer for argret fetching
                  (inbuf (make-bytevector (* 8 2))))
            (unless (and (= x0 0) (= x3 0) (= x4 0))
              (error "Invalid NCCC export"))
            ;; Fetch argument info by making directly NCCC call
            (let ((outbuf (make-bytevector (* 8 (+ 3 narg nret)))))
             (bv-set!/u64 inbuf 0 6)
             (bv-set!/u64 inbuf 8 objid)
             (nccc-call dispatchptr inbuf 0 outbuf 0)
             (unless (= 0 (bv-ref/u64 outbuf 0))
               (error "Failed to fetch argret info"))
             (unless (= narg (bv-ref/u64 outbuf 8))
               (error "Unmatched narg"))
             (unless (= nret (bv-ref/u64 outbuf 16))
               (error "Unmatched nret"))
             (let ((arg* (get-argret outbuf 24 narg))
                   (ret* (get-argret outbuf (+ 24 (* 8 narg)) nret)))
               (write (list 'FUNC: name arg* ret*)) (newline)
               (let ((func (genfunc name stubptr arg* ret*)))
                (hashtable-set! ht-procs (string->symbol name) func)))))))
       (loop (+ cur 1))))
    ht-procs)

  (define (import-dispatch! dispatchptr)
    (define libinfo (gen-nccc-caller/64 dispatchptr 
                                        '(u64) 
                                        '(ptr u64 u64 u64 u64 u64)))
    (call-with-values
      (lambda () (libinfo 1))
      (lambda (strptr x1 f0-stub-count x3 x4 x5)
        (let ((libsym (string->symbol (ptr->string strptr))))
         (hashtable-set! %global-bundle-cache
                         libsym
                         (genbundle dispatchptr f0-stub-count))))))

  (define (nccc-init-embedded-bundle!)
    (let loop ((cur 0))
     (let ((dispatch (nccc-get-dispatch cur)))
      (unless (or (not dispatch) (= (ptr->integer dispatch) 0))
        (import-dispatch! dispatch)
        (loop (+ cur 1))))))

  (define (nccc-lookup lib funcname)
    (let ((libht (hashtable-ref %global-bundle-cache lib #f)))
     (and libht
          (hashtable-ref libht funcname #f))))


  )
