(library (yuni compat nccc)

  (export
    bv-ref/s8 bv-ref/u8
    bv-ref/s16 bv-ref/u16
    bv-ref/s32 bv-ref/u32
    bv-ref/s64 bv-ref/u64
    bv-set!/s8 bv-set!/u8
    bv-set!/s16 bv-set!/u16
    bv-set!/s32 bv-set!/u32
    bv-set!/s64 bv-set!/u64
    bv-ref/ptr bv-set!/ptr

    ptr-ref/s8 ptr-ref/u8
    ptr-ref/s16 ptr-ref/u16
    ptr-ref/s32 ptr-ref/u32
    ptr-ref/s64 ptr-ref/u64
    ptr-set!/s8 ptr-set!/u8
    ptr-set!/s16 ptr-set!/u16
    ptr-set!/s32 ptr-set!/u32
    ptr-set!/s64 ptr-set!/u64
    ptr-ref/ptr ptr-set!/ptr

    integer->ptr
    ptr->integer

    nccc-call nccc-sizeof-ptr
    nccc-get-dispatch)

  (import (yuni scheme) (rvm-primitives))

  ;; pointer object
  (define ident-ptr (list 'POINTER))
  
  (define (make-ptr i bv length?)
    (vector ident-ptr i bv length?))

  (define (%true-ptr? obj)
    (and (vector? obj)
         (eq? (vector-ref obj 0) ident-ptr)))

  (define (integer->ptr i)
    (make-ptr i #f #f))

  (define (bytevector->ptr bv)
    (let ((addr (bytevector->address bv))
          (len (bytevector-length bv)))
      (make-ptr addr bv len)))

  (define (ptr->bytevector ptr?)
    (if (integer? ptr?)
        (address->bytevector ptr? #f)
        (let ((cache (vector-ref ptr? 2)))
         (if cache
             cache
             (begin
               (let* ((addr (vector-ref ptr? 1))
                      (bv (address->bytevector addr #f)))
                 (vector-set! ptr? 2 bv)
                 bv))))))

  (define (ptr->integer ptr)
    (cond
      ((integer? ptr) ptr)
      ((%true-ptr? ptr)
       (let ((cache (vector-ref ptr 1)))
        (if cache
            cache
            (begin
              (let* ((bv (vector-ref ptr 2))
                     (addr (bytevector->address bv)))
               (vector-set! ptr 1 addr)
               (vector-set! ptr 3 (bytevector-length bv))
               addr)))))
      (else
        (error "Pointer required" ptr))))

  (define bv-ref/ptr bv-ref/ptr0)

  (define (bv-set!/ptr bv offs ptr)
    (let ((i (ptr->integer ptr)))
     (bv-set!/ptr0 bv offs i)))

  ;; Ref/set against ptr
  (define (ptr-ref/u8 ptr offs)
    (let ((bv (ptr->bytevector ptr)))
     (bv-ref/u8 bv offs)))

  (define (ptr-ref/s8 ptr offs)
    (let ((bv (ptr->bytevector ptr)))
     (bv-ref/s8 bv offs)))

  (define (ptr-ref/u16 ptr offs)
    (let ((bv (ptr->bytevector ptr)))
     (bv-ref/u16 bv offs)))

  (define (ptr-ref/s16 ptr offs)
    (let ((bv (ptr->bytevector ptr)))
     (bv-ref/s16 bv offs)))

  (define (ptr-ref/u32 ptr offs)
    (let ((bv (ptr->bytevector ptr)))
     (bv-ref/u32 bv offs)))

  (define (ptr-ref/s32 ptr offs)
    (let ((bv (ptr->bytevector ptr)))
     (bv-ref/s32 bv offs)))

  (define (ptr-ref/u64 ptr offs)
    (let ((bv (ptr->bytevector ptr)))
     (bv-ref/u64 bv offs)))

  (define (ptr-ref/s64 ptr offs)
    (let ((bv (ptr->bytevector ptr)))
     (bv-ref/s64 bv offs)))

  (define (ptr-ref/ptr ptr offs)
    (let ((bv (ptr->bytevector ptr)))
     (bv-ref/ptr bv offs)))

  (define (ptr-set!/u8 ptr offs v)
    (let ((bv (ptr->bytevector ptr)))
     (bv-set!/u8 bv offs v)))

  (define (ptr-set!/s8 ptr offs v)
    (let ((bv (ptr->bytevector ptr)))
     (bv-set!/s8 bv offs v)))

  (define (ptr-set!/u16 ptr offs v)
    (let ((bv (ptr->bytevector ptr)))
     (bv-set!/u16 bv offs v)))

  (define (ptr-set!/s16 ptr offs v)
    (let ((bv (ptr->bytevector ptr)))
     (bv-set!/s16 bv offs v)))

  (define (ptr-set!/u32 ptr offs v)
    (let ((bv (ptr->bytevector ptr)))
     (bv-set!/u32 bv offs v)))

  (define (ptr-set!/s32 ptr offs v)
    (let ((bv (ptr->bytevector ptr)))
     (bv-set!/s32 bv offs v)))

  (define (ptr-set!/u64 ptr offs v)
    (let ((bv (ptr->bytevector ptr)))
     (bv-set!/u64 bv offs v)))

  (define (ptr-set!/s64 ptr offs v)
    (let ((bv (ptr->bytevector ptr)))
     (bv-set!/s64 bv offs v)))

  (define (ptr-set!/ptr ptr offs v)
    (let ((bv (ptr->bytevector ptr)))
     (bv-set!/ptr bv offs v)))


  ;; nccc-call
  (define (callbuffilt buf)
    (cond
      ((bytevector? buf) buf)
      ((%true-ptr? buf) (ptr->bytevector buf))
      (else
        (error "Invalid buffer" buf))))

  (define (nccc-call funcptr in inoffs out outoffs)
    (nccc-call0 (ptr->integer funcptr) 
                (callbuffilt in) inoffs
                (callbuffilt out) outoffs))

  ;; Embedded NCCC library
  (define (nccc-get-dispatch idx) 
    (let ((i (nccc-get-dispatch0 idx)))
     (if (> i 0)
         (integer->ptr i)
         #f)))

  
  )
