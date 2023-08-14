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
    address->bytevector bytevector->address
    nccc-call nccc-sizeof-ptr
    nccc-get-dispatch)

  (import (rvm-primitives)))
