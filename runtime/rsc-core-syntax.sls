(library (rsc-core-syntax)
  (export
    ;; rsc specific => (rsc-core-syntax/primitives)
    let let* letrec
    define
    and or cond

    ;; In (yunivm-core-syntax)
    if begin lambda set! quote letrec*
    syntax-rules ... => else define-syntax

    ;; Should not be used but be explicit...
    $define/core)
  (import (rsc-core-syntax/primitives)
          (yunivm-core-syntax)))
