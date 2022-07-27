(library (yuniribbit util debug-expand)
         (export debug-expand)
         (import (yuni scheme))

(define-syntax debug-expand
  (syntax-rules ()
    ((_ forms ...)
     (begin forms ...))))         
         
)
