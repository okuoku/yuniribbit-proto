(library (yuniribbit util debug-expand)
         (export debug-expand/define
                 debug-expand
                 )
         (import (yuni scheme))

(define-syntax debug-expand/define
  (syntax-rules ()
    ((_ forms ...)
     (define __1 #f)
     ;(begin forms ...)
     )))         

(define-syntax debug-expand
  (syntax-rules ()
    ((_ forms ...)
     #f
     ;(begin forms ...)
     )))         
         
)
