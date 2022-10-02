;; FIXME: Dummy yuniconfig library
(library (yuniconfig build)
         (export
           yuniconfig-platform)
         (import (yuni scheme))

(define (yuniconfig-platform)
  "Dummy platform"
  )
)
