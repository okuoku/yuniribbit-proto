(library (r7c heap char) ;; FIXME: Undocumented
  (export char->integer
          $char=?)
  (import (rsc-core-syntax)
          (rvm-primitives))

  (define ($char=? a b)
    (= (char->integer a) (char->integer b)))
  
  )
