(import (yuni scheme)
        ;(yuniribbit rvm)
        (yuniribbit rsc)
        (yuni compat ident))

(when (eq? (ident-impl) 'cyclone)
  (error "Cyclone: not yet ported."))

(run-rsc "-vvv" "-l" "max-tc.scm" "repl-max.scm")
