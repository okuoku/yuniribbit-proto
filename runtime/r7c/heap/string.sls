(library (r7c heap string)
  (export string?
          string? string-length string-ref string-set!
          $string=? $make-string
          $string-fill! $string-copy!)
  (import (rvm-primitives)
          (r7c heap listloop)
          (r7c heap pair)
          (rsc-core-syntax))

  ;; string-type = 3
  (define (string? str)
    (eqv? 3 (field2 str)))

  (define (require-str str)
    (if (string? str)
        #t
        (error "String required" str)))

  (define (string-length str)
    (require-str str)
    (vec-length str))

  (define (string-ref str o)
    (require-str str)
    (vec-ref str o))

  (define (string-set! str o c)
    (require-str str)
    (vec-set! str o c))

  (define ($string=? str1 str2)
    (require-str str1 str2)
    (vec= str1 str2))

  (define ($string-fill! str fill from to)
    (require-str str)
    (vec-fill! str fill from to))

  (define ($string-copy! dest pos src offs len)
    (require-str dest)
    (require-str src)
    (vec-copy! dest pos src offs len))
  )
