(library (ribbon util mergebundle)
  (export merge-bootstrap-bundle!)
  (import (yuni scheme)
          (yuni hashtables))

  (define macro-libs
    '((yunivm-core-syntax)
      (r7c heap listloop) ;; quasiquote
      (rvm reader-runtime)
      (rvm exception-runtime)))

  (define visible-libs
    '((rvm-primitives)
      (rvm exception-runtime)
      (r7c heap listloop)
      (rvm reader-runtime)))

  (define (merge-bootstrap-bundle! bundle) ;; => merged bundle
    (define rootsym #f)
    (define ht-libs (make-symbol-hashtable))
    (define ht-visited (make-symbol-hashtable))
    (define ht-destination (make-symbol-hashtable)) ;; libsym => targetsym
    (define libnames '())
    (define deleted-code* '())
    (define deleted-macro* '())
    (define deleted-macronames '())
    (define deleted-imports '())
    (define ht-deleted-import (make-symbol-hashtable)) ;; libsym => libname

    (define (lookup libname) ;; => libsym
      (let loop ((q libnames))
       (when (null? q)
         (error "Library not found" libname))
       (let ((a (caar q))
             (lib (cdar q))
             (next (cdr q)))
         (if (equal? a libname)
             lib
             (loop next)))))

    ;; Pass1: Read all libraries in bundle
    (for-each (lambda (lib)
                (let ((libname (vector-ref lib 0))
                      (libsym (vector-ref lib 1)))
                 (hashtable-set! ht-libs libsym lib)
                 (hashtable-set! ht-visited libsym #f)
                 (hashtable-set! ht-destination libsym #f)
                 (set! libnames (cons (cons libname libsym) libnames))
                 (when (equal? libname '(yuni scheme))
                   (set! rootsym libsym))))
              bundle)

    ;; Pass2: Collect deletion candidates 
    ;; FIXME: Currently unused (delete side libraries anyway)
    (let enter! ((tgt rootsym))
     (let ((lib (hashtable-ref ht-libs tgt #f)))
      (when lib
        (unless (hashtable-ref ht-visited tgt #f)
          (hashtable-set! ht-visited tgt #t)
          (hashtable-set! ht-destination tgt 'delete)
          (let ((import* (vector-ref lib 2)))
           (for-each enter! import*))))))
    (hashtable-set! ht-destination rootsym 'root)

    ;; Pass3: Rescue visible-libs
    (for-each (lambda (lib)
                (let* ((libname (vector-ref lib 0))
                       (libsym (vector-ref lib 1))
                       (a (member libname visible-libs)))
                  (when (eq? libname #t)
                    ;; FIXME: bug?
                    (hashtable-set! ht-destination '**program** 'visible))
                  (when a
                    (hashtable-set! ht-destination libsym 'visible))))
              bundle)

    ;; Pass4: Collect deleted macro and codes
    (for-each (lambda (lib)
                (let ((libsym (vector-ref lib 1)))
                 (case (hashtable-ref ht-destination libsym #f)
                   ((visible delete #f) ;; FIXME fold ?
                    (unless (eq? libsym '**program**)
                      (let ((macro* (vector-ref lib 7))
                            (prog* (vector-ref lib 5))
                            (macronames (vector-ref lib 6))
                            (importnames (vector-ref lib 3)))
                        (write (list 'MERGE: libsym)) (newline)
                        (for-each (lambda (n)
                                    (unless (lookup n)
                                      (error "External import" n)))
                                  importnames)
                        (set! deleted-code* (append deleted-code* prog*))
                        (set! deleted-macro* (append deleted-macro* macro*))
                        (set! deleted-macronames (append deleted-macronames
                                                         macronames)))))

                   (else 'do-nothing))))
              bundle)

    ;; Pass5: output
    (let loop ((acc '())
               (q bundle))
      (if (null? q)
          (reverse acc)
          (let* ((next (cdr q))
                 (lib (car q))
                 (libname (vector-ref lib 0))
                 (libsym (vector-ref lib 1))
                 (import* (vector-ref lib 2))
                 (destination (hashtable-ref ht-destination libsym #f)))
            (write (list 'FILTER: libname libsym destination)) (newline)
            (case destination
              ((root visible)
               ;; Remove deleted library for import* and import
               (let ((new-import* '())
                     (new-imports '()))
                 (for-each (lambda (insym)
                             (let ((indest (hashtable-ref ht-destination
                                                          insym #f)))
                               (unless (or (eq? indest 'delete)
                                           (eq? indest #f))
                                 (set! new-import* (cons insym new-import*))
                                 (let* ((inlib (hashtable-ref ht-libs insym
                                                              #f))
                                        (inname (vector-ref inlib 0)))
                                   (write (list 'NEEDED: inname)) (newline)
                                   (set! new-imports (cons inname 
                                                           new-imports))))))
                           import*)

                 (when (eq? 'root destination)
                   ;; FIXME: Inject macro library
                   (let ((msyms (map lookup macro-libs)))
                    (set! new-import* (append new-import* msyms))
                    (set! new-imports (append new-imports macro-libs))))

                 (vector-set! lib 2 new-import*)
                 (vector-set! lib 3 new-imports)
                 (write (list 'DEBUG: (vector-ref lib 6))) (newline)
                 ;; Adjust macro/code content
                 (case destination
                   ((root) ;; Merge macro 
                    (let ((macro* (vector-ref lib 7))
                          (code* (vector-ref lib 5))
                          (macronames (vector-ref lib 6)))
                      (write (list 'PERMERGE: (length code*) (length macro*)))
                      (newline)
                      (vector-set! lib 5
                                   (append deleted-code* code*))
                      (vector-set! lib 7
                                   (append deleted-macro* macro*))
                      (vector-set! lib 6
                                   (append deleted-macronames macronames))
                      (write (list 'MERGED: 
                                   (length (vector-ref lib 5))
                                   (length (vector-ref lib 7)))) (newline)
                      ))
                   ((visible) ;; Delete prog
                    (unless (eq? libsym '**program**)
                      (vector-set! lib 5 '())
                      (vector-set! lib 7 '())))
                   (else 'do-nothing))
                 ;; go next
                 (loop (cons lib acc) next)))
              ((delete #f)
               ;; Remove from list
               (loop acc next))
              (else (error "Unexpected")))))))
  )
