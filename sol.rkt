#lang racket

(require "language.rkt")
(require "compiler.rkt")
(require "lexer.rkt")
(require "cpp-backend.rkt")

(require racket/cmdline)

(define compiler (make-parameter "g++"))
(define link-flags (make-parameter '()))
(define compile-flags (make-parameter '()))
(define verbose (make-parameter #f))

(define files-to-compile
  (command-line
   #:program "compiler"
   #:once-each
   [("-v" "--verbose") "Be verbose" (verbose #t)]
   
   #:once-each
   [("-x" "--compiler") c
                        "Use <c> as the compiler"
                        (compiler c)]
   #:multi
   [("-l" "--link-flags") lf ; flag takes one argument
                          "Add a flag <lf> for the linker"
                          (link-flags (cons lf (link-flags)))]
   #:multi
   [("-c" "--compile-flags") cf
                          "Add a flag <cf> for the compiler"
                          (compile-flags (cons cf (compile-flags)))]

   #:args (filename . filenames)
   (cons filename filenames)))

(define (fname->modname fname)
  (string-join
    (drop-right
      (string-split (string-trim fname) "." #:trim? #f #:repeat? #t)
      1)
    "."))

(when (verbose)
  (displayln files-to-compile))

(when (verbose)
  (with-input-from-file (car files-to-compile)
                        (lambda ()
                          (displayln (read-string 99999)))))

(define parse-trees (map (lambda (x)
                           (call-with-input-file
                             x
                             (lambda (p)
                               (list
                                 (begin (reset-parser!)
                                        (parse program p))
                                 (fname->modname x)))))
                         files-to-compile))

(when (verbose)
  (displayln parse-trees))

(define lex-trees (map (lambda (x) (list (lex (car x)) (cadr x))) parse-trees))

(when (verbose)
  (displayln lex-trees))

(define atrees (map (lambda (x)
                      (begin
                        (reset-compiler!)
                        (list
                          (compile (car x) (cadr x))
                          (cadr x))))
                    lex-trees))

(when (verbose)
  (displayln atrees))

(define programs
    (reverse
      (cons
        (let-values (((header source)
                      (begin
                        (reset-backend!)
                        (atree->c++ (car (last atrees)) (cadr (last atrees)) #t))))
                    (list (list header source) (cadr (last atrees))))
        (map (lambda (x)
               (let-values (((header source)
                             (begin
                               (reset-backend!)
                               (atree->c++ (car x) (cadr x) #f))))
                           (list (list header source) (cadr x))))
             (drop-right atrees 1)))))

(map (lambda (x)
       (begin
         (call-with-output-file
           (string-append (cadr x) ".h")
           (lambda (f)
             (fprintf f (caar x)))
           #:exists 'replace)
         (call-with-output-file
           (string-append (cadr x) ".cpp")
           (lambda (f)
             (fprintf f (cadar x)))
           #:exists 'replace)))
     programs)
