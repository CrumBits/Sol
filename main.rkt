#lang racket

(require "language.rkt")
(require "compiler.rkt")
(require "lexer.rkt")
(require "cpp-backend.rkt")

(define parse-tree (parse program
                          (current-input-port)))
(begin
  (displayln parse-tree)
  (if (void? parse-tree)
      (displayln "Failed to parse")
      (let ((ast (lex parse-tree)))
       (if (string? ast)
           (begin
             (displayln "During lexing")
             (displayln ast))
           (begin
             (displayln ast)
             (let ((atree (compile ast "toplevel")))
              (begin
                (displayln atree)
                (let-values (((header source) (atree->c++ atree "toplevel" #t)))
                            (begin
                              (displayln header)
                              (displayln "//////////////////////")
                              (displayln source))))))))))
