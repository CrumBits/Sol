#lang racket

(provide (all-defined-out)
         (all-from-out "parser.rkt"))
(require "parser.rkt")

(define-syntax (sexp f)
  (syntax-case f ()
               ((_ name e ...)
                #'(seq name
                       (drop (lit "("))
                       (drop (star (whitespace)))
                       e ...
                       (drop (star (whitespace)))
                       (drop (lit ")"))))))

(define digit (char-range "0-9"))

(define cid (plus (alt (char-range "a-z")
                             (char-range "A-Z")
                             (char-range "0-9")
                             (lit "_")
                             (lit "<")
                             (lit ":")
                             ;(lit "(")
                             ;(lit ")") ; Needed for function pointers...
                             (lit "*")
                             (lit "/")
                             (lit ">"))))

(define typ (seq 'type
                  (drop (lit "["))
                  cid
                  (drop (lit "]"))))

(define plain-int (plus digit))
(define pos-int (seq 'pos-int (opt (drop (lit "+"))) plain-int))
(define neg-int (seq 'neg-int (drop (lit "-")) plain-int))
(define int (alt neg-int pos-int))

(define float (seq 'float
                   int
                   (lit ".")
                   (opt plain-int)
                   (opt (seq 'exp
                             (lit "e")
                             int))))

(define number (alt float int))

(define symb-pretokens (alt
                         (lit "?")
                         (lit "!")
                         (lit "+")
                         (lit "-")
                         (lit "*")
                         (lit "/")
                         (lit ":")))

(define symb-posttokens (alt (char-range "0-9") symb-pretokens))

(define symb-tokens (alt
                      (char-range "a-z")
                      (char-range "A-Z")))

(define symb (seq 'symb (star symb-pretokens) (plus symb-tokens) (star symb-posttokens)))

(define symbexpr (sexp 'symbexpr (drop (lit "quote")) (drop (plus (whitespace))) symb))

(define slstr (seq 'str
                 (drop (lit "\""))
                 (star (alt
                         (whitespace)
                         (lit "\\\"")
                         (char-range "^\"")))
                 (drop (lit "\""))))

(define str (alt slstr slstr))

(define native (sexp 'native
                    (drop (lit "native"))
                    (drop (star (whitespace)))
                    str))

(define native-declare (sexp 'native-declare
                             (drop (lit "native-declare"))
                             (drop (star (whitespace)))
                             str))

(define term (alt number symbexpr symb str))

(define def (sexp 'def
                  (drop (lit "define"))
                  (drop (plus (whitespace)))
                  (or-error symb)
                  (drop (plus (whitespace)))
                  'subexpr))

(define defclass (sexp 'defclass
                       (drop (lit "define-class"))
                       (drop (plus (whitespace)))
                       (or-error symb)
                       (drop (plus (whitespace)))
                       (opt (sexp 'parent-classes
                                  (plus
                                    (seq 'parent-seq
                                         (drop (star (whitespace)))
                                         (sexp 'parent-class
                                               (alt (lit "public")
                                                    (lit "private"))
                                               (drop (plus (whitespace)))
                                               cid)))))
                       (drop (star (whitespace)))
                       (sexp 'class-content
                             (sexp 'public-class-content
                                   (drop (lit "public"))
                                   (plus (seq 'class-body
                                              (drop (plus (whitespace)))
                                              def)))
                             (opt (sexp 'private-class-content
                                        (drop (lit "private"))
                                        (plus (seq 'class-body
                                                   (drop (plus (whitespace)))
                                                   def)))))))

(define func-arg (seq 'arg (drop (star (whitespace))) term (drop (star (whitespace))) (opt typ) (drop (star (whitespace)))))
(define func (sexp 'lambda
                   (drop (lit "lambda"))
                   (drop (plus (whitespace)))
                   (drop (lit "("))
                   (opt (star func-arg))
                   (drop (lit ")"))
                   (drop (plus (whitespace)))
                   'subexpr))

(define appl (sexp 'apply
                   (alt symb func 'appl)
                     (star
                       (seq 'apply-arg
                            (drop (plus (whitespace)))
                            'subexpr))))

(define modimport (sexp 'import (drop (lit "import")) (drop (plus (whitespace))) cid))

(define subexpr (alt native func term appl))
(define expr (seq 'expr
                  (drop (star (whitespace)))
                  (alt modimport defclass def native-declare subexpr)
                  (drop (star (whitespace)))))

(define program (seq 'program
                     (plus expr)
                     (drop (eos))))

(push-parser 'func func)
(push-parser 'subexpr subexpr)
(push-parser 'appl appl)
