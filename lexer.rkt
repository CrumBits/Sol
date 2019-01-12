#lang racket

(provide lex)
(require racket/match)

(define (lex parse-tree)
  (letrec ((inner-fn (lambda (subtree statetree ret)
                       (begin
                         ;(displayln (car subtree))
                       (match (car subtree)
                              ('nothing (ret '()))
                              ('program (inner-fn (cadr subtree) statetree ret))
                              ('import (inner-fn (cadr subtree) statetree (lambda (r) (ret (list 'import (string-join (cdr r) ""))))))
                              ('symb (inner-fn (cadr subtree)
                                               statetree
                                               (lambda (r)
                                                 (let ((prefix "")
                                                       (postfix "")
                                                       (fix (foldr
                                                                string-append
                                                                ""
                                                                (cdr r))))
                                                   (if (not (empty? (cddr subtree)))
                                                       (inner-fn (caddr subtree)
                                                                 statetree
                                                                 (lambda (r2)
                                                                   (begin
                                                                     (set! postfix (foldr
                                                                                     string-append
                                                                                       ""
                                                                                       (cdr r2)))
                                                                     (if (not (empty? (cdddr subtree)))
                                                                         (inner-fn (cadddr subtree)
                                                                                   statetree
                                                                                   (lambda (r3)
                                                                                     (begin
                                                                                       (set! prefix fix)
                                                                                       (set! fix postfix)
                                                                                       (set! postfix (foldr
                                                                                                       string-append
                                                                                                         ""
                                                                                                         (cdr r3)))
                                                                                       (ret `(symb ,(string-append
                                                                                                      prefix
                                                                                                      fix
                                                                                                      postfix))))))
                                                                         (ret `(symb ,(string-append
                                                                                        prefix
                                                                                        fix
                                                                                        postfix)))))))
                                                 (ret `(symb ,(string-append
                                                               prefix
                                                               fix
                                                               postfix))))))))
                              ('symbexpr (inner-fn (cadr subtree)
                                                   statetree
                                                   (lambda (r)
                                                     (ret `(symbol ,(cadr r))))))
                              ('def (inner-fn (cadr subtree)
                                              statetree
                                              (lambda (r)
                                                (inner-fn (caddr subtree)
                                                          statetree
                                                          (lambda (r2)
                                                            (ret `(def ,r ,r2)))))))
                              ('expr (inner-fn (cadr subtree) statetree ret))
                              ('neg-int (inner-fn (cadr subtree) statetree
                                                  (lambda (r)
                                                    (ret
                                                      (list
                                                        'int
                                                        (- (string->number
                                                          (apply
                                                            string-append
                                                            (cdr r)))))))))
                              ('float (inner-fn (cadr subtree) statetree
                                                (lambda (rval)
                                                  (inner-fn (list 'pos-int (cadddr subtree)) statetree
                                                            (lambda (rmant)
                                                              (if (empty? (cddddr subtree))
                                                                  (ret `(float ,@(list (string->number
                                                                                            (string-append
                                                                                              (number->string (cadr rval))
                                                                                              "."
                                                                                              (number->string (cadr rmant)))))))
                                                                  (inner-fn (car (cddddr subtree)) statetree
                                                                            (lambda (rexp)
                                                                              (ret `(float ,@(list (string->number
                                                                                                        (string-append
                                                                                                          (number->string (cadr rval))
                                                                                                          "."
                                                                                                          (number->string (car rmant))
                                                                                                          "e"
                                                                                                          (number->string (cadr rexp)))))))))))))))
                                                                        
                              ('exp (inner-fn (caddr subtree) statetree ret))
                              ('type (inner-fn (cadr subtree) statetree
                                               (lambda (r)
                                                 (ret `(type ,(foldr
                                                                string-append
                                                                   ""
                                                                   (cdr r)))))))
                              ('native (inner-fn (cadr subtree) statetree
                                                 (lambda (r) (ret `(native ,r)))))
                              ('native-declare (inner-fn (cadr subtree) statetree
                                                 (lambda (r) (ret `(native-declare ,r)))))
                              ('defclass (begin (displayln (cadr subtree))
                                                (inner-fn (cadr subtree) statetree
                                                   (lambda (r)
                                                     (inner-fn (cons 'lst (cddr subtree)) statetree
                                                               (lambda (r2)
                                                                 (ret `(defclass ,r ,@(cdr r2)))))))))
                              ('parent-classes (inner-fn (cadr subtree) statetree
                                                         (lambda (r)
                                                           (if (empty? (cddr subtree))
                                                               (ret `(parent-classes ,(list 'seq r)))
                                                               (inner-fn (cons 'parent-classes (cddr subtree)) statetree
                                                                         (lambda (r2)
                                                                           (ret `(parent-classes ,(list 'seq (cons r (cdr r2)))))))))))
                              ('parent-class (inner-fn (cadr subtree) statetree
                                                       (lambda (r)
                                                         (inner-fn (caddr subtree) statetree
                                                                   (lambda (r2)
                                                                     (ret `(parent-class ,r ,(string-join (cdr r2) ""))))))))
                              ('class-content (inner-fn (cadr subtree) statetree
                                                        (lambda (r)
                                                          (if (empty? (cddr subtree))
                                                              (ret `(class-content ,r))
                                                              (inner-fn (caddr subtree) statetree
                                                                        (lambda (r2)
                                                                          (ret `(class-content ,r ,r2))))))))
                              ('public-class-content (inner-fn (cadr subtree) statetree
                                                               (lambda (r)
                                                                 (ret `(public-class-content ,r)))))
                              ('private-class-content (inner-fn (cadr subtree) statetree
                                                                (lambda (r)
                                                                  (ret `(private-class-content ,r)))))
                              ('class-body (inner-fn (cadr subtree) statetree ret))
                              ('parent-seq (inner-fn (cadr subtree) statetree ret))
                              ('str (inner-fn (cadr subtree) statetree
                                              (lambda (r) (ret `(str ,(foldr string-append "" (cadr r)))))))
                              ('arg (inner-fn (cadr subtree) statetree (lambda (r)
                                                               (if (empty? (cddr subtree))
                                                                   (ret `(typed-symb
                                                                           ,r
                                                                           "auto"))
                                                                   (inner-fn (caddr subtree) statetree
                                                                             (lambda (rtype)
                                                                               (ret `(typed-symb
                                                                                       ,r
                                                                                       ,(cadr rtype)))))))))
                              ('lambda (inner-fn (cadr subtree) statetree
                                                 (lambda (r)
                                                   (inner-fn (caddr subtree) statetree
                                                             (lambda (r2)
                                                               (ret `(lambda (arglist ,@(cdr r)) ,r2)))))))
                              ('apply-arg (inner-fn (cadr subtree) statetree ret))
                              ('apply (inner-fn (cadr subtree) statetree
                                                (lambda (r)
                                                  (if (empty? (cddr subtree))
                                                      (ret `(apply ,r (applynil)))
                                                      (inner-fn (caddr subtree) statetree
                                                                (lambda (r2)
                                                                  (ret `(apply ,r (applylist ,@(cdr r2))))))))))
                              ('pos-int (inner-fn (cadr subtree) statetree (lambda (r) (ret
                                                                               (list
                                                                                 'int
                                                                                 (string->number
                                                                                   (apply
                                                                                     string-append
                                                                                     (cdr r))))))))
                              ('lst (inner-fn (cons '%lst (cdr subtree)) statetree (lambda (r) (ret `(seq ,@r)))))
                              ('lit (ret (cadr subtree)))

                              ; Internal forms
                              ('%lst (inner-fn (cadr subtree) statetree
                                               (lambda (r)
                                                 (if (empty? (cddr subtree))
                                                     (ret (list r))
                                                     (inner-fn (cons '%lst (cddr subtree)) statetree
                                                               (lambda (r2)
                                                                 (ret (cons r r2))))))))
                              
                              (else (cons 'error:unknown-symbol (car subtree))))))))
    (inner-fn parse-tree #f (lambda (x) x))))
