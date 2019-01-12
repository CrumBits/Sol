#lang racket

(provide atree->c++
         reset-backend!)
(require racket/match)
(require "compiler.rkt")

(define (reset-backend!)
  #f)

(define (ctor-def def kname)
  (let ((dummy-node (<atree-node>
                      (<atree-node>-tag def)
                      (<atree-node>-type def)
                      (<atree-node>-name def)
                      kname
                      (<atree-node>-children def))))
    (string-append
      (fn-qualified-string dummy-node)
      ";")))

(define (dtor-text def kname nodes)
  (let ((dummy-node (<atree-node>
                      (<atree-node>-tag def)
                      (<atree-node>-type def)
                      (<atree-node>-name def)
                      kname
                      (<atree-node>-children def))))
    (string-append
      "~~" kname "::" kname "() {\n"
      (fn-content-string dummy-node)
      (let ((bindings (if (and nodes (not (empty? nodes)))
                          (gather-bindings nodes)
                          '())))
       (string-join
         (map (lambda (x)
                (string-append
                  "//"
                  "delete "
                  (cadr x)
                  ""
                  ";\n"))
              bindings)
         ""))
      "}\n")))

(define (ctor-text def kname nodes)
  (let ((dummy-node (<atree-node>
                      (<atree-node>-tag def)
                      (<atree-node>-type def)
                      (<atree-node>-name def)
                      kname
                      (<atree-node>-children def))))
    (string-append
      (fn-qualified-string dummy-node)
      " {\n"
      (let ((bindings (if (and nodes (not (empty? nodes)))
                          (gather-bindings nodes)
                          '())))
       (string-join
         (map (lambda (x)
                (string-append
                  (cadr x)
                  " = "
                  (caddr x)
                  ";\n"))
              bindings)
         ""))
      (fn-content-string dummy-node)
      "}\n")))

(define (fn-string f)
  (string-append
    (last (<atree-node>-type f))
    " "
    (fn-substring f)))

(define (fn-substring f)
  (string-append
    (<atree-node>-content f)
    "("
    (string-join
      (map (lambda (x y) (string-append x " " y))
           (cadr (<atree-node>-type f))
           (let ((children (<atree-node>-children (car (<atree-node>-children f)))))
            (map <atree-node>-content children)))
      ", ")
    ")"))

(define (fn-qualified-string f)
  (string-append
    (last (<atree-node>-type f))
    " "
    (if (eq? (<atree-node>-tag f) 'anon-meth)
        (string-append
          (<atree-node>-content (last (<atree-node>-children f)))
          "::")
        "")
    (fn-substring f)))


(define (fn-node->header f)
  (string-append
    (fn-string f)
    ";"))

(define (fn-content-string f)
  (let ((content (map <atree-node>-content (<atree-node>-children (cadr (<atree-node>-children f))))))
   (string-append
     (string-join 
       (map (lambda (x) (string-append x ";"))
            (drop-right content 1))
       "\n")
     (match (<atree-node>-tag (last (<atree-node>-children (cadr (<atree-node>-children f)))))
            ((or 'lit 'symb 'anon-func 'anon-meth) (string-append "return " (last content) ";\n"))
            ((or 'binding 'func-binding) "")))))

(define (fn-node->source f)
  (string-append
    (fn-qualified-string f)
    " {\n"
    (fn-content-string f)
    "}\n"))

(define (class-string subtree)
  (letrec ((inner-fn (lambda (subtree)
                       (match (<atree-node>-tag subtree)
                              ('class-parents (list (string-join (map (lambda (x)
                                                                        (car (inner-fn
                                                                               (car (<atree-node>-children x)))))
                                                                      ;; Each element is within a *-class-parent seq
                                                                      (<atree-node>-children
                                                                        (car (<atree-node>-children subtree))))
                                                                 ;; The one child of a class-parents is a seq of *-class-parent's
                                                                 ", ") #f))
                              ('public-class-parent (list (string-append "public " (<atree-node>-content subtree)) #f))
                              ('private-class-parent (list (string-append "private " (<atree-node>-content subtree)) #f))))))

    (let ((has-parent (car (<atree-node>-children subtree))))
     (string-append
       "class "
       (<atree-node>-content subtree)
       (if has-parent
           (string-append " : "
                          (car (inner-fn (car (<atree-node>-children subtree)))))
           "")))))

(define (gather-bindings tree)
  (letrec ((inner-fn (lambda (subtree visited)
                       (if (or (list? subtree)
                               (member subtree visited))
                           '()
                           (match (<atree-node>-tag subtree)
                                  ('binding
                                   (let ((types (map <atree-node>-type (<atree-node>-children subtree)))
                                         (symbval (map node->header (<atree-node>-children subtree)))
                                         (children-bindings (foldr (lambda (x accum)
                                                                     (append
                                                                       (inner-fn
                                                                         x
                                                                         (cons subtree visited))
                                                                       accum))
                                                                   '()
                                                                   (<atree-node>-children subtree))))
                                     (cons
                                       (list
                                         (if (<atree-node>-type subtree)
                                             (<atree-node>-type subtree)
                                             "auto")
                                         (caar symbval)
                                         (if (cadr (cadr symbval))
                                             (cadr (cadr symbval))
                                             (car (cadr symbval))))
                                       children-bindings)))
                                  ('defclass
                                   '())
                                  (else (if (or (not (<atree-node>-children subtree))
                                                (empty? (<atree-node>-children subtree)))
                                            '()
                                            (foldr (lambda (x accum) (append (inner-fn x (cons subtree visited)) accum))
                                                   '()
                                                   (<atree-node>-children subtree)))))))))
    (inner-fn tree '())))

(define (class-node->header k)
  (letrec ((inner-fn (lambda (subtree)
                       (match (<atree-node>-tag subtree)
                              ('defclass (list (string-append
                                                 (class-string subtree)
                                                 " {\n"
                                                 (car (inner-fn (cadr (<atree-node>-children subtree))))
                                                 "};\n")
                                               #f))
                              ('class-content
                               (list
                                 (string-join
                                   (map (lambda (x) (car (inner-fn x)))
                                        (<atree-node>-children subtree))
                                   "\n")
                                 #f))
                              ((or 'private-class-content 'public-class-content)
                               (list 
                                 (string-append (if (eq? (<atree-node>-tag subtree) 'public-class-content)
                                                    "public:\n"
                                                    "private:\n")
                                                (string-join
                                                  (map (lambda (x)
                                                       (let* ((processed-defs
                                                                (map
                                                                  (lambda (def)
                                                                    (let ((tag 
                                                                            (if (eq? (<atree-node>-tag def) 'func-binding)
                                                                                (match (<atree-node>-content
                                                                                         (car (<atree-node>-children def)))
                                                                                       ("make" 'ctor)
                                                                                       ("finalize" 'dtor)
                                                                                       (else #f))
                                                                                #f)))
                                                                      (match tag
                                                                             ('ctor
                                                                              (list (ctor-def def (<atree-node>-content k)) tag))
                                                                             ('dtor
                                                                              (list (string-append "~~" (<atree-node>-content k) "::" (<atree-node>-content k) "();") tag))
                                                                             (else
                                                                               (list (car (node->header def)) #f)))))
                                                                  (<atree-node>-children x)))
                                                              (def-texts (map car processed-defs)))
                                                         (string-append
                                                           (string-join
                                                             (filter (lambda (x) (> (string-length x) 0)) def-texts)
                                                             ";\n")
                                                           (if (eq? (<atree-node>-tag subtree) 'public-class-content)
                                                               (string-append
                                                                 (if (not (foldr (lambda (x rest)
                                                                                   (or rest (eq? (cadr x) 'ctor)))
                                                                                 #f
                                                                                 processed-defs))
                                                                     (string-append (<atree-node>-content k) "();\n")
                                                                     "")
                                                                 (if (not (foldr (lambda (x rest)
                                                                                   (or rest (eq? (cadr x) 'dtor)))
                                                                                 #f
                                                                                 processed-defs))
                                                                     (string-append "~~" (<atree-node>-content k) "();\n")
                                                                     ""))
                                                               ""))))
                                                     (<atree-node>-children subtree))
                                                  "\n")
                                                (let ((bindings (apply append
                                                                       (if (and (<atree-node>-children subtree)
                                                                                (not (empty? (<atree-node>-children subtree))))
                                                                           (map gather-bindings
                                                                                (<atree-node>-children subtree))
                                                                           '()))))
                                                      (if (not (empty? bindings))
                                                          (string-append
                                                            (string-join
                                                              (map
                                                                (lambda (x) (string-append (car x) " " (cadr x) ";\n"))
                                                                bindings)
                                                              "")
                                                            "\n")
                                                          "")))
                                 #f))))))
    (inner-fn k)))

(define (class-node->source k)
  (letrec ((inner-fn (lambda (subtree)
                       (match (<atree-node>-tag subtree)
                              ('defclass
                               (list
                                 (car (inner-fn (cadr (<atree-node>-children subtree))))
                                 #f))
                              ('class-content
                               (list
                                 (string-join
                                   (map (lambda (x) (car (inner-fn x)))
                                        (<atree-node>-children subtree))
                                   "\n")
                                 #f))
                              ((or 'public-class-content 'private-class-content)
                               (list
                                 (string-join
                                   (map (lambda (x)
                                          (let* ((processed-defs
                                                   (map (lambda (def)
                                                          (let ((tag 
                                                                  (if (eq? (<atree-node>-tag def) 'func-binding)
                                                                      (match (<atree-node>-content
                                                                               (car (<atree-node>-children def)))
                                                                             ("make" 'ctor)
                                                                             ("finalize" 'dtor)
                                                                             (else #f))
                                                                      #f)))
                                                            (match tag
                                                                   ('ctor
                                                                    (list (ctor-text def (<atree-node>-content k) x) tag))
                                                                   ('dtor
                                                                    (list (dtor-text def (<atree-node>-content k) x) tag))
                                                                   (else
                                                                     (list (car (node->source def)) #f)))))
                                                        (<atree-node>-children x)))
                                                 (def-texts (string-join (map car processed-defs) "\n")))
                                            (string-append
                                              def-texts
                                              (if (eq? (<atree-node>-tag subtree) 'public-class-content)
                                                  (string-append
                                                    (if (not (foldr
                                                               (lambda (v accum) (or accum (eq? (cadr v) 'ctor)))
                                                               #f
                                                               processed-defs))
                                                        (string-append
                                                          (<atree-node>-content k) "::" (<atree-node>-content k) "() {\n"
                                                          (let ((bindings (foldr append
                                                                                 '()
                                                                               (if (and (<atree-node>-children subtree)
                                                                                        (not (empty? (<atree-node>-children subtree))))
                                                                                   (map gather-bindings
                                                                                        (<atree-node>-children subtree))
                                                                               '()))))
                                                            (string-join
                                                              (map (lambda (x)
                                                                     (string-append
                                                                       (cadr x)
                                                                       " = "
                                                                       (caddr x)
                                                                       ";\n"))
                                                                   bindings)
                                                              ""))
                                                          "}\n\n")
                                                        "")
                                                    (if (not (foldr
                                                               (lambda (v accum) (or accum (eq? (cadr v) 'dtor)))
                                                               #f
                                                               processed-defs))
                                                        (string-append
                                                          "~~" (<atree-node>-content k) "::" (<atree-node>-content k) "() {\n"
                                                          (let ((bindings (foldr append
                                                                                 '()
                                                                               (if (and (<atree-node>-children subtree)
                                                                                        (not (empty? (<atree-node>-children subtree))))
                                                                                   (map gather-bindings
                                                                                        (<atree-node>-children subtree))
                                                                                   '()))))
                                                            (string-join
                                                              (map (lambda (x)
                                                                     (string-append
                                                                       "//"
                                                                       "delete "
                                                                       (cadr x)
                                                                       ""
                                                                       ";\n"))
                                                                   bindings)
                                                              ""))
                                                          "}\n\n")
                                                        ""))
                                                  ""))))
                                        (<atree-node>-children subtree))
                                   "\n")
                                 #f))))))
    (inner-fn k)))

(define (fntype->ctype typ)
  (string-append
    (last typ)
    "(*)("
    (string-join (cadr typ) ", ")
    ")"))

(define (type->printf-type typ)
  (match typ
         ((or "int" "unsigned" "unsigned int" "long" "long int" "long long" "long long int")
          "%d")
         ((or "float" "double")
          "%f")
         ("char*"
          "%s")
         ((or "char" "unsigned char")
          "%c")
         ((? list?) "%p") ; Function pointer
         ((? (lambda (x) (or
                           (string-suffix? x "*")
                           (string-suffix? x "]"))))
          "%p")
         (else "<opaque>")))

(define (die message)
  (begin
    (displayln message)
    (exit -2)))

(define (atree->c++ tree mod (toplevel? #f))
  (values (atree->header tree mod toplevel?)
          (atree->source tree mod toplevel?)))

(define (atree->header tree mod toplevel?)
  (let ((+declare-list+ (cadr tree))
        (+class-table+ (caddr tree))
        (+function-table+ (cadddr tree))
        (+import-list+ (car (cddddr tree)))
        (r (node->header (car tree)))
        (bindings (gather-bindings (car tree))))
      (string-append
        "#ifndef SOL_" (string-upcase mod) "_H\n"
        "#define SOL_" (string-upcase mod) "_H\n\n"
        (if (not (empty? +import-list+))
              (string-join
                (map
                  (lambda (x)
                    (string-append "#include \"" x ".h\"\n"))
                  (reverse +import-list+))
                "")
            "")
        (if (not (empty? +declare-list+))
            (string-append (string-join (reverse +declare-list+) "\n") "\n")
            "")
        (if (not (empty? +class-table+))
            (string-append
              (string-join
                (hash-map +class-table+
                          (lambda (k v)
                            (car (class-node->header (car v)))))
                "\n")
              "\n")
            "")
        (if (not (empty? +function-table+))
            (apply string-append
                   (hash-map +function-table+
                             (lambda (k v)
                               (if (and (<atree-node>-content v)
                                        (not (eq? (<atree-node>-tag v) 'anon-meth)))
                                   (string-append (fn-node->header v) "\n")
                                   ""))))
            "")
        (if (not (empty? bindings))
            (string-join
              (map (lambda (x)
                     (string-append "extern " (car x) " " (cadr x) ";\n"))
                   bindings)
              "")
            "")
        (car r)
        "\n"
        (if (not toplevel?)
            (string-append "int " mod "_entry(int argc, char** argv);")
            "")
        "\n#endif")))

(define (node->header tree)
  (letrec ((inner-fn (lambda (subtree)
                       (begin
                         (match (<atree-node>-tag subtree)
                                ((or 'nothing 'defclass) (list "" #f))
                                ('native (list "" #f))
                                ('lit (list (<atree-node>-content subtree) #f))

                                ('symb (list (<atree-node>-content subtree) #f))

                                ('seq (let* ((raw-clauses (map inner-fn (<atree-node>-children subtree)))
                                             (clauses (map car raw-clauses))
                                             (last-type (<atree-node>-type (last (<atree-node>-children subtree)))))
                                        (list
                                          (string-append
                                            (string-join
                                              (map
                                                (lambda (x)
                                                  (string-append x ";"))
                                                (filter (lambda (x)
                                                          (> (string-length x) 0))
                                                        (drop-right clauses 1)))
                                              "\n")
                                            (if (and last-type
                                                     (not (equal? last-type "void")))
                                                (match (<atree-node>-tag (last (<atree-node>-children subtree)))
                                                       ((or 'call 'lit 'symb)
                                                        (string-append
                                                          "extern "
                                                          last-type
                                                          " "
                                                          (<atree-node>-name subtree)
                                                          ";\n"))
                                                       ('func-binding (if (eq? (<atree-node>-tag (cadr (<atree-node>-children (last (<atree-node>-children subtree))))) 'anon-meth)
                                                                          (string-append
                                                                            (fn-node->header (cadr (<atree-node>-children (last (<atree-node>-children subtree)))))
                                                                            "\n")
                                                                          ""))
                                                       ('binding "")
                                                       (else
                                                         (if (not (cadr (last raw-clauses)))
                                                             (string-append
                                                               (if last-type
                                                                   (if (string? last-type)
                                                                       last-type
                                                                       (fntype->ctype last-type))
                                                                   "auto")
                                                               " "
                                                               (<atree-node>-name subtree)
                                                               " = "
                                                               (last clauses)
                                                               ";\n")
                                                             (string-append
                                                               (last clauses)
                                                               ";\n"))))
                                                ""))
                                          (match (<atree-node>-tag (last (<atree-node>-children subtree)))
                                                 ((or 'func-binding 'binding) #f)
                                                 (else (<atree-node>-name subtree))))))

                                ('binding (list "" (<atree-node>-content subtree)))
                                ('func-binding
                                 (list
                                   ""
                                   (<atree-node>-content subtree)))
                                ('arglist
                                 (list
                                   (string-join
                                     (map
                                       (lambda (x y) (string-append (if x x "auto") " " y))
                                       (map <atree-node>-type (<atree-node>-children subtree))
                                       (map (lambda (x) (car (inner-fn x))) (<atree-node>-children subtree)))
                                     ", ")
                                   #f))
                                ('applylist
                                 (list
                                   (string-join (map (lambda (x) (car (inner-fn x))) (<atree-node>-children)) ", ")
                                   #f))

                                ('call (list
                                         (string-append
                                           (<atree-node>-content (car (<atree-node>-children subtree)))
                                           "("
                                           (car (inner-fn (cadr (<atree-node>-children subtree))))
                                           ")")
                                         #f))

                                ('anon-func (list (string-join (map (lambda (x) (car (inner-fn x))) (<atree-node>-children subtree)) "\n") #f))
                                ('anon-meth (list (string-join (map (lambda (x) (car (inner-fn x))) (drop-right (<atree-node>-children subtree) 1)) "\n") #f))

                                ('err (die (<atree-node>-content subtree)))
                                (else (die (format "Unknown atree node: '~a'" (<atree-node>-tag subtree)))))))))
    (inner-fn tree)))

(define (node->source tree)
  (letrec ((inner-fn (lambda (subtree)
                       (begin
                         (match (<atree-node>-tag subtree)
                                ((or 'nothing 'defclass) (list "" #f))
                                ('native (list (<atree-node>-content subtree) #f))
                                ('lit (list (<atree-node>-content subtree) #f))

                                ('symb (list (<atree-node>-content subtree) #f))

                                ('seq (let* ((raw-clauses (map inner-fn (<atree-node>-children subtree)))
                                             (clauses (map car raw-clauses))
                                             (last-type (<atree-node>-type (last (<atree-node>-children subtree)))))
                                        (list
                                          (string-append
                                            (string-join
                                              (map (lambda (x) (string-append x ";"))
                                              (filter (lambda (x) (> (string-length x) 0)) (drop-right clauses 1)))
                                              "\n")
                                            (if (and last-type
                                                     (not (equal? last-type "void")))
                                                (match (<atree-node>-tag (last (<atree-node>-children subtree)))
                                                       ((or 'call 'lit 'symb)
                                                        (string-append
                                                          last-type
                                                          " "
                                                          (<atree-node>-name subtree)
                                                          " = "
                                                          (last clauses) 
                                                          ";\n"))
                                                       ('func-binding "")
                                                       ('binding 
                                                        (let ((bind-children (<atree-node>-children (last (<atree-node>-children subtree)))))
                                                         (string-append
                                                           (<atree-node>-type (cadr bind-children))
                                                           " "
                                                           (<atree-node>-content (car bind-children))
                                                           " = "
                                                           (<atree-node>-content (cadr bind-children))
                                                           ";\n")))
                                                       (else
                                                         (if (not (cadr (last raw-clauses)))
                                                             (string-append
                                                               (if last-type
                                                                   (if (string? last-type)
                                                                       last-type
                                                                       (fntype->ctype last-type))
                                                                   "auto")
                                                               " "
                                                               (<atree-node>-name subtree)
                                                               " = "
                                                               (last clauses)
                                                               ";\n")
                                                             (string-append
                                                               (last clauses)
                                                               ";\n"))))
                                                ""))
                                          (match (<atree-node>-tag (last (<atree-node>-children subtree)))
                                                 ((or 'func-binding 'binding) #f)
                                                 (else (<atree-node>-name subtree))))))

                                ('binding (list "" (<atree-node>-content subtree)))
                                ('func-binding
                                 (list
                                   ""
                                   (<atree-node>-content subtree)))
                                ('arglist
                                 (list
                                   (string-join
                                     (map
                                       (lambda (x y) (string-append (if x x "auto") " " y))
                                       (map <atree-node>-type (<atree-node>-children subtree))
                                       (map (lambda (x) (car (inner-fn x))) (<atree-node>-children subtree)))
                                     ", ")
                                   #f))
                                ('applylist
                                 (list
                                   (string-join (map (lambda (x) (car (inner-fn x))) (<atree-node>-children)) ", ")
                                   #f))
                                ('call (list
                                         (string-append
                                           (<atree-node>-content (car (<atree-node>-children subtree)))
                                           "("
                                           (car (inner-fn (cadr (<atree-node>-children subtree))))
                                           ")")
                                         #f))

                                ('anon-func (list (string-join (map (lambda (x) (car (inner-fn x))) (<atree-node>-children subtree)) "\n") #f))
                                ('anon-meth (list (string-join (map (lambda (x) (car (inner-fn x))) (drop-right (<atree-node>-children subtree) 1)) "\n") #f))

                                ('err (die (<atree-node>-content subtree)))
                                (else (die (format "Unknown atree node: '~a'" (<atree-node>-tag subtree)))))))))
    (inner-fn tree)))

(define (atree->source tree mod toplevel?)
  (let ((+function-table+ (cadddr tree))
        (+import-list+ (car (cddddr tree)))
        (+class-table+ (caddr tree))
        (r (node->source (car tree)))
        (bindings (gather-bindings (car tree))))
    (string-append
      "#include \"" mod ".h\"\n"
      "#include <stdio.h>\n\n"
      (if (not (empty? +function-table+))
          (apply string-append
                 (hash-map +function-table+
                           (lambda (k v)
                             (if (<atree-node>-content v)
                                 (string-append (fn-node->source v) "\n")
                                 ""))))
          "")
      (if (not (empty? +class-table+))
          (string-join
            (hash-map +class-table+
                      (lambda (k v)
                        (car (class-node->source (car v)))))
               "\n")
          "")
      (if (not (empty? bindings))
          (string-join
            (map (lambda (x)
                   (string-append (car x) " " (cadr x) ";\n"))
                 bindings)
            "")
          "")
      "int "
      (if toplevel?
          "main"
          (string-append mod "_entry"))
      "(int argc, char** argv) {\n"
      (if (and toplevel?
               (not (empty? +import-list+)))
          (string-join
            (map (lambda (x) (string-append (string-replace x "/" "___") "_entry(argc, argv);\n"))
                 (reverse +import-list+))
            "")
          "")
      (if (not (empty? bindings))
          (string-join
            (map (lambda (x)
                   (string-append (cadr x) " = " (caddr x) ";\n"))
                 bindings)
            "")
          "")
      (match (<atree-node>-tag (car tree))
             ('native (car r))
             (else
               (string-append
                 (car r)
                 (if (and
                       (not (equal? (<atree-node>-type (car tree)) "void"))
                       (not (equal? (car r) "")))
                     (string-append
                       ""
                       (if (cadr r)
                           (string-append
                             "printf(\"" (type->printf-type (<atree-node>-type (car tree))) "\\n\", "
                             (if (list? (<atree-node>-type (car tree)))
                                 "&"
                                 "")
                             (cadr r)
                             ");\n")
                           ""))
                     ""))))
      "return 0;\n"
      "}\n")))
