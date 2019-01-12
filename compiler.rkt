#lang racket

(provide compile
        <atree-node>
         <atree-node>-tag
         <atree-node>-content
         <atree-node>-type
         <atree-node>-name
         <atree-node>-children
         
         reset-compiler!)
(require racket/match)
(require racket/hash)
(require compatibility/mlist)

(define *ctx-parent-symbol* "$%PARENT")
(define *symbol-prefix* "_")

(define (make-ctx prev-ctx)
  (let ((ret (make-hash)))
   (hash-set! ret *ctx-parent-symbol* prev-ctx)
   ret))

(define (ctx-set-parent! ctx parent-ctx)
  (hash-set! ctx *ctx-parent-symbol* parent-ctx)
  ctx)

(define (update-ctx! ctx key val)
  (begin
    (hash-set! ctx key val)
    ctx))

(define (ctx-merge! ctx1 ctx2)
  (hash-union! ctx1 ctx2
               #:combine/key
               (lambda (k a b) b)))

(define (ctx-lookup ctx key)
  (let ((maybe-val (hash-ref ctx key #f))
        (maybe-parent (hash-ref ctx *ctx-parent-symbol* #f)))
    (if maybe-val
        maybe-val
        (if maybe-parent
            (ctx-lookup maybe-parent key)
            #f))))

(define +name-counter+ 0)
(define +function-table+ (make-hash))
(define +symbol-table+ (make-hash))
(define +class-table+ (make-hash))
(define +declare-list+ '())
(define +local-function-table+ (make-hash))
(define +local-symbol-table+ (make-hash))
(define +local-class-table+ (make-hash))
(define +local-declare-list+ '())
(define +mod-prefix+ "toplevel")
(define +import-list+ '())
(define +local-import-list+ '())
(define +top-ctx+ (make-ctx #f))

(define (reset-compiler!)
  (set! +mod-prefix+ "toplevel")
  (set! +local-import-list+ '())
  (set! +local-function-table+ (make-hash))
  (set! +local-symbol-table+ (make-hash))
  (set! +local-class-table+ (make-hash))
  (set! +local-declare-list+ '()))

(struct <atree-node> ((tag #:mutable)
                      (type #:mutable)
                      (name #:mutable)
                      (content #:mutable)
                      (children #:mutable))
        #:transparent)

(define (die message)
  (begin
    (displayln message)
    (exit -1)))

(define (unescaped-str str)
  (letrec ((loop (lambda (rlist curr-str)
                   (if (empty? rlist)
                       curr-str
                       (loop (cdr rlist)
                             (string-replace curr-str (caar rlist) (cdar rlist)))))))
    (loop '(("\\n" . "\n")
            ("\\t" . "\t")
            ("\\\"" . "\"")
            ("\\\\" . "\\")
            ("\\\n" . "\\n")
            ("\\\t" . "\\t")
            ("\\\"" . "\""))
          str)))

(define (native-sub str ctx)
  (let ((all-cand (regexp-match* "(?<!\\\\)\\$\\{[^:space:]+\\}" str)))
   (letrec ((loop (lambda (cand curr-str)
                    (if (empty? cand)
                        (unescaped-str curr-str)
                        (let ((maybe-match
                                (ctx-lookup ctx (substring (car cand) 2 (- (string-length (car cand)) 1)))))
                          (if maybe-match
                              (loop (cdr cand) (string-replace curr-str (car cand) (<atree-node>-content maybe-match)))
                              (loop (cdr cand) curr-str)))))))
     (loop all-cand str))))

(define (type->formatstr dtype)
  (match dtype
         ('int "%d")
         ('float "%f")
         (else "<error:unknown format>")))

(define (unique-symbol! prefix)
  (begin
    (set! +name-counter+ (+ 1 +name-counter+))
    (string-append prefix (number->string +name-counter+))))

(define (typestr t)
  (match t
         ('int "int")
         ('float "float")
         (else (format "<error:unknown type:~a>" t))))

(define (next-declare dtype)
  (let ((dtype-str (typestr dtype)))
   (string-append dtype-str " " (unique-symbol! dtype-str))))

(define (next-symbol!)
  (begin
    (set! +name-counter+ (+ 1 +name-counter+))
    (number->string +name-counter+)))

(define (make-name! #:name (name #f))
  (string-append +mod-prefix+
                 (if (not name)
                     "_a"
                     (string-append "_" name))
                 (next-symbol!)))

(define (last-ident dtype)
  (let ((dtype-str (typestr dtype)))
   (string-append dtype-str (number->string +name-counter+))))

(define (leaf-token? x)
  (match x
         ((or 'int 'float 'symbol 'symb 'typed-symb 'str) #t)
         (else #f)))

(define (obj-binding? obj)
  (if obj
      (eq? (<atree-node>-tag obj) 'binding)
      #f))

(define (obj-symb? obj)
  (if obj
      (eq? (<atree-node>-tag obj) 'symb)
      #f))

(define (obj-lit? obj)
  (if obj
      (eq? (<atree-node>-tag obj) 'lit)
      #f))

(define (make-obj lst)
  (<atree-node> (car lst)
                (cadr lst)
                (caddr lst)
                (cadddr lst)
                (if (not (empty? (cddddr lst)))
                    (car (cddddr lst))
                    '())))

(define (register-import! x)
  (set! +import-list+ (cons x +import-list+))
  (set! +local-import-list+ (cons x +local-import-list+)))

(define (register-declare! decl)
  (set! +declare-list+ (cons decl +declare-list+))
  (set! +local-declare-list+ (cons decl +local-declare-list+)))

(define (register-class! class-name class-obj class-ctx)
  (hash-set! +class-table+ class-name (cons class-obj class-ctx))
  (hash-set! +local-class-table+ class-name (cons class-obj class-ctx)))

(define (function-lookup key)
  (hash-ref +function-table+ key #f))

(define (delete-anon-function! key)
  (let ((maybe-hit (hash-ref +function-table+ key #f)))
   (if maybe-hit
       (if (or (eq? (<atree-node>-tag maybe-hit) 'anon-func)
               (eq? (<atree-node>-tag maybe-hit) 'anon-meth))
           (begin
             (hash-remove! +function-table+ key)
             (hash-remove! +local-function-table+ key))
           #f)
       #f)))

(define (maybe-register-symbol! s)
  (let ((maybe-symbol (hash-ref +symbol-table+ s #f))
        (next-token (hash-count +symbol-table+)))
   (if maybe-symbol
       maybe-symbol
       (begin
         (hash-set! +symbol-table+ s (number->string next-token))
         (hash-set! +local-symbol-table+ s (number->string next-token))
         (hash-ref +symbol-table+ s)))))

(define (register-function! name obj)
  (begin
    (hash-set! +function-table+ name obj)
    (hash-set! +local-function-table+ name obj)
    obj))

(define (type->ctype typ)
  (match typ
         ('int "int")
         ('float "float")
         ('symbol "int")
         ('str "char*")
         ((? string?) typ)
         ((list '-> (list p ...) r) (string-append (type->ctype r) "(*)("
                                                   (string-join (map p type->ctype) ",")
                                                   ")"))
         (else (begin (displayln (format "Found unknown type ~a" typ)) "auto"))))

(define (ctype->printf-type typ)
  (match typ
         ((or "int" "unsigned" "unsigned int" "long" "long int" "long long" "long long int")
          "%d")
         ((or "float" "double")
          "%f")
         ("char*"
          "%s")
         ((or "char" "unsigned char")
          "%c")
         ((? (lambda (x) (or
                           (string-suffix? x "*")
                           (string-suffix? x "]"))))
          "%p")
         (else "<opaque>")))

(define (type->printf-type typ)
  (match typ
         ('int "%d")
         ('float "%f")
         ('symbol "%d")
         ('str "%s")
         ((? string?) (ctype->printf-type typ))
         (else "%p")))

(define (make-fromto from to)
  (list (append to from)
        (append (map (lambda (x) (string-append *symbol-prefix* x)) to) to)))

(define (symbolize x)
  (letrec ((loop (lambda (str repfrom repto)
                   (if (empty? repfrom)
                       str
                       (loop (string-replace str (car repfrom) (car repto)) (cdr repfrom) (cdr repto))))))
    (make-name! #:name
                (apply loop x (make-fromto
                                '("+" "-" "?" "!" "*" "/" ":")
                                '("plus" "minus" "bool" "impure" "star" "slash" "colon"))))))

(define (compile-leaf leaf ctx)
  (match (car leaf)
         ('int (<atree-node> 'lit "int" #f (format "~a" (cadr leaf)) #f))
         ('float (<atree-node> 'lit "float" #f (format "~a" (cadr leaf)) #f))
         ('symbol (let ((new-symb (maybe-register-symbol! (cadr leaf))))
                   (<atree-node> 'symbol "int" (cadr leaf) new-symb #f)))
         ('symb  (let ((binding (ctx-lookup ctx (cadr leaf))))
                  (<atree-node> 'symb
                                     (if binding
                                         (<atree-node>-type binding)
                                         #f)
                                     (cadr leaf)
                                     (if binding
                                         (<atree-node>-content binding)
                                         (symbolize (cadr leaf)))
                                     (if binding
                                         (<atree-node>-children binding)
                                         #f))))
         ('str (<atree-node> 'lit "char*" #f (cadr leaf) #f))
         ('typed-symb (let* ((symb (compile-leaf (cadr leaf) ctx))
                             (binding (ctx-lookup ctx (<atree-node>-name symb))))
                        (let ((node-type 
                                (if binding
                                    (if (not (equal? (<atree-node>-type binding) (caddr leaf)))
                                        'err
                                        (<atree-node>-type binding))
                                    (caddr leaf))))
                        (<atree-node>
                          'symb
                          node-type
                          (<atree-node>-name symb)
                          (if (eq? 'err node-type)
                              (format "[~a:~a] Type mismatch: '~a', '~a'" "~a" "~a"
                                      #f
                                      #f
                                      (<atree-node>-type binding)
                                      (caddr leaf))
                              (<atree-node>-content symb))
                          (<atree-node>-children symb)))))
         (else (die (format "Unknown leaf token ~a" (car leaf))))))

(define (compile-node node ctx)
  (letrec ((inner-fn (lambda (subnode subctx ret)
                       (match (car subnode)
                              ((? leaf-token?) (ret (compile-leaf subnode subctx) subctx))
                              ('import (begin
                                         (register-import! (cadr subnode))
                                         (ret (<atree-node> 'nothing #f #f #f #f) subctx)))
                              ('defclass (let ((class-ctx (make-ctx subctx))
                                               (self (<atree-node>
                                                       'defclass
                                                       #f
                                                       #f
                                                       #f
                                                       #f)))
                                           (begin
                                             (update-ctx! class-ctx "this" self)
                                             (inner-fn (cadr subnode) class-ctx
                                                       (lambda (rname cname)
                                                         (begin (set-<atree-node>-name! self (<atree-node>-name rname))
                                                                (set-<atree-node>-content! self (<atree-node>-content rname))
                                                                (inner-fn (caddr subnode) cname
                                                                          (lambda (r c)
                                                                            (begin
                                                                              (if (eq? (<atree-node>-tag r) 'class-parents)
                                                                                  (inner-fn (cadddr subnode) c
                                                                                            (lambda (r2 c2)
                                                                                              (begin
                                                                                                (set-<atree-node>-children! self (list r r2))
                                                                                                (register-class! (<atree-node>-name rname)
                                                                                                                 self
                                                                                                                 c2))))
                                                                                  (begin
                                                                                    (set-<atree-node>-children! self
                                                                                                                (list #f r))
                                                                                    (register-class! (<atree-node>-name rname)
                                                                                                     self
                                                                                                     c)))
                                                                              (ret self c))))))))))
                              ('parent-classes (inner-fn (cadr subnode) subctx
                                                         (lambda (r c)
                                                           (if (empty? (cddr subnode))
                                                               (ret (<atree-node>
                                                                      'class-parents
                                                                      #f
                                                                      #f
                                                                      #f
                                                                      (list r))
                                                                    c)
                                                               (inner-fn (cons 'parent-classes (cddr subnode)) c
                                                                         (lambda (r2 c2)
                                                                           (ret (<atree-node> 
                                                                                  'class-parents
                                                                                  #f
                                                                                  #f
                                                                                  #f
                                                                                  `(,r ,@(<atree-node>-children r2)))
                                                                                c2)))))))
                              ('parent-class (ret (<atree-node>
                                                    (match (cadr subnode)
                                                           ("public" 'public-class-parent)
                                                           ("private" 'private-class-parent))
                                                    #f
                                                    #f
                                                    (caddr subnode)
                                                    #f)
                                                    subctx))
                              ('class-content (inner-fn (cadr subnode) subctx
                                                        (lambda (r c)
                                                          (if (empty? (cddr subnode))
                                                              (ret (<atree-node>
                                                                     'class-content
                                                                     #f
                                                                     #f
                                                                     #f 
                                                                     (list r))
                                                                   subctx)
                                                              (inner-fn (cons 'class-content (cddr subnode)) subctx
                                                                        (lambda (r2 c2)
                                                                          (ret (<atree-node>
                                                                                 'class-content
                                                                                 #f
                                                                                 #f
                                                                                 #f
                                                                                 `(,r ,@(<atree-node>-children r2)))
                                                                               subctx)))))))
                              ('public-class-content (inner-fn (cadr subnode) subctx
                                                               (lambda (r c)
                                                                 (ret (<atree-node>
                                                                        'public-class-content
                                                                        #f
                                                                        #f
                                                                        #f
                                                                        (list r))
                                                                      c))))
                              ('private-class-content (inner-fn (cadr subnode) subctx
                                                                (lambda (r c)
                                                                  (ret (<atree-node>
                                                                         'private-class-content
                                                                         #f
                                                                         #f
                                                                         #f
                                                                         (list r))
                                                                       c))))
                              ('seq (inner-fn (cadr subnode)
                                              subctx
                                              (lambda (r c)
                                                (let* ((name (make-name!))
                                                       (obj (<atree-node> 
                                                              'seq
                                                              (<atree-node>-type r)
                                                              name
                                                              (<atree-node>-content r)
                                                              (list r))))
                                                  (begin
                                                    (if (eq? (car (cadr subnode)) 'def)
                                                        (update-ctx! subctx (<atree-node>-name r) r)
                                                        #f)
                                                    (if (empty? (cddr subnode))
                                                        (ret obj subctx)
                                                        (inner-fn (cons 'seq (cddr subnode))
                                                                  subctx
                                                                  (lambda (r2 c2)
                                                                    (ret (<atree-node>
                                                                           'seq
                                                                           (<atree-node>-type r2)
                                                                           name
                                                                           (<atree-node>-content r2)
                                                                           `(,r ,@(<atree-node>-children r2)))
                                                                         subctx)))))))))
                              ('def (inner-fn (cadr subnode) subctx
                                              (lambda (r-symb c-symb)
                                                (inner-fn (caddr subnode) subctx
                                                          (lambda (r-val c-val)
                                                            (match (<atree-node>-tag r-val)
                                                                   ((or 'anon-func 'anon-meth)
                                                                    (begin
                                                                      (register-function!
                                                                        (<atree-node>-name r-symb)
                                                                        (function-lookup (<atree-node>-name r-val)))
                                                                      (delete-anon-function!
                                                                        (<atree-node>-name r-val))
                                                                      (set-<atree-node>-content! (function-lookup (<atree-node>-name r-symb)) (<atree-node>-content r-symb))
                                                                      (update-ctx! subctx
                                                                                   (<atree-node>-name r-symb)
                                                                                   (ctx-lookup c-val (<atree-node>-name r-val)))
                                                                      (ret (<atree-node> 'func-binding
                                                                                         (<atree-node>-type r-val)
                                                                                         (<atree-node>-name r-symb)
                                                                                         (<atree-node>-content r-symb)
                                                                                         (list r-symb r-val))
                                                                           c-val)))
                                                                   (else
                                                                     (begin
                                                                       (update-ctx! subctx
                                                                                    (<atree-node>-name r-symb)
                                                                                    r-val)
                                                                       (ret (<atree-node> 'binding
                                                                                          (<atree-node>-type r-val)
                                                                                          (<atree-node>-name r-symb)
                                                                                          (<atree-node>-content r-symb)
                                                                                          (list r-symb r-val))
                                                                            subctx)))))))))
                              ('arglist (inner-fn (cadr subnode) subctx
                                                  (lambda (r c)
                                                    (begin
                                                      (update-ctx! subctx (<atree-node>-name r) r)
                                                      (if (empty? (cddr subnode))
                                                          (ret (<atree-node>
                                                                 'arglist
                                                                 #f
                                                                 #f
                                                                 #f
                                                                 (list r))
                                                               subctx)
                                                          (inner-fn (cons 'arglist (cddr subnode)) subctx
                                                                    (lambda (r2 c2)
                                                                      (begin
                                                                        (ctx-merge! subctx c2)
                                                                        (ret (<atree-node> 
                                                                               'arglist
                                                                               #f
                                                                               #f
                                                                               #f
                                                                               `(r ,@(<atree-node>-children r2)))
                                                                             subctx)))))))))
                              ('native (ret (<atree-node> 'native
                                                          #f
                                                          #f
                                                          (native-sub (string-append (cadadr subnode) "\n") subctx)
                                                          #f)
                                            subctx))
                              ('native-declare (begin
                                                 (register-declare! (native-sub (cadadr subnode) subctx))
                                                 (ret (<atree-node>
                                                        'nothing
                                                        #f
                                                        #f
                                                        #f
                                                        #f)
                                                      subctx)))
                              ('lambda (let ((new-ctx (make-ctx #f)))
                                        (inner-fn (cadr subnode) new-ctx
                                                  (lambda (rsymb csymb)
                                                    (begin
                                                      (ctx-set-parent! csymb subctx)
                                                      (inner-fn (cons 'seq (cddr subnode)) csymb
                                                                (lambda (rfn cfn)
                                                                  (let* ((name (make-name!))
                                                                         (maybe-class (ctx-lookup subctx "this"))
                                                                         (fn-obj (<atree-node>
                                                                                   (if maybe-class 'anon-meth 'anon-func)
                                                                                   (list '-> (map (lambda (x)
                                                                                                    (if (<atree-node>-type x)
                                                                                                        (<atree-node>-type x)
                                                                                                        "auto"))
                                                                                                  (<atree-node>-children rsymb))
                                                                                         (if (or (eq? (<atree-node>-tag rfn) 'native)
                                                                                                 (eq? (<atree-node>-tag rfn) 'nothing))
                                                                                             "void"
                                                                                             (<atree-node>-type rfn)))
                                                                                   name
                                                                                   name
                                                                                   (list rsymb rfn (if maybe-class maybe-class '())))))
                                                                    (begin
                                                                      (register-function!
                                                                        name
                                                                        fn-obj)
                                                                      (ret fn-obj csymb))))))))))
                              ('applylist (inner-fn (cadr subnode) subctx
                                                    (lambda (r c)
                                                      (if (empty? (cddr subnode))
                                                          (ret (<atree-node>
                                                                 'applylist
                                                                 #f
                                                                 #f
                                                                 #f
                                                                 (list r))
                                                               subctx)
                                                          (inner-fn (cons 'applylist (cddr subnode)) subctx
                                                                    (lambda (r2 c2)
                                                                      (ret (<atree-node>
                                                                             'applylist
                                                                             #f
                                                                             #f
                                                                             #f
                                                                             `(,r ,@(<atree-node>-children r2)))
                                                                           subctx)))))))
                              ('apply (inner-fn (cadr subnode) subctx
                                                (lambda (rsymb csymb)
                                                  (let* ((symb-id (ctx-lookup csymb (<atree-node>-name rsymb))))
                                                    (if (not symb-id)
                                                        (ret (<atree-node> 'err
                                                                           #f
                                                                           #f
                                                                           (format "[~a:~a]: Expected a function in apply position" #f #f)
                                                                           #f)
                                                             subctx)
                                                        (if (eq? 'applynil (caaddr subnode))
                                                            (ret (<atree-node>
                                                                   'call
                                                                   (if (<atree-node>-type symb-id)
                                                                       (last (<atree-node>-type symb-id))
                                                                       "auto")
                                                                   #f
                                                                   #f
                                                                   (list symb-id))
                                                                 csymb)
                                                            (inner-fn (caddr subnode) subctx
                                                                      (lambda (rvals cvals)
                                                                        (let ((new-ctx (make-ctx csymb)))
                                                                         (ret (<atree-node>
                                                                                'call
                                                                                (if (<atree-node>-type symb-id)
                                                                                    (last (<atree-node>-type symb-id))
                                                                                    "auto")
                                                                                #f
                                                                                #f
                                                                                `(,symb-id ,@(<atree-node>-children rvals)))
                                                                              new-ctx))))))))))
                              (else (die (format "Unknown node ~a" subnode)))))))
    (inner-fn node ctx (lambda (x ctx) x))))

(define (compile ast mod)
  (set! +mod-prefix+ (native-sub mod (make-ctx #f)))
  (list (compile-node ast +top-ctx+)
        +local-declare-list+
        +local-class-table+
        +local-function-table+
        +local-import-list+))
