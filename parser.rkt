#lang racket

(provide parse
         push-parser
         pop-parser
         
         lit
         char-range
         
         drop
         star
         plus

         opt
         alt
         
         whitespace
         eos
         
         seq
         
         or-error
         with
         
         rev

         reset-parser!
         )

(require racket/format)
(require srfi/13)

(define *rand-peek* 25)
(define *drop-symbol* (gensym))

(define *parser-table* (make-hash))
(define *error-stack* '())
(define (reset-error error)
  (set! *error-stack* error))

(define *state-stack* '())

(define (reset-parser!)
  (set! *error-stack* '())
  (set! *state-stack* '()))

(define (state-line state)
  (car state))
(define (state-char state)
  (cadr state))
(define (state-total-chars state)
  (caddr state))
(define (reset-state state)
  (set! *state-stack* state))

(define *frag-stack* '())
(define (crush-frag)
  (let ((curr-frag (car *frag-stack*)))
   (pop-frag)
   (pop-frag)
   (push-frag curr-frag)))
(define (reset-frag frag)
  (set! *frag-stack* frag))

(define (push-error l c expect got)
  (set! *error-stack* (cons (format "[~a:~a]: Expected '~a', got '~a'" l c expect got) *error-stack*)))

(define (pop-error)
  (if (not (empty? *error-stack*))
      (set! *error-stack* (cdr *error-stack*))
      #f))

(define (push-state state)
  (set! *state-stack* (cons state *state-stack*)))

(define (pop-state)
  (if (not (empty? *state-stack*))
      (set! *state-stack* (cdr *state-stack*))
      #f))

(define (push-frag frag)
  (set! *frag-stack* (cons frag *frag-stack*)))

(define (pop-frag)
  (if (not (empty? *frag-stack*))
      (set! *frag-stack* (cdr *frag-stack*))
      #f))

(define (die)
  (letrec ((loop (lambda (curr-stack)
                   (if (not (empty? curr-stack))
                       (begin
                         (displayln (car curr-stack))
                         (loop (cdr curr-stack)))
                       #f))))
    (loop *error-stack*)))

(define (find-parser symb)
  (if (symbol? symb)
      (car (hash-ref *parser-table* symb `(,symb)))
      symb))

(define (push-parser name expr)
  (let ((prev-parser (hash-ref *parser-table* name '())))
   (hash-set! *parser-table* name (cons expr prev-parser))))

(define (pop-parser name)
  (let ((prev-parser (hash-ref *parser-table* name '())))
   (if (not (empty? prev-parser))
       (hash-set! *parser-table* name (cdr prev-parser))
       #f)))

(define lit
  (lambda (x)
    (lambda (ipt)
      (let ((is-eof (eof-object? (peek-char ipt (state-total-chars (car *state-stack*))))))
       (begin
         (if is-eof
             (begin
               (push-state (list (state-line (car *state-stack*))
                                 (+ (state-char (car *state-stack*)) 1)
                                 (+ (state-total-chars (car *state-stack*)) 1)))
               (push-error (state-line (car *state-stack*)) (state-char (car *state-stack*)) x "EOF")
               (push-frag (format "~a" (peek-char ipt (state-total-chars (car *state-stack*)))))
               #f)
             (let ((next-match (peek-string (string-length x) (state-total-chars (car *state-stack*)) ipt)))
              (if (string=? next-match x)
                  (let ((n-cnt (regexp-match-positions* "\n" x)))
                   (begin
                     (push-state (list (+ (state-line (car *state-stack*)) (length n-cnt))
                                       (if (empty? n-cnt)
                                           (+ (state-char (car *state-stack*)) (string-length x))
                                           (- (string-length x) (cdar (reverse n-cnt))))
                                       (+ (state-total-chars (car *state-stack*)) (string-length x))))
                     (push-frag (format "~a" x))
                     (list 'lit x)))
                  (begin
                    (push-state (list (state-line (car *state-stack*))
                                      (+ (state-char (car *state-stack*)) (string-length x))
                                      (+ (state-total-chars (car *state-stack*)) (string-length x))))
                    (push-error (state-line (cadr *state-stack*)) (state-char (cadr *state-stack*)) x next-match)
                    (push-frag (format "~a" next-match))
                    #f)))))))))

(define with
  (lambda (with-name with-parser expr-parser)
    (lambda (ipt)
      (let ((subexpr ((find-parser with-parser) ipt)))
       (if subexpr
           (begin
             (push-parser with-name (lit (car *frag-stack*)))
             (let ((with-result ((find-parser expr-parser) ipt)))
              (begin
                (pop-parser with-name)
                with-result)))
           #f)))))

(define rev
  (lambda (parser)
    (lambda (ipt)
      (let* ((submatch ((find-parser parser) ipt))
             (my-state (cdr *state-stack*)))
        (if submatch
            (begin
              (push-error
                (state-line my-state)
                (state-char my-state)
                (format "inverse of '~a'" parser)
                (car *frag-stack*))
              #f)
            (begin
              (pop-error)
              (car *frag-stack*)))))))

(define char-range
  (lambda (range)
    (lambda (ipt)
      (let ((is-eof (eof-object? (peek-char ipt (state-total-chars (car *state-stack*))))))
       (if is-eof
           (begin
             (push-error
               (state-line (car *state-stack*))
               (+ (state-char (car *state-stack*)) 1)
               (format "character in range '~a'" range)
               "EOF")
             (push-frag (format "~a" (peek-char ipt (state-total-chars (car *state-stack*)))))
             (push-state (list (state-line (car *state-stack*))
                               (+ (state-char (car *state-stack*)) 1)
                               (+ (state-total-chars (car *state-stack*)) 1)))
             #f)
           (let* ((next-char (peek-string 1 (state-total-chars (car *state-stack*)) ipt))
                  (maybe-match (regexp-match (format "[~a]" range) next-char)))
             (begin
               (push-frag (format "~a" next-char))
               (push-state (list (state-line (car *state-stack*))
                                 (+ (state-char (car *state-stack*)) 1)
                                 (+ (state-total-chars (car *state-stack*)) 1)))
               (if maybe-match
                   (list 'lit (car maybe-match))
                   (begin
                     (push-error
                       (state-line (cadr *state-stack*))
                       (state-char (cadr *state-stack*))
                       (format "character in range '~a'" range)
                       next-char)
                     #f)))))))))

(define eos
  (lambda ()
    (lambda (ipt)
      (let ((next (peek-char ipt (state-total-chars (car *state-stack*)))))
       (begin
         (push-frag (format "~a" next))
         (push-state (list (state-line (car *state-stack*))
                           (+ (state-char (car *state-stack*)) 1)
                           (+ (state-total-chars (car *state-stack*)) 1)))
         (if (eof-object? next)
             next
             (begin
               (push-error
                 (state-line (cadr *state-stack*))
                 (state-char (cadr *state-stack*))
                 "EOF"
                 next)
               #f)))))))

(define whitespace
  (lambda ()
    (lambda (ipt)
      (let ((next-char (peek-char ipt (state-total-chars (car *state-stack*)))))
       (begin
         (push-frag (format "~a" next-char))
         (push-state (list
                       (if (eq? #\newline next-char)
                           (+ (state-line (car *state-stack*)) 1)
                           (state-line (car *state-stack*)))
                       (if (eq? #\newline next-char)
                           0
                           (+ (state-char (car *state-stack*)) 1))
                       (+ (state-total-chars (car *state-stack*)) 1)))
         (if (or (eq? #\space next-char) (eq? #\newline next-char))
             (list 'lit (format "~a" next-char))
             (begin
               (push-error
                 (state-line (cadr *state-stack*))
                 (state-char (cadr *state-stack*))
                 "whitespace"
                 next-char)
               #f)))))))

(define drop
  (lambda (parser)
    (lambda (ipt)
      (let ((curr-state *state-stack*)
            (curr-error *error-stack*)
            (curr-frag *frag-stack*)
            (parse-result ((find-parser parser) ipt)))
       (if parse-result
           *drop-symbol*
           (begin
             (reset-state *state-stack*)
             (push-error (state-line (cadr *state-stack*))
                         (state-char (cadr *state-stack*))
                         (format "dropped match for '~a'" parser)
                         (car *frag-stack*))
             #f))))))

(define alt
  (lambda (parser . parsers)
    (lambda (ipt)
      (letrec ((curr-state *state-stack*)
               (curr-error *error-stack*)
               (curr-frag *frag-stack*)
               (loop (lambda (ps first?)
                       (if (empty? ps)
                           (begin
                             (push-error
                               (state-line (car curr-state))
                               (state-char (car curr-state))
                               (format "any of '~a'" (cons parser parsers))
                               (car *frag-stack*))
                             #f)
                           (begin
                             (if (not first?)
                                 (begin
                                   (reset-error curr-error)
                                   (reset-state curr-state)
                                   (reset-frag curr-frag))
                                 #f)
                             (let ((parse-result ((find-parser (car ps)) ipt)))
                              (if parse-result
                                  parse-result
                                  (loop (cdr ps) #f))))))))
        (loop (cons parser parsers) #t)))))

(define star
  (lambda (parser)
    (lambda (ipt)
      (letrec ((loop (lambda (accum frag-accum first?)
                       (let ((curr-state *state-stack*)
                             (curr-frag *frag-stack*)
                             (curr-error *error-stack*)
                             (parse-result ((find-parser parser) ipt)))
                         (if parse-result
                             (begin
                               (loop (if (not (eq? parse-result *drop-symbol*))
                                         (if (and (list? parse-result)
                                                  (eq? (car parse-result) 'lst))
                                             (cons parse-result accum)
                                             (cons parse-result accum))
                                         accum)
                                     (string-append frag-accum (car *frag-stack*))
                                     #f))
                             (begin
                               (reset-error curr-error)
                               (reset-state curr-state)
                               (reset-frag curr-frag)
                               (if (empty? accum)
                                   *drop-symbol*
                                   `(lst ,@(reverse accum)))))))))
        (loop '() "" #t)))))

(define plus
  (lambda (parser)
    (lambda (ipt)
      (letrec ((loop (lambda (accum frag-accum first?)
                       (let ((curr-state *state-stack*)
                             (curr-frag *frag-stack*)
                             (curr-error *error-stack*)
                             (parse-result ((find-parser parser) ipt)))
                         (if parse-result
                             (begin
                               (loop (if (not (eq? parse-result *drop-symbol*))
                                         (if (list? parse-result)
                                             (cons parse-result accum)
                                             (cons parse-result accum))
                                         accum)
                                     (string-append frag-accum (car *frag-stack*))
                                     #f))
                             (if (> (length accum) 0)
                                 (begin
                                   (reset-error curr-error)
                                   (reset-state curr-state)
                                   (reset-frag curr-frag)
                                   `(lst ,@(reverse accum)))
                                 (begin
                                   (push-error
                                     (state-line (cadr *state-stack*))
                                     (state-char (cadr *state-stack*))
                                     (format "at least one '~a'" parser)
                                     (car *frag-stack*))
                                   #f)))))))
        (loop '() "" #t)))))

(define or-error
  (lambda (parser)
    (lambda (ipt)
      (let ((parse-result ((find-parser parser) ipt)))
       (if parse-result
           parse-result
           (die))))))

(define seq
  (lambda (tag . parsers)
    (lambda (ipt)
      (letrec ((loop (lambda (accum ps)
                       (if (empty? ps)
                           `(,tag ,@(reverse accum))
                           (let ((parse-result ((find-parser (car ps)) ipt)))
                            (if parse-result
                                (if (not (eq? parse-result *drop-symbol*))
                                    (loop (if (and (list? parse-result)
                                                   (eq? (car parse-result) 'lst))
                                              (cons parse-result accum)
                                              (cons parse-result accum))
                                          (cdr ps))
                                    (loop accum (cdr ps)))
                                (begin
                                  (push-error
                                    (state-line (cadr *state-stack*))
                                    (state-char (cadr *state-stack*))
                                    (format "'~a' sequence" tag)
                                    (car *frag-stack*))
                                  #f)))))))
        (loop '() parsers)))))

(define opt
  (lambda (parser)
    (lambda (ipt)
      (let ((curr-state *state-stack*)
            (curr-error *error-stack*)
            (curr-frag *frag-stack*)
            (parse-result ((find-parser parser) ipt)))
       (if parse-result
           parse-result
           (begin
             (reset-error curr-error)
             (reset-state curr-state)
             (reset-frag curr-frag)
             *drop-symbol*))))))

(define (parse fn ipt)
  (if (empty? *state-stack*)
      (begin
        (push-state (list 0 0 0))
        (push-frag ""))
      #f)
  (let ((parse-result ((find-parser fn) ipt)))
   (if (not parse-result)
       (die)
       parse-result)))
