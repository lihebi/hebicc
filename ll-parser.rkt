#lang racket

(require parser-tools/lex)
(require "token.rkt")
(require "lexer.rkt")
(require "ast.rkt")
(require rackunit)



;; back-stack back    |||   active-back port-cache
(define (ll-lexer lexer)
  (let ([port-cache (list (lexer) (lexer))]
        [back-stack '()]
        [back '()]
        [is-backtracking? #f]
        [active-back '()])
    (define (consume)
      (let ([res #f])
        (if (not (null? active-back))
            (begin
              (set! res (car active-back))
              (set! active-back (cdr active-back)))
            (begin
              (set! res (car port-cache))
              (set! port-cache (append port-cache (list (lexer))))
              (set! port-cache (cdr port-cache))))
        res))
    (lambda (k)
      (case k
        [(consume) (let ([res (consume)])
                     (when is-backtracking?
                       (set! back (append back `(,res))))
                     res)]
        [(track) (begin
                   (set! is-backtracking? #t)
                   (when (not (null? back))
                     ;; (println back-stack)
                     ;; (println back)
                     (set! back-stack (append back-stack `(,back)))
                     (set! back '())))]
        [(pop) (when (not (null? back-stack))
                 (set! back (last back-stack))
                 (set! back-stack (drop-right back-stack 1)))]
        [(restore) (begin
                     ;; (println "Before restore")
                     ;; (printf "back-stack: ~a~n" back-stack)
                     ;; (printf "back: ~a~n" back)
                     ;; (printf "active-back: ~a~n" active-back)
                     (set! active-back (append back active-back))
                     (when (not (null? back-stack))
                       (set! back (last back-stack))
                       (set! back-stack (drop-right back-stack 1)))
                     ;; (println "After restore")
                     ;; (printf "back-stack: ~a~n" back-stack)
                     ;; (printf "back: ~a~n" back)
                     ;; (printf "active-back: ~a~n" active-back)
                     )]
        [(0) (if (not (null? active-back))
                 (car active-back)
                 (car port-cache))]
        [(1) (begin
               (if (not (null? active-back))
                   (if (not (null? (cdr active-back)))
                       (cadr active-back)
                       (car port-cache))
                   (cadr port-cache)))]
        [else (raise-syntax-error #f "Invalid operation for ll-lexer")]))))

((lambda (x)
   (case x
     [(''a 8) 'a]
     ['5 '5])) 'a)

(define (string->ll str)
  (ll-lexer (string-lexer str)))

(define (print-all-tokens ll)
  (for ([i (in-naturals)]
        #:break (eq? (token-name (ll 0)) 'eof))
    (println (ll 0))
    (ll 'consume)))

(module+ test
  (let ([ll (string->ll "1 2 3 4 5 6 7 8 9")])
    (check-equal? (token-value (ll 0)) "1")
    (check-equal? (token-value (ll 1)) "2")
    (ll 'consume)
    (check-equal? (token-value (ll 0)) "2")
    (check-equal? (token-value (ll 1)) "3")
    (ll 'consume)
    (ll 'consume)
    (check-equal? (token-value (ll 0)) "4")
    (check-equal? (token-value (ll 1)) "5"))
  (let ([ll (string->ll "1 2 3 4 5 6 7 8 9")])
    (ll 'track)
    (ll 'consume)                       ; 1
    (ll 'consume)                       ; 2
    (ll 'track)
    (ll 'consume)                       ; 3
    (check-equal? (token-value (ll 0)) "4")
    (ll 'track)
    (ll 'consume)                       ; 4
    (ll 'consume)                       ; 5
    (check-equal? (token-value (ll 0)) "6")
    (ll 'pop)                           ; drop 4 5
    (check-equal? (token-value (ll 0)) "6")
    (check-equal? (token-value (ll 1)) "7")
    (ll 'restore)                       ; (3) (6 7 ..
    (check-equal? (token-value (ll 0)) "3")
    (check-equal? (token-value (ll 1)) "6")
    (ll 'restore)
    (check-equal? (token-value (ll 0)) "1") ; (1 2 3) (6 7 ..
    (check-equal? (token-value (ll 1)) "2")))

;; (define (parse-program str)
;;   (let ([lexer (string-lexer str)])
;;     (parse lexer)))



;; TODO general checking EOF
;; TODO general consume token

(define (try-consume ll target)
  (when (eq? (token-name (ll 0)) target)
    (ll 'consume)))

(define (expect-consume ll target)
  (if (eq? (token-name (ll 0)) target)
      (ll 'consume)
      (raise-syntax-error #f "Error")))

(define (skip-until ll target)
  (case (token-name (ll 0))
    [(target) `(,(ll 0))]
    ['l-paren (cons (ll 0) (skip-until ll 'r-paren))]
    ['l-bracket (cons (ll 0) (skip-until ll 'r-bracket))]
    ['l-brace (cons (ll 0) (skip-until ll 'r-brace))]
    ;; FIXME unbalanced parens
    ;; semicolon force return
    ;; ['semi-colon #f]
    [else (let ([cur (ll 'consume)])
            (cons (ll 0) (skip-until ll target)))]))



(define (try-parse func ll)
  (ll 'track)
  (let ([res (#%app func ll)])
    (if res
        (ll 'pop)
        (ll 'restore))
    res))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO NOW
(define (parse-statement ll)
  (parse-labeled-statement ll)
  (parse-compound-statement ll))

(define (parse-statement-or-declaration ll)
  (println "parse-statement-or-declaration")
  (match (token-name (ll 0))
    ['IDENTIFIER (when (eq? (token-name (ll 1)) ':)
                   (parse-labeled-statement ll))]
    ;; if is part of declaration specifier
    [(or 'typedef
         'extern
         'static
         'auto
         'register
         'short
         'long
         'signed
         'unsigned
         'void
         'char
         'int
         'float
         'double
         'bool
         'struct
         'union
         'enum
         'const
         'volatile
         'restrict
         'inline)
     (parse-declaration ll)]
    ['case (parse-case-statement ll)]
    ['default (parse-default-statement ll)]
    ['l-brace (parse-compound-statement ll)]
    ['semi-colon (parse-null-statement ll)]
    ['if (parse-if-statement ll)]
    ['switch (parse-switch-statement ll)]
    ['while (parse-while-statement ll)]
    ['do (parse-do-statement ll)]
    ['for (parse-for-statement ll)]
    ['goto (parse-goto-statement ll)]
    ['continue (parse-continue-statement ll)]
    ['break (parse-break-statement ll)]
    ['return (parse-return-statement ll)]
    [_ (parse-expr-statement ll)]))



;; labeled-statement ::= identifier ':' statement
(define (parse-labeled-statement ll)
  (ll 'consume)
  (ll 'consume)
  (parse-statement ll))

;; this actually parse all nested case
(define (parse-case-statement ll)
  (let loop ([dummy #f])
    (when (eq? (token-name (ll 0)) 'case)
      (parse-constant-expression ll)
      (ll 'consume)
      (loop))
    (parse-statement ll)))

(define (parse-default-statement ll)
  (ll 'consume)
  (parse-statement ll))

(define (parse-compound-statement ll)
  ;; FIXME track the open and close brace
  (ll 'consume)
  (let loop ([dummy #f])
    (when (not (eq? (token-name (ll 0)) 'r-brace))
      (parse-statement-or-declaration ll))))

(define (parse-null-statement ll)
  (ll 'consume))

;; FIXME scope
(define (parse-if-statement ll)
  (ll 'consume)                         ; if
  (ll 'consume)                         ; (
  (parse-expression ll)
  (ll 'consume)                         ; )
  (parse-statement ll)
  (when (eq? (token-name (ll 0)) 'else)
    (ll 'consume)                       ; else
    (parse-statement ll)))
(define (parse-switch-statement ll)
  (ll 'consume)                         ; switch
  (ll 'consume)                         ; (
  (parse-expression ll)
  (ll 'consume)                         ; )
  (parse-statement ll))
(define (parse-while-statement ll)
  (ll 'consume)                         ; while
  (ll 'consume)                         ; (
  (parse-expression ll)
  (ll 'consume)                         ; )
  (parse-statement ll))
(define (parse-do-statement ll)
  (ll 'consume)
  (parse-statement ll)
  ;; FIXME why use parse-expression here instead of parse-paren-expr-or-condition
  (parse-expression ll))
(define (parse-for-statement ll)
  (ll 'consume)
  ;; FIXME this might be a simple assignment statement
  ;; FIXME check emptyness
  (parse-declaration ll)
  (parse-expression ll)
  (parse-expression ll)
  ;; FIXME GENERAL when consuming, check if it is expected
  (ll 'consume)
  (parse-statement ll))
(define (parse-goto-statement ll)
  (ll 'consume)
  ;; this identifier should be returned
  ;; FIXME not an identifier?
  (when (eq? (token-name (ll 0)) 'identifier)
    (ll 'consume)))
(define (parse-continue-statement ll)
  (ll 'consume))
(define (parse-break-statement ll)
  (ll 'consume))
(define (parse-return-statement ll)
  (ll 'consume)
  (parse-expression ll))

;; TODO
;; OK, I only care about the whole thing inside the expr, so I'm going
;; to use skip-until trick
(define (parse-expr-statement ll)
  ;; (parse-assignment-expression ll)
  ;; (parse-rhs-of-binary-expression ll prec-comma)
  (skip-until ll 'semi-colon))



(module+ test
  (parse-statement (string->ll "int a;")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-constant-expression ll)
  ;; (println "parse-constant-expression")
  (parse-conditional-expression ll))

(define (parse-expression ll)
  ;; (println "parse-expression")
  (for/fold ([res (parse-assignment-expression ll)])
            ([i (in-naturals)]
             #:break (not (eq? (token-name (ll 0)) 'comma)))
    (let ([comma (ll 'consume)])
      (expr res comma
            (parse-assignment-expression ll)))))


;; ::= conditional-expression
;; :: unary-expression assignment-operator assignment-expression
;;
;; Clang divote from the K&R grammar to take unary-expression as
;; conditional-expression, for consistency. After all, the reason to
;; use unary-expression is to specify the lhs should be
;; assignable. This can be checked in semantic pass. For Helium, this
;; is even less important, since Helium tends not to break down
;; expression
(define (parse-assignment-expression ll)
  ;; (println "parse-assignment-expression")
  (let ([lhs (parse-conditional-expression ll)])
    (if (member (token-name (ll 0))
                '(= *= /= %= += -= <<= >>= &= ^= or-assign))
        (let ([op (ll 'consume)])
          (expr:assign lhs op (parse-assignment-expression ll)))
        lhs)))

;; ternary expression ::= lhs ? mid : 3rd
(define (parse-conditional-expression ll)
  ;; (println "parse-conditional-expression")
  (let ([lhs (parse-logical-or-expression ll)])
    (if (eq? (token-name (ll 0)) '?)
        (let ([? (ll 'consume)]
              [mid (parse-expression ll)]
              [: (ll 'consume)]
              [3rd (parse-conditional-expression ll)])
          (expr:ternary lhs ? mid : 3rd))
        lhs)))

(define (parse-logical-or-expression ll)
  ;; (println "parse-logical-or-expression")
  (for/fold ([lhs (parse-logical-and-expression ll)])
            ([i (in-naturals)]
             #:break (not (eq? (token-name (ll 0)) 'or-op)))
    (let ([op (ll 'consume)]
          [rhs (parse-logical-and-expression ll)])
      (expr:or_op lhs op rhs))))

(define (parse-logical-and-expression ll)
  ;; (println "parse-logical-and-expression")
  (for/fold ([lhs (parse-inclusive-or-expression ll)])
            ([i (in-naturals)]
             #:break (not (eq? (token-name (ll 0)) '&&)))
    (let ([op (ll 'consume)]
          [rhs (parse-inclusive-or-expression ll)])
      (expr:&& lhs op rhs))))

(define (parse-inclusive-or-expression ll)
  ;; (println "parse-inclusive-or-expression")
  (for/fold ([lhs (parse-exclusive-or-expression ll)])
            ([i (in-naturals)]
             #:break (not (eq? (token-name (ll 0)) 'or)))
    (let ([op (ll 'consume)]
          [rhs (parse-exclusive-or-expression ll)])
      (expr:or lhs op rhs))))
(define (parse-exclusive-or-expression ll)
  ;; (println "parse-exclusive-or-expression")
  (for/fold ([lhs (parse-and-expression ll)])
            ([i (in-naturals)]
             #:break (not (eq? (token-name (ll 0)) '^)))
    (let ([op (ll 'consume)]
          [rhs (parse-and-expression ll)])
      (expr:xor lhs op rhs))))

(define (parse-and-expression ll)
  ;; (println "parse-and-expression")
  (for/fold ([lhs (parse-equality-expression ll)])
            ([i (in-naturals)]
             #:break (not (eq? (token-name (ll 0)) '&)))
    (let ([op (ll 'consume)]
          [rhs (parse-equality-expression ll)])
      (expr:and lhs op rhs))))

(define (parse-equality-expression ll)
  ;; (println "parse-equality-expression")
  (for/fold ([lhs (parse-relational-expression ll)])
            ([i (in-naturals)]
             #:break (not (member (token-name (ll 0)) '(== !=))))
    (let ([op (ll 'consume)]
          [rhs (parse-relational-expression ll)])
      (expr:equal lhs op rhs))))

(define (parse-relational-expression ll)
  ;; (println "parse-relational-expression")
  (for/fold ([lhs (parse-shift-expression ll)])
            ([i (in-naturals)]
             #:break (not (member (token-name (ll 0)) '(< > <= >=))))
    (let ([op (ll 'consume)]
          [rhs (parse-shift-expression ll)])
      (expr:rel lhs op rhs))))


;; << >>
(define (parse-shift-expression ll)
  ;; (println "parse-shift-expression")
  (for/fold ([lhs (parse-additive-expression ll)])
            ([i (in-naturals)]
             #:break (not (member (token-name (ll 0)) '(<< >>))))
    (let ([op (ll 'consume)]
          [rhs (parse-additive-expression ll)])
      (expr:shift lhs op rhs))))

;; +-
(define (parse-additive-expression ll)
  ;; (println "parse-additive-expression")
  (for/fold ([lhs (parse-multiplicative-expression ll)])
            ([i (in-naturals)]
             #:break (not (member (token-name (ll 0)) '(+ -))))
    (let ([op (ll 'consume)]
          [rhs (parse-multiplicative-expression ll)])
      (expr:add lhs op rhs))))

;; ::= cast-expression
;; ::= mul * / % cast-expression
(define (parse-multiplicative-expression ll)
  ;; (println "parse-multiplicative-expression")
  (for/fold ([lhs (parse-cast-expression ll)])
            ([i (in-naturals)]
             #:break (not (member (token-name (ll 0)) '(* / %))))
    (let ([op (ll 'consume)]
          [rhs (parse-cast-expression ll)])
      (expr:mult lhs op rhs))))

;; ::= unary-expression
;; ::= ( type-name ) cast-expression
(define (parse-cast-expression ll)
  ;; (println "parse-cast-expression")
  (let ([unary (try-parse parse-unary-expression ll)])
    (if unary unary
        (when (eq? (token-name (ll 0)) 'l-paren)
          (let ([l-paren (ll 'consume)]
                [type-name (skip-until ll 'r-paren)]
                [r-paren (ll 'consume)]
                [inner (parse-cast-expression ll)])
            (expr:cast l-paren type-name r-paren inner))))))



(define (parse-unary-expression ll)
  ;; (println "parse-unary-expression")
  (case (token-name (ll 0))
    [(++ --) (let ([op (ll 'consume)])
               (expr:unary op (parse-unary-expression ll)))]
    ;; ::= sizeof unary-expression
    ;; ::= sizeof ( type-name )
    [(sizeof) (begin
                (let ([op (ll 'consume)])
                  (let ([res (try-parse #'parse-unary-expression ll)])
                    (if res
                        (expr:unary op res)
                        (when (eq? (token-name (ll 0)) 'l-paren)
                          (let ([l-paren (ll 'consume)]
                                [type-name (skip-until ll 'r-paren)]
                                [r-paren (ll 'consume)])
                            (expr:unary op l-paren type-name r-paren)))))))]
    ;; ::= unary-operator cast-expression
    [(& * + - ~ !) (let ([op (ll 'consume)])
                     (expr:unary op (parse-cast-expression ll)))]
    ;; ::= postfix-expression
    [else (parse-postfix-expression ll)]))

(define (parse-identifier ll)
  (when (not (eq? (token-name (ll 0)) 'identifier))
    (raise-syntax-error #f "Not identifier"))
  (id (token-value (ll 'consume))))

;; TODO (typename) {initializer-list}
(define (parse-postfix-expression ll)
  ;; (println "parse-postfix-expression")
  (let ([prim (parse-primary-expression ll)])
    (for/fold ([res prim])
              ([i (in-naturals)]
               #:break (not (member (token-name (ll 0))
                                    '(l-bracket l-paren period -> ++ --))))
      (case (token-name (ll 0))
        [(l-bracket) (begin
                       
                       (let ([l-bracket (ll 'consume)]
                             [post (parse-expression ll)]
                             [r-bracket (ll 'consume)])
                         (expr:postfix res l-bracket post r-bracket)))]
        [(l-paren) (begin
                     (let ([l-paren (ll 'consume)])
                       (when (not (eq? (token-name (ll 0)) 'r-paren))
                         (let ([arg-list (parse-argument-expression-list ll)]
                               [r-paren (ll 'consume)])
                           (expr:postfix res l-paren arg-list r-paren)))))]
        [(period ->) (begin
                       (let ([op (ll 'consume)])
                         (let ([post (parse-identifier ll)])
                           (expr:postfix res op post))))]
        [(++ --) (expr:postfix res (ll 'consume))]
        [else (raise-syntax-error #f (format "Error ~a" (ll 0)))]))))

(define (parse-argument-expression-list ll)
  (parse-assignment-expression ll)
  (for ([i (in-naturals)]
        #:break (not (eq? (token-name (ll 0)) 'comma)))
    (parse-assignment-expression ll)))

(define (parse-primary-expression ll)
  ;; (println "parse-primary-expression")
  (case (token-name (ll 0))
    [(identifier) (id (token-value (ll 'consume)))]
    [(i-constant) (number (token-value (ll 'consume)))]
    [(f-constant) (number (token-value (ll 'consume)))]
    [(string-literal) (expr:string (token-value (ll 'consume)))]
    [(l-paren) (let ([l-paren (ll 'consume)]
                     [inner (parse-expression ll)]
                     [r-paren (expect-consume ll 'r-paren)])
                 (expr:paren l-paren inner r-paren))]
    [else (raise-syntax-error #f (format "Error ~a" (ll 0)))]))


(module+ test
  (check-equal? (parse-assignment-expression (string->ll "a=b;"))
                (expr:assign '#s(id "a") (token-= "=") '#s(id "b")))
  (check-equal? (parse-expression (string->ll "a"))
                (id "a"))
  (check-equal? (parse-expression (string->ll "(a)"))
                (expr:paren (token-l-paren "(") '#s(id "a") (token-r-paren ")")))
  (check-equal? (parse-expression (string->ll "a+b+c"))
                (expr:add (expr:add (id "a")
                                    (token-+ "+")
                                    (id "b"))
                          (token-+ "+")
                          (id "c")))
  (check-equal? (parse-expression (string->ll "a+b*c*d-x"))
                (expr:add
                 (expr:add
                  (id "a") (token-+ "+")
                  (expr:mult (expr:mult '#s(id "b") (token-* "*") '#s(id "c"))
                             (token-* "*")
                             (id "d")))
                 (token-- "-")
                 (id "x")))
  (check-equal? (parse-expression (string->ll "a>>b<<c"))
                (expr:shift
                 (expr:shift (id "a") (token->> ">>") (id "b"))
                 (token-<< "<<")
                 (id "c"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Decl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; corresponding to parse-simple-declaration, the non-simple ones are
;; for C++
(define (parse-declaration ll)
  (println "parse-declaration")
  (parse-declaration-specifiers ll)
  (parse-decl-group ll))

 (define (parse-declaration-specifiers ll)
  (println "parse-declaration-specifiers")
  (case (token-name (ll 0))
    ;; storage class specifier
    ['typedef #f]
    ['extern #f]
    ['static #f]
    ['auto #f]
    ['register #f]
    ;; function specifier
    ['inline #f]
    ;; type specifier
    ['short #f]
    ['long #f]
    ['signed #f]
    ['unsigned #f]
    ['void #f]
    ['char #f]
    ['int #f]
    ['float #f]
    ['double #f]
    ['bool #f]
    ;; class specifier
    ['struct #f]
    ['union #f]
    ;; enum specifier
    ['enum #f]
    ;; cv qualifier
    ['const #f]
    ['volatile #f]
    ['restrict #f]
    ))

(define (parse-decl-group ll)
  (println "parse-decl-group")
  (parse-declarator ll)
  ;; FIXME use conditional to choose between function definition and
  ;; simple declarator
  (parse-function-definition ll)
  (let loop ([comma (try-consume ll 'comma)])
    (when (not comma)
      (parse-declarator)
      (loop (try-consume ll 'comma)))))

;; TODO we can actually skip-function-body, for the sake of:
;; 1. extract all functions
;; 2. extract all typedefs
(define (parse-function-definition ll)
  (print "parse-function-definition")
  ;; FIXME conditional
  (parse-knr-param-declarations)
  (parse-compound-statement-body))

(define (declarator-specifier? tok)
  (case (token-name tok)
    ;; storage class specifier
    [('typedef 'extern 'static 'auto 'register) #t]
    ;; type specifiers
    [('short 'long 'signed 'unsigned 'void 'char 'int 'float 'double 'bool) #t]
    ;; struct-or-union-specifier
    [('struct 'union 'enum) #t]
    ;; type qualifier
    [('const 'volatile 'restrict) #t]
    ;; function specifier
    [('inline) #t]
    [else #f]))

(define (parse-knr-param-declarations ll)
  (let loop ([specifier? (declarator-specifier? (ll 0))])
    (when specifier?
      (parse-declaration-specifiers ll)
      (parse-declarator ll)
      ;; FIXME seems to have another loop for parse-declarator
      )))

;; TODO What is this??
(define (parse-compound-statement-body ll)
  ;; finally loop back ...
  (parse-statement-or-declaration ll))

(define (parse-declarator ll)
  (println "parse-declarator")
  (println (ll 0))
  (parse-type-qualifier-list-opt ll)
  (if (eq? (token-name (ll 0)) '*)
      ;; pointer
      (begin
        (parse-type-qualifier-list-opt ll)
        ;; recursive parse internal
        (parse-declarator ll))
      ;; FIXME
      (begin
        (parse-type-qualifier-list-opt ll)
        (parse-declarator ll))))

;; TODO What's this??
(define (parse-type-qualifier-list-opt ll)
  (let loop ([dummy 1])
    (case (token-name (ll 0))
      ['const (loop)]
      ['volatile (loop)]
      ['restrict (loop)]
      [else #f])))

