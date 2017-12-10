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

(define (file->ll f)
  (ll-lexer (file-lexer f)))

(define (print-all-tokens ll)
  (for ([i (in-naturals)]
        #:break (eq? (get-token-name (ll 0)) 'eof))
    (println (ll 0))
    (ll 'consume)))

(module+ test
  #;
  (let ([ll (file->ll "/home/hebi/github/benchmark/craft/grammar/a.c")])
    (println (ll 0))
    (print-all-tokens ll))
  (let ([ll (string->ll "/* this is comment */ int a;")])
    (print-all-tokens ll))
  (let ([ll (string->ll "1 2 3 4 5 6 7 8 9")])
    (check-equal? (get-token-value (ll 0)) "1")
    (check-equal? (get-token-value (ll 1)) "2")
    (ll 'consume)
    (check-equal? (get-token-value (ll 0)) "2")
    (check-equal? (get-token-value (ll 1)) "3")
    (ll 'consume)
    (ll 'consume)
    (check-equal? (get-token-value (ll 0)) "4")
    (check-equal? (get-token-value (ll 1)) "5"))
  (let ([ll (string->ll "1 2 3 4 5 6 7 8 9")])
    (ll 'track)
    (ll 'consume)                       ; 1
    (ll 'consume)                       ; 2
    (ll 'track)
    (ll 'consume)                       ; 3
    (check-equal? (get-token-value (ll 0)) "4")
    (ll 'track)
    (ll 'consume)                       ; 4
    (ll 'consume)                       ; 5
    (check-equal? (get-token-value (ll 0)) "6")
    (ll 'pop)                           ; drop 4 5
    (check-equal? (get-token-value (ll 0)) "6")
    (check-equal? (get-token-value (ll 1)) "7")
    (ll 'restore)                       ; (3) (6 7 ..
    (check-equal? (get-token-value (ll 0)) "3")
    (check-equal? (get-token-value (ll 1)) "6")
    (ll 'restore)
    (check-equal? (get-token-value (ll 0)) "1") ; (1 2 3) (6 7 ..
    (check-equal? (get-token-value (ll 1)) "2")))

;; (define (parse-program str)
;;   (let ([lexer (string-lexer str)])
;;     (parse lexer)))

;; TODO general checking EOF
;; TODO general consume token

(define (try-consume ll target)
  (if (or (eq? (get-token-name (ll 0)) target)
          (and (list? target)
               (member (get-token-name (ll 0)) target)))
      (ll 'consume) #f))

(define (expect-consume ll target)
  (if (or (eq? (get-token-name (ll 0)) target)
          (and (list? target)
               (member (get-token-name (ll 0)) target)))
      (ll 'consume)
      (raise-syntax-error
       #f
       (format "Error expect-consume ~a, but see ~a" target (ll 0)))))

(define (skip-until ll target)
  ;; returns a list of tokens skipped
  ;; target can be a single token symbol, or a list of them
  (if (or (eq? (get-token-name (ll 0)) target)
          (and (list? target) (member (get-token-name (ll 0)) target)))
      (list)
      (case (get-token-name (ll 0))
        ['l-paren (cons (ll 'consume) (skip-until ll 'r-paren))]
        ['l-bracket (cons (ll 'consume) (skip-until ll 'r-bracket))]
        ['l-brace (cons (ll 'consume) (skip-until ll 'r-brace))]
        ;; FIXME unbalanced parens
        ;; semicolon force return
        ['semi-colon (raise-syntax-error
                      #f (format "Skip until ~a, but reach semi-colon."
                                 target))]
        ['eof (raise-syntax-error
               #f (format "Skip until ~a, but reach EOF" target))]
        [else (let ([cur (ll 'consume)])
                (cons (ll 0) (skip-until ll target)))])))



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

(define (parse-statement ll)
  (case (get-token-name (ll 0))
    [(identifier) (if (eq? (get-token-name (ll 1)) ':)
                      (parse-labeled-statement ll)
                      (parse-expr-statement ll))]
    ;; if is part of declaration specifier
    [(
      typedef extern static auto register
      short long signed unsigned void char int float double bool
      struct union enum
      const volatile restrict inline)
     (parse-declaration ll)]
    [(case) (parse-case-statement ll)]
    [(default) (parse-default-statement ll)]
    [(l-brace) (parse-compound-statement ll)]
    [(if) (parse-if-statement ll)]
    [(switch) (parse-switch-statement ll)]
    [(while) (parse-while-statement ll)]
    [(do) (parse-do-statement ll)]
    [(for) (parse-for-statement ll)]
    [(goto) (parse-goto-statement ll)]
    [(continue) (parse-continue-statement ll)]
    [(break) (parse-break-statement ll)]
    [(return) (parse-return-statement ll)]
    [else (parse-expr-statement ll)]))

;; labeled-statement ::= identifier ':' statement
;; 
(define (parse-labeled-statement ll)
  (let ([label (expect-consume ll 'identifier)]
        [colon (expect-consume ll ':)]
        [stmt (parse-statement ll)])
    (stmt:label label colon stmt)))

(define (parse-case-statement ll)
  (let ([kw (expect-consume ll 'case)]
        [expr (parse-constant-expression ll)]
        [colon (expect-consume ll ':)]
        [body (parse-statement ll)])
    (stmt:case kw expr colon body)))

(define (parse-default-statement ll)
  (let ([kw (expect-consume ll 'default)]
        [body (parse-statement ll)])
    (stmt:default kw body)))

(define (parse-compound-statement ll)
  (let ([lbrace (expect-consume ll 'l-brace)])
    (stmt:comp lbrace
               (for/list ([i (in-naturals)]
                          #:break (eq? (get-token-name (ll 0)) 'r-brace))
                 (parse-statement ll))
               (expect-consume ll 'r-brace))))

(define (parse-if-statement ll)
  (let ([if-kw (expect-consume ll 'if)]
        [lparen (expect-consume ll 'l-paren)]
        [c (parse-expression ll)]
        [rparen (expect-consume ll 'r-paren)]
        [t (parse-statement ll)])
    (if (eq? (get-token-name (ll 0)) 'else)
        (let ([else-kw (expect-consume ll 'else)]
              [f (parse-statement ll)])
          (stmt:if if-kw lparen c rparen t else-kw f))
        (stmt:if if-kw lparen c rparen t #f #f))))

(define (parse-switch-statement ll)
  (let ([kw (expect-consume ll 'switch)]
        [l (expect-consume ll 'l-paren)]
        [expr (parse-expression ll)]
        [r (expect-consume ll 'r-paren)]
        [body (parse-statement ll)])
    (stmt:switch kw l expr r body)))

(define (parse-while-statement ll)
  (let ([kw (expect-consume ll 'while)]
        [l (expect-consume ll 'l-paren)]
        [test (parse-expression ll)]
        [r (expect-consume ll 'r-paren)]
        [body (parse-statement ll)])
    (stmt:while kw l test r body)))

(define (parse-do-statement ll)
  (let ([do-kw (expect-consume ll 'do)]
        [body (parse-statement ll)]
        [while-kw (expect-consume ll 'while)]
        [l (expect-consume ll 'l-paren)]
        [test (parse-expression ll)]
        [r (expect-consume ll 'r-paren)])
    (stmt:do do-kw body while-kw l test r)))

(define (parse-for-statement ll)
  (let ([kw (expect-consume ll 'for)]
        [l (expect-consume ll 'l-paren)]
        ;; FIXME conform to K&R
        ;; will eat first ;
        [e1 (parse-statement ll)]
        ;; will eat second ;
        [e2 (parse-expr-statement ll)]
        [e3 (if (eq? (get-token-name (ll 0)) 'r-paren)
                #f
                (parse-expression ll))]
        [r (expect-consume ll 'r-paren)]
        [body (parse-statement ll)])
    (stmt:for kw l e1 e2 e3 r body)))
(define (parse-goto-statement ll)
  (let ([goto (expect-consume ll 'goto)]
        [label (expect-consume ll 'identifier)]
        [semi (expect-consume ll 'semi-colon)])
    (stmt:goto goto label semi)))
(define (parse-continue-statement ll)
  (stmt:continue (expect-consume ll 'continue)
                 (expect-consume ll 'semi-colon)))
(define (parse-break-statement ll)
  (stmt:break (expect-consume ll 'break) (expect-consume ll 'semi-colon)))


(define (parse-return-statement ll)
  (let ([kw (expect-consume ll 'return)]
        [expr (if (eq? (get-token-name (ll 0)) 'semi)
                  #f
                  (parse-expression ll))]
        [semi (expect-consume ll 'semi-colon)])
    (stmt:return kw expr semi)))

(define (parse-expr-statement ll)
  (if (eq? (get-token-name (ll 0)) 'semi-colon)
      (stmt:empty (expect-consume ll 'semi-colon))
      (let ([expr (parse-expression ll)]
            [semi (expect-consume ll 'semi-colon)])
        (stmt:expr expr semi))))


(module+ test
  (parse-statement (string->ll "if (a>0) a=8;"))
  (parse-statement (string->ll "aa: goto b;"))
  (parse-case-statement (string->ll "case 5: goto ddd;"))
  (parse-return-statement (string->ll "return a;")))

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
             #:break (not (eq? (get-token-name (ll 0)) 'comma)))
    (let ([comma (expect-consume ll 'comma)])
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
    (let ([op (try-consume
               ll '(= *= /= %= += -= <<= >>= &= ^= or-assign))])
      (if op (expr:assign lhs op (parse-assignment-expression ll)) lhs))))

;; ternary expression ::= lhs ? mid : 3rd
(define (parse-conditional-expression ll)
  ;; (println "parse-conditional-expression")
  (let ([lhs (parse-logical-or-expression ll)])
    (if (eq? (get-token-name (ll 0)) '?)
        (let ([? (expect-consume ll '?)]
              [mid (parse-expression ll)]
              [: (expect-consume ll ':)]
              [3rd (parse-conditional-expression ll)])
          (expr:ternary lhs ? mid : 3rd))
        lhs)))


(define (left-recursive-helper ll child-parser ops constructor)
  (for/fold ([lhs (#%app child-parser ll)])
            ([i (in-naturals)]
             #:break (not (member (get-token-name (ll 0)) ops)))
    (let ([op (expect-consume ll ops)]
          [rhs (child-parser ll)])
      (constructor lhs op rhs))))

(define (parse-logical-or-expression ll)
  (left-recursive-helper
   ll parse-logical-and-expression '(or-op) expr:or_op))

(define (parse-logical-and-expression ll)
  (left-recursive-helper
   ll parse-inclusive-or-expression '(&&) expr:&&))

(define (parse-inclusive-or-expression ll)
  (left-recursive-helper
   ll parse-exclusive-or-expression '(or) expr:or))
(define (parse-exclusive-or-expression ll)
  (left-recursive-helper
   ll parse-and-expression '(^) expr:xor))

(define (parse-and-expression ll)
  (left-recursive-helper
   ll parse-equality-expression '(&) expr:and))

(define (parse-equality-expression ll)
  (left-recursive-helper
   ll parse-relational-expression '(== !=) expr:equal))

(define (parse-relational-expression ll)
  (left-recursive-helper
   ll parse-shift-expression '(< > <= >=) expr:rel))

(define (parse-shift-expression ll)
  (left-recursive-helper
   ll parse-additive-expression '(<< >>) expr:shift))

(define (parse-additive-expression ll)
  (left-recursive-helper
   ll parse-multiplicative-expression '(+ -) expr:add))

;; ::= cast-expression
;; ::= mul * / % cast-expression
(define (parse-multiplicative-expression ll)
  (left-recursive-helper
   ll parse-cast-expression '(* / %) expr:mult))

;; ::= unary-expression
;; ::= ( type-name ) cast-expression
(define (parse-cast-expression ll)
  (let ([unary (try-parse parse-unary-expression ll)])
    (if unary unary
        (when (eq? (get-token-name (ll 0)) 'l-paren)
          (let ([l-paren (expect-consume ll 'l-paren)]
                [type-name (skip-until ll 'r-paren)]
                [r-paren (expect-consume ll 'r-paren)]
                [inner (parse-cast-expression ll)])
            (expr:cast l-paren type-name r-paren inner))))))

(define (parse-unary-expression ll)
  ;; (println "parse-unary-expression")
  (case (get-token-name (ll 0))
    [(++ --) (let ([op (ll 'consume)])
               (expr:unary op (parse-unary-expression ll)))]
    ;; ::= sizeof unary-expression
    ;; ::= sizeof ( type-name )
    [(sizeof) (begin
                (let ([op (ll 'consume)])
                  (let ([res (try-parse #'parse-unary-expression ll)])
                    (if res
                        (expr:unary op res)
                        (when (eq? (get-token-name (ll 0)) 'l-paren)
                          (let ([l-paren (expect-consume ll 'l-paren)]
                                [type-name (skip-until ll 'r-paren)]
                                [r-paren (expect-consume ll 'r-paren)])
                            (expr:unary op l-paren type-name r-paren)))))))]
    ;; ::= unary-operator cast-expression
    [(& * + - ~ !) (let ([op (ll 'consume)])
                     (expr:unary op (parse-cast-expression ll)))]
    ;; ::= postfix-expression
    [else (parse-postfix-expression ll)]))

;; TODO (typename) {initializer-list}
(define (parse-postfix-expression ll)
  ;; (println "parse-postfix-expression")
  (let ([prim (parse-primary-expression ll)])
    (for/fold ([res prim])
              ([i (in-naturals)]
               #:break (not (member (get-token-name (ll 0))
                                    '(l-bracket l-paren period -> ++ --))))
      (case (get-token-name (ll 0))
        [(l-bracket) (begin
                       
                       (let ([l-bracket (expect-consume ll 'l-bracket)]
                             [post (parse-expression ll)]
                             [r-bracket (expect-consume ll 'r-bracket)])
                         (expr:postfix res l-bracket post r-bracket)))]
        [(l-paren) (begin
                     (let ([l-paren (expect-consume ll 'l-paren)])
                       (when (not (eq? (get-token-name (ll 0)) 'r-paren))
                         (let ([arg-list (parse-argument-expression-list ll)]
                               [r-paren (expect-consume ll 'r-paren)])
                           (expr:postfix res l-paren arg-list r-paren)))))]
        [(period ->) (begin
                       (let ([op (ll 'consume)])
                         (let ([post (id (expect-consume ll 'identifier))])
                           (expr:postfix res op post))))]
        [(++ --) (expr:postfix res (ll 'consume))]
        [else (raise-syntax-error #f (format "Error ~a" (ll 0)))]))))

(define (parse-argument-expression-list ll)
  (parse-assignment-expression ll)
  (for ([i (in-naturals)]
        #:break (not (eq? (get-token-name (ll 0)) 'comma)))
    (parse-assignment-expression ll)))

(define (parse-primary-expression ll)
  ;; (println "parse-primary-expression")
  (case (get-token-name (ll 0))
    [(identifier) (id (ll 'consume))]
    [(i-constant) (number (ll 'consume))]
    [(f-constant) (number (ll 'consume))]
    [(string-literal) (expr:string (ll 'consume))]
    [(l-paren) (let ([l-paren (ll 'consume)]
                     [inner (parse-expression ll)]
                     [r-paren (expect-consume ll 'r-paren)])
                 (expr:paren l-paren inner r-paren))]
    [else (raise-syntax-error #f "Error parse-primary-expression"
                              (ll 0))]))


(module+ test
  (parse-assignment-expression (string->ll "a=b;"))
  (parse-expression (string->ll "a"))
  (parse-expression (string->ll "(a)"))
  (parse-expression (string->ll "a+b+c"))
  (parse-expression (string->ll "a+b*c*d-x"))
  (parse-expression (string->ll "a>>b<<c")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Decl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#;
(module+ test
  (begin
    (parse-translation-unit
     (ll-lexer
      (file-lexer
       "/home/hebi/github/benchmark/download/findutils/find/parser.c")))
    (void)))

;; ::= external-declaration | translation-unit external-declaration
(define (parse-translation-unit ll)
  (decl:trans_unit
   (for/list ([i (in-naturals)]
              #:break (eq? (get-token-name (ll 0)) 'eof))
     (parse-external-declaration ll))))

;; ::= function-definition | declaration
(define (parse-external-declaration ll)
  (if (external-declaration-is-function? ll)
      (parse-function-definition ll)
      (parse-declaration ll)))

;; we can actually skip-function-body, for the sake of:
;; 1. extract all functions
;; 2. extract all typedefs
;; ::= declaration-specifiers declarator declaration-list_opt compound-stmt
;; declaration-list ::= declaration-list declaration
(define (parse-function-definition ll)
  (let ([specifiers (parse-declaration-specifiers ll)]
        [declarator (parse-declarator ll)]
        [krstyle (for/list ([i (in-naturals)]
                            #:break (eq? (get-token-name (ll 0))
                                         'l-brace))
                   (parse-declaration ll))]
        ;; FIXME maybe just a declaration, not a definition
        [body (parse-compound-statement ll)])
    (decl:function specifiers declarator krstyle body)))

;; ::= declaration-specifiers init-declarator-list_opt
(define (parse-declaration ll)
  (let ([specifiers (parse-declaration-specifiers ll)]
        [declarators (parse-init-declarator-list ll)]
        [semi (expect-consume ll 'semi-colon)])
    (decl:decl specifiers declarators semi)))

(define (external-declaration-is-function? ll)
  (ll 'track)
  (let ([specifiers (parse-declaration-specifiers ll)]
        [declarator (parse-declarator ll)])
    (ll 'restore)
    (let ([suffix (decl:declarator-suffix declarator)])
      (if (decl:param_list? suffix) #t #f))))

;; declarator ::= pointer_opt direct-declarator
;; direct-declarator ::= identifier | (declarator)
;; ===== array
;; direct-declarator ::= direct-declarator[][][]
;; ===== for function parameters
;; direct-declarator ::= direct-declarator(parameter-type-list)
;; direct-declarator ::= direct-declarator(identifier-list_opt)
(define (parse-declarator ll)
  (let ([pointer (parse-pointer ll)])
    (let ([direct (case (get-token-name (ll 0))
                    [(identifier) (id (ll 'consume))]
                    [(l-paren) (let ([l (expect-consume ll 'l-paren)]
                                     [inner (parse-declarator ll)]
                                     [r (expect-consume ll 'r-paren)])
                                 (decl:paren l inner r))]
                    [else (raise-syntax-error
                           #f (format
                               "parse-declarator expect direct, get ~a"
                               (ll 0)))])]
          [suffix (case (get-token-name (ll 0))
                    [(l-paren) (let ([lparen (expect-consume ll 'l-paren)]
                                     [params (parse-parameter-list ll)]
                                     [rparen (expect-consume ll 'r-paren)])
                                 (decl:param_list lparen params rparen))]
                    [(l-bracket)
                     (for/list ([i (in-naturals)]
                                #:break (not (eq? (get-token-name (ll 0))
                                                  'l-bracket)))
                       (let ([l (expect-consume ll 'l-bracket)]
                             [inner (skip-until ll 'r-bracket)]
                             [r (expect-consume ll 'r-bracket)])
                         (type:array l inner r)))]
                    [else #f])])
      (decl:declarator pointer direct suffix))))

(define (parse-parameter-list ll)
  (if (member (get-token-name (ll 1)) '(comma r-paren))
      ;; parse k&r style
      (let ([first (id (expect-consume ll 'identifier))])
        (cons first (for/list ([i (in-naturals)]
                               #:break (not (eq? (get-token-name (ll 0)) 'comma)))
                      ;; FIXME track this comma
                      (expect-consume ll 'comma)
                      (id (expect-consume ll 'identifier)))))
      ;; parse regular parameters
      (let ([first (parse-parameter-declaration ll)])
        (cons first (for/list ([i (in-naturals)]
                               #:break (not (eq? (get-token-name (ll 0)) 'comma)))
                      ;; FIXME track this comma
                      (expect-consume ll 'comma)
                      (parse-parameter-declaration ll))))))

;; parameter-declaration ::= declaration-specifiers declarator
;; parameter-declaration ::= declaration-specifiers abstract-declarator_opt
;; abstract-declarator ::= pointer | pointer_opt direct-abstract-declarator
(define (parse-parameter-declaration ll)
  (let ([specifiers (parse-declaration-specifiers ll)])
    (if (member (get-token-name (ll 0)) '(comma r-paren))
        (decl:param specifiers #f)
        (let ([decl (parse-declarator ll)])
          (decl:param specifiers decl)))))

(module+ test
  (parse-parameter-list (string->ll "int a, char b"))
  (parse-declaration (string->ll "int a=8;"))
  (parse-declaration (string->ll "char **a[], b;"))
  (parse-translation-unit
   (string->ll "int a;char **b[5];void foo() {return a;}")))


(define (parse-declaration-specifiers ll)
  (for/list ([i (in-naturals)]
             #:break
             (not
              (member
               (get-token-name (ll 0))
               (append
                ;; storage class specifier
                '(typedef extern static auto register)
                ;; function specifier
                '(inline)
                ;; type specifier
                ;; FIXME typename
                '(short long signed unsigned void char int float double bool)
                ;; class specifier
                ;; enum specifier
                '(struct union enum)
                ;; type qualifier
                '(const volatile restrict)))))
    (ll 'consume)))

;; init-declarator, init-declarator ...
(define (parse-init-declarator-list ll)
  (for/fold ([res (list (parse-init-declarator ll))])
            ([i (in-naturals)]
             #:break (not (eq? (get-token-name (ll 0)) 'comma)))
    (expect-consume ll 'comma)
    (append res (list (parse-init-declarator ll)))))


;; init-declarator ::= declarator | declarator = initializer
(define (parse-init-declarator ll)
  (let ([declarator (parse-declarator ll)])
    (if (eq? (get-token-name (ll 0)) '=)
        (let ([= (expect-consume ll '=)]
              [initializer (parse-initializer ll)])
          (decl:init_declarator declarator = initializer))
        (decl:init_declarator declarator #f #f))))

;; initializer ::= assignment-expression
;;   | { initializer-list , }
(define (parse-initializer ll)
  (if (eq? (get-token-name (ll 0)) 'l-brace)
      (let ([l (expect-consume ll 'l-brace)]
            [inits (skip-until ll 'r-brace)]
            [r (expect-consume ll 'r-brace)])
        (decl:initializer l inits r))
      (decl:initializer #f (parse-assignment-expression ll) #f)))

;; pointer ::= * type-qualifier-list_opt
;; pointer ::= * type-qualifier-list_opt pointer
;; type-qualifier-list ::= type-qualifier-list type-qualifier
;; type-qualifier ::= const | restrict | volatile
(define (parse-pointer ll)
  (for/list ([i (in-naturals)]
             #:break (not (member (get-token-name (ll 0))
                                  '(* const restrict volatile))))
    (ll 'consume)))
