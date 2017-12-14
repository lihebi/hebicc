#lang racket

(require "token.rkt")
(require "lexer.rkt")
(require parser-tools/lex
         parser-tools/yacc)


(module+ test
  (let ([parser (lr-parser)]
        [lexer (string-lexer
                "
int foo() {if (a) if (b) a=b;else a=b;}")])
    (parser (lambda () (lexer)))))

(define (lr-parser)
  (parser
   (start translation-unit
          )
   (end eof)
   (src-pos)
   (tokens t et)
   (error (lambda (token-ok? token-name token-value start-pos end-pos)
            (printf "~a ~a ~a ~a ~a ~n" token-ok? token-name token-value start-pos end-pos)
            (raise-syntax-error #f "Parser Error")))
   ;; resolve dangling else problem
   (precs (nonassoc r-paren)
          (nonassoc else))
   (debug "debug.txt")
   (grammar

    ;; ==============================
    ;; External definitions
    ;; ==============================
    (translation-unit [(external-declaration) #f]
                      [(translation-unit external-declaration) #f])
    (external-declaration [(function-definition) #f]
                          [(declaration) #f])
    (function-definition [(declaration-specifiers declarator declaration-list? compound-statement) #f])
    (declaration-list? [() #f]
                       [(declaration-list) #f])
    (declaration-list [(declaration) #f]
                      [(declaration-list declaration) #f])

    ;; ==============================
    ;; expressions
    ;; ==============================
    (constant-expression [(conditional-expression) #f])
    (expression [(assignment-expression) #f]
                [(expression comma assignment-expression) #f])
    (assignment-expression [(conditional-expression) #f]
                           [(unary-expression
                             assignment-operator
                             assignment-expression)
                            #f])
    (conditional-expression [(logical-or-expression) #f]
                            [(logical-or-expression
                              ? expression
                              : conditional-expression)
                             #f])
    (assignment-operator [(=) #f]
                         [(*=) #f]
                         [(/=) #f]
                         [(%=) #f]
                         [(+=) #f]
                         [(-=) #f]
                         [(<<=) #f]
                         [(>>=) #f]
                         [(&=) #f]
                         [(^=) #f]
                         [(or-assign) #f])
    (logical-or-expression [(logical-and-expression) #f]
                           [(logical-or-expression
                             or-op logical-and-expression) #f])
    (logical-and-expression [(inclusive-or-expression) #f]
                            [(logical-and-expression
                              && inclusive-or-expression) #f])
    (inclusive-or-expression [(exclusive-or-expression) #f]
                             [(inclusive-or-expression
                               or exclusive-or-expression) #f])
    (exclusive-or-expression [(and-expression) #f]
                             [(exclusive-or-expression ^ and-expression) #f])
    (and-expression [(equality-expression) #f]
                    [(and-expression & equality-expression) #f])
    (equality-expression [(relational-expression) #f]
                         [(equality-expression == relational-expression) #f]
                         [(equality-expression != relational-expression) #f])
    (relational-expression [(shift-expression) #f]
                           [(relational-expression < shift-expression) #f]
                           [(relational-expression > shift-expression) #f]
                           [(relational-expression <= shift-expression) #f]
                           [(relational-expression >= shift-expression) #f])
    (shift-expression [(additive-expression) #f]
                      [(shift-expression << additive-expression) #f]
                      [(shift-expression >> additive-expression) #f])
    (additive-expression [(multiplicative-expression) #f]
                         [(additive-expression + multiplicative-expression) #f]
                         [(additive-expression - multiplicative-expression) #f])
    (multiplicative-expression [(cast-expression) #f]
                               [(multiplicative-expression * cast-expression) #f]
                               [(multiplicative-expression / cast-expression) #f]
                               [(multiplicative-expression % cast-expression) #f])
    (cast-expression [(unary-expression) #f]
                     [(l-paren type-name r-paren cast-expression) #f])
    (unary-expression [(postfix-expression) #f]
                      [(++ unary-expression) #f]
                      [(-- unary-expression) #f]
                      [(unary-operator cast-expression) #f]
                      [(sizeof unary-expression) #f]
                      [(sizeof l-paren type-name r-paren) #f])
    (postfix-expression [(primary-expression) #f]
                        [(postfix-expression l-bracket expression r-bracket) #f]
                        [(postfix-expression l-paren r-paren) #f]
                        [(postfix-expression l-paren argument-expression-list r-paren) #f]
                        [(postfix-expression period identifier) #f]
                        [(postfix-expression -> identifier) #f]
                        [(postfix-expression ++) #f]
                        [(postfix-expression --) #f]
                        [(l-paren type-name r-paren l-brace initializer-list r-brace) #f]
                        [(l-paren type-name r-paren l-brace initializer-list comma r-brace) #f])
    (unary-operator [(&) #f]
                    [(*) #f]
                    [(+) #f]
                    [(-) #f]
                    [(~) #f]
                    [(!) #f])
    (primary-expression [(identifier) #f]
                        [(constant) #f]
                        [(string-literal) #f]
                        [(l-paren expression r-paren) #f])
    (constant [(i-constant) #f]
              [(f-constant) #f])
    (argument-expression-list [(assignment-expression) #f]
                              [(argument-expression-list comma assignment-expression) #f])

    ;; ==============================
    ;; declarations
    ;; ==============================
    (declaration [(declaration-specifiers init-declarator-list? semi-colon) #f])

    ;; specifiers
    (declaration-specifiers [(storage-class-specifier declaration-specifiers?) #f]
                            [(type-specifier declaration-specifiers?) #f]
                            [(type-qualifier declaration-specifiers?) #f]
                            [(function-specifier declaration-specifiers?) #f])
    (type-name [(specifier-qualifier-list) #f]
               [(specifier-qualifier-list abstract-declarator) #f])
    (specifier-qualifier-list [(type-specifier) #f]
                              [(type-specifier specifier-qualifier-list) #f]
                              [(type-qualifier) #f]
                              [(type-qualifier specifier-qualifier-list) #f])
    (type-qualifier-list [(const) #f]
                         [(restrict) #f]
                         [(volatile) #f])
    (type-specifier [(void) #f]
                    [(char) #f]
                    [(short) #f]
                    [(int) #f]
                    [(long) #f]
                    [(float) #f]
                    [(double) #f]
                    [(signed) #f]
                    [(unsigned) #f]
                    [(bool) #f]
                    [(complex) #f]
                    ;; this seems to be more complex
                    [(struct-or-union-specifier) #f]
                    [(enum-specifier) #f]
                    [(typedef-name) #f])
    (type-qualifier [(const) #f]
                    [(restrict) #f]
                    [(volatile) #f])
    (storage-class-specifier [(typedef) #f]
                             [(extern) #f]
                             [(static) #f]
                             [(auto) #f]
                             [(register) #f])
    (function-specifier [(inline) #f])
    (identifier-list [(identifier) #f]
                     [(identifier-list comma identifier) #f])
    ;; FIXME typedef name
    (enumerator-constant [(identifier) #f])

    ;; structure
    (struct-or-union-specifier [(struct-or-union identifier? l-brace struct-declaration-list r-brace) #f]
                               [(struct-or-union identifier) #f])
    (struct-or-union [(struct) #f]
                     [(union) #f])
    (enum-specifier [(enum identifier? l-brace enumerator-list r-brace) #f]
                    [(enum identifier? l-brace enumerator-list comma r-brace) #f]
                    [(enum identifier) #f])
    (struct-declaration [(specifier-qualifier-list struct-declarator-list) #f])
    (enumerator [(enumeration-constant) #f]
                [(enumeration-constant = constant-expression) #f])
    (enumerator-list [(enumerator) #f]
                     [(enumerator-list comma enumerator) #f])
    (struct-declaration-list [(struct-declaration) #f]
                             [(struct-declaration-list struct-declaration) #f])
    (struct-declarator-list [(struct-declarator) #f]
                            [(struct-declarator-list comma struct-declarator) #f])
    (struct-declarator [(declarator) #f]
                       [(declarator? : constant-expression) #f])


    ;; declarator
    (declarator [(        direct-declarator) #f]
                [(pointer direct-declarator) #f])
    (abstract-declarator [(pointer) #f]
                         [(direct-abstract-declarator) #f]
                         [(pointer direct-abstract-declarator) #f])
    (pointer [(*) #f]
             [(* type-qualifier-list) #f]
             [(* pointer) #f]
             [(* type-qualifier-list pointer) #f])
    (direct-abstract-declarator [(l-paren abstract-declarator r-paren) #f]
                                [(                           l-bracket                     assignment-expression? r-bracket) #f]
                                [(                           l-bracket type-qualifier-list assignment-expression? r-bracket) #f]
                                [(direct-abstract-declarator l-bracket                     assignment-expression? r-bracket) #f]
                                [(direct-abstract-declarator l-bracket type-qualifier-list assignment-expression? r-bracket) #f]
                                [(                           l-bracket static type-qualifier-list? assignment-expression r-bracket) #f]
                                [(direct-abstract-declarator l-bracket static type-qualifier-list? assignment-expression r-bracket) #f]
                                [(                           l-bracket type-qualifier-list static assignment-expression r-bracket) #f]
                                [(direct-abstract-declarator l-bracket type-qualifier-list static assignment-expression r-bracket) #f]
                                [(                           l-bracket * r-bracket) #f]
                                [(direct-abstract-declarator l-bracket * r-bracket) #f]
                                [(                           l-paren parameter-type-list? r-paren) #f]
                                [(direct-abstract-declarator l-paren parameter-type-list? r-paren) #f]
                                )
    (parameter-type-list [(parameter-list) #f]
                         [(parameter-list comma ellipsis) #f])
    (parameter-declaration [(declaration-specifiers declarator) #f]
                           [(declaration-specifiers abstract-declarator?) #f])
    (direct-declarator [(identifier) #f]
                       [(l-paren declarator r-paren) #f]
                       [(direct-declarator l-bracket type-qualifier-list? assignment-expression? r-bracket) #f]
                       [(direct-declarator l-bracket static type-qualifier-list? assignment-expression r-bracket) #f]
                       [(direct-declarator l-bracket type-qualifier-list static assignment-expression r-bracket) #f]
                       [(direct-declarator l-bracket type-qualifier-list? * r-bracket) #f]
                       [(direct-declarator l-paren parameter-type-list r-paren) #f]
                       [(direct-declarator l-paren identifier-list? r-paren) #f])
    ;; initializer
    (init-declarator-list? [() #f]
                           [(init-declarator-list) #f])
    (init-declarator-list [(init-declarator) #f]
                          [(init-declarator-list comma init-declarator) #f])
    (init-declarator [(declarator) #f]
                     [(declarator = initializer) #f])
    (initializer-list [(initializer) #f]
                      [(designation initializer) #f]
                      [(initializer-list comma initializer) #f]
                      [(initializer-list comma designation initializer) #f])
    (initializer [(assignment-expression) #f]
                 [(l-brace initializer-list r-brace) #f]
                 [(l-brace initializer-list comma r-brace) #f])
    (designation [(designator-list =) #f])
    (designator-list [(designator) #f]
                     [(designator-list designator) #f])
    (designator [(l-bracket constant-expression r-bracket) #f]
                [(period identifier) #f])


    ;; optional
    (declaration-specifiers? [() #f]
                             [(declaration-specifiers) #f])
    (abstract-declarator? [() #f]
                          [(abstract-declarator) #f])
    (direct-abstract-declarator? [() #f]
                                 [(direct-abstract-declarator) #f])
    (type-qualifier-list? [() #f]
                          [(type-qualifier-list) #f])
    (assignment-expression? [() #f]
                            [(assignment-expression) #f])
    (parameter-type-list? [() #f]
                          [(parameter-type-list) #f])
    (identifier? [() #f]
                 [(identifier) #f])
    (declarator? [() #f]
                 [(declarator) #f])
    (pointer? [() #f]
              [(pointer) #f])
    (identifier-list? [() #f]
                      [(identifier-list) #f])
    (parameter-list [(parameter-declaration) #f]
                    [(parameter-list comma parameter-declaration) #f])


    ;; ==============================
    ;; Statements
    ;; ==============================

    (statement [(labeled-statement) #f]
               [(compound-statement) #f]
               [(expression-statement) #f]
               [(selection-statement) #f]
               [(iteration-statement) #f]
               [(jump-statement) #f])

    ;; c.rkt has statement-tn, what is that??
    (labeled-statement [(identifier : statement) #f]
                       [(case constant-expression : statement) #f]
                       [(default : statement) #f])
    (compound-statement [(l-brace block-item-list? r-brace) #f])
    (block-item-list [(block-item) #f]
                     [(block-item-list block-item) #f])
    (block-item [(declaration) #f]
                [(statement) #f])
    (expression-statement [(expression? semi-colon) #f])
    (selection-statement [(if l-paren expression r-paren statement) #f]
                         [(if l-paren expression r-paren statement else statement) #f]
                         [(switch l-paren expression r-paren statement) #f])
    (iteration-statement [(while l-paren expression r-paren statement) #f]
                         [(do statement while l-paren statement r-paren) #f]
                         [(for l-paren expression? semi-colon expression? semi-colon expression? r-paren statement) #f]
                         ;; the first declaration eats the first semi-colon. May incur some trouble here
                         [(for l-paren declaration expression? semi-colon expression? r-paren statement) #f])
    (jump-statement [(goto identifier semi-colon) #f]
                    [(continue semi-colon) #f]
                    [(break semi-colon) #f]
                    [(return expression? semi-colon) #f])
    ;; optional
    (expression? [() #f]
                 [(expression) #f])
    (block-item-list? [() #f]
                      [(block-item-list) #f])
    )))
