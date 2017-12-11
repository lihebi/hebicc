#lang racket

(require "token.rkt")
(require "lexer.rkt")
(require parser-tools/lex
         parser-tools/yacc)

(define (lr-parser)
  (parser
   (start TranslationUnit)
   (end eof)
   (src-pos)
   (tokens t et)
   (error (lambda (token-ok? token-name token-value start-pos end-pos)
            (raise-syntax-error #f "Parser Error")))
   (grammar
    (TODO [() #f])
    (TranslationUnit [(TODO) #f])
    (Identifier [(identifier) #f])

    ;; expressions
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
    ;; declarations
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
    (type-name [(specifier-qualifier-list) #f]
               [(specifier-qualifier-list abstract-declarator) #f])
    )))
