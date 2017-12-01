#lang racket

(require parser-tools/lex
         parser-tools/yacc
         "lexer.rkt")

(provide parse)

(define (parse program)
  '())


(parser
 (start translation-unit)
 (end EOF)
 (tokens Keywords Basic Special Punctuators)
 (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
          (raise-syntax-error "Error")))
 (grammar
  ;; trans unit & declaration
  (translation-unit [(external-declaration) #f]
                    [(translation-unit external-declaration) #f])
  (external-declaration [(function-definition) #f]
                        [(declaration) #f])
  (function-definition [(declaration-specifiers declarator declaration-list compound-statement) #f]
                       [(declaration-specifiers declarator compound-statement) #f])
  (declaration [(declaration-specifiers init-declarator-list) #f]
               [(declaration-specifiers) #f])
  (declaration-specifiers [(storage-class-specifier declaration-specifiers) #f]
                          [(storage-class-specifier) #f]
                          [(type-specifier declaration-specifiers) #f]
                          [(type-specifier) #f]
                          [(type-qualifier declaration-specifiers) #f]
                          [(type-qualifier) #f]
                          [(function-specifier declaration-specifiers) #f]
                          [(function-specifier) #f])
  (declarator [(pointer direct-declarator) #f]
              [(direct-declarator) #f])
  (declaration-list [(declaration) #f]
                    [(declaration-list declaration) #f])
  (init-declarator-list [(init-declarator) #f]
                        [(init-declarator-list COMMA init-declarator) #f])
  (direct-declarator [(identifier) #f]
                     [(L_BRACE declarator R_BRACE) #f]
                     [(direct-declarator L_BRACKET type-qualifier-list)])
  ;; specifier
  (storage-class-specifier [(typedef) #f]
                           [(extern) #f]
                           [(static) #f]
                           [(auto) #f]
                           [(register) #f])
  (type-specifier [(void) #f]
                  [(char) #f]
                  [(short) #f]
                  [(int) #f]
                  [(long) #f]
                  [(float) #f]
                  [(double) #f]
                  [(signed) #f]
                  [(unsigned) #f]
                  [(_Bool) #f]
                  [(_Complex) #f]
                  [(struct-or-union-specifier) #f]
                  [(enum-specifier) #f]
                  [(typedef-name) #f])
  (type-qualifier [(const) #f]
                  [(restrict) #f]
                  [(volatile) #f])
  (function-specifier [(inline) #f])
  ;; special
  (pointer [(* type-qualifier-list) #f]
           [(*) #f]
           [(* type-qualifier-list pointer) #f]
           [(* pointer) #f])
  ;; statements
  (compound-statement [(L_BRACE block-item-list R_BRACE)]
                      [(L_BRACE R_BRACE)])
  ))
