#lang racket

(require parser-tools/lex
         parser-tools/yacc
         "lexer.rkt"
         "token.rkt")

(define (the-parser)
  (parser
   (start expression)
   (end eof)
   (error (lambda (token-ok? token-name token-value start-pos end-pos)
            (raise-syntax-error #f "Parser Error")))
   (tokens t et)
   (grammar
    (primary-expression
     ((identifier) #f)
     ((constant) #f)
     ((string) #f)
     ((l-paren expression r-paren) #f)
     ((generic-selection) #f))
    (constant ((i-constant) #f) ((f-constant) #f) ((enumeration-constant) #f))
    ;; (enumeration-constant ((identifier) #f))
    (string ((string-literal) #f) ((func-name) #f))
    (generic-selection
     ((generic l-paren assignment-expression comma generic-assoc-list r-paren) #f))
    (generic-assoc-list
     ((generic-association) #f)
     ((generic-assoc-list comma generic-association) #f))
    (generic-association
     ((type-name : assignment-expression) #f)
     ((default : assignment-expression) #f))
    (postfix-expression
     ((primary-expression) #f)
     ((postfix-expression l-bracket expression r-bracket) #f)
     ((postfix-expression l-paren r-paren) #f)
     ((postfix-expression l-paren argument-expression-list r-paren) #f)
     ((postfix-expression period identifier) #f)
     ((postfix-expression * identifier) #f)
     ((postfix-expression ++) #f)
     ((postfix-expression --) #f)
     ((l-paren type-name r-paren l-brace initializer-list r-brace) #f)
     ((l-paren type-name r-paren l-brace initializer-list comma r-brace) #f))
    (argument-expression-list
     ((assignment-expression) #f)
     ((argument-expression-list comma assignment-expression) #f))
    (unary-expression
     ((postfix-expression) #f)
     ((++ unary-expression) #f)
     ((-- unary-expression) #f)
     ((unary-operator cast-expression) #f)
     ((sizeof unary-expression) #f)
     ((sizeof l-paren type-name r-paren) #f)
     ((alignof l-paren type-name r-paren) #f))
    (unary-operator ((&) #f) ((*) #f) ((+) #f) ((-) #f) ((~) #f) ((!) #f))
    (cast-expression
     ((unary-expression) #f)
     ((l-paren type-name r-paren cast-expression) #f))
    (multiplicative-expression
     ((cast-expression) #f)
     ((multiplicative-expression * cast-expression) #f)
     ((multiplicative-expression / cast-expression) #f)
     ((multiplicative-expression % cast-expression) #f))
    (additive-expression
     ((multiplicative-expression) #f)
     ((additive-expression + multiplicative-expression) #f)
     ((additive-expression - multiplicative-expression) #f))
    (shift-expression
     ((additive-expression) #f)
     ((shift-expression << additive-expression) #f)
     ((shift-expression >> additive-expression) #f))
    (relational-expression
     ((shift-expression) #f)
     ((relational-expression < shift-expression) #f)
     ((relational-expression > shift-expression) #f)
     ((relational-expression < shift-expression) #f)
     ((relational-expression > shift-expression) #f))
    (equality-expression
     ((relational-expression) #f)
     ((equality-expression = relational-expression) #f)
     ((equality-expression != relational-expression) #f))
    (and-expression
     ((equality-expression) #f)
     ((and-expression & equality-expression) #f))
    (exclusive-or-expression
     ((and-expression) #f)
     ((exclusive-or-expression ^ and-expression) #f))
    (inclusive-or-expression
     ((exclusive-or-expression) #f)
     ((inclusive-or-expression or exclusive-or-expression) #f))
    (logical-and-expression
     ((inclusive-or-expression) #f)
     ((logical-and-expression && inclusive-or-expression) #f))
    (logical-or-expression
     ((logical-and-expression) #f)
     ((logical-or-expression or-op logical-and-expression) #f))
    (conditional-expression
     ((logical-or-expression) #f)
     ((logical-or-expression ? expression : conditional-expression) #f))
    (assignment-expression
     ((conditional-expression) #f)
     ((unary-expression assignment-operator assignment-expression) #f))
    (assignment-operator
     ((=) #f)
     ((*=) #f)
     ((/=) #f)
     ((%=) #f)
     ((+=) #f)
     ((-=) #f)
     ((<<=) #f)
     ((>>=) #f)
     ((&=) #f)
     ((^=) #f)
     ((or-assign) #f))
    (expression
     ((assignment-expression) #f)
     ((expression comma assignment-expression) #f))
    (constant-expression ((conditional-expression) #f))
    (declaration
     ((declaration-specifiers semi-colon) #f)
     ((declaration-specifiers init-declarator-list semi-colon) #f)
     ((static-assert-declaration) #f))
    (declaration-specifiers
     ((storage-class-specifier declaration-specifiers) #f)
     ((storage-class-specifier) #f)
     ((type-specifier declaration-specifiers) #f)
     ((type-specifier) #f)
     ((type-qualifier declaration-specifiers) #f)
     ((type-qualifier) #f)
     ((function-specifier declaration-specifiers) #f)
     ((function-specifier) #f)
     ((alignment-specifier declaration-specifiers) #f)
     ((alignment-specifier) #f))
    (init-declarator-list
     ((init-declarator) #f)
     ((init-declarator-list comma init-declarator) #f))
    (init-declarator ((declarator = initializer) #f) ((declarator) #f))
    (storage-class-specifier
     ((typedef) #f)
     ((extern) #f)
     ((static) #f)
     ((thread-local) #f)
     ((auto) #f)
     ((register) #f))
    (type-specifier
     ((void) #f)
     ((char) #f)
     ((short) #f)
     ((int) #f)
     ((long) #f)
     ((float) #f)
     ((double) #f)
     ((signed) #f)
     ((unsigned) #f)
     ((bool) #f)
     ((complex) #f)
     ((imaginary) #f)
     ((atomic-type-specifier) #f)
     ((struct-or-union-specifier) #f)
     ((enum-specifier) #f)
     ((typedef-name) #f))
    (struct-or-union-specifier
     ((struct-or-union l-brace struct-declaration-list r-brace) #f)
     ((struct-or-union identifier l-brace struct-declaration-list r-brace) #f)
     ((struct-or-union identifier) #f))
    (struct-or-union ((struct) #f) ((union) #f))
    (struct-declaration-list
     ((struct-declaration) #f)
     ((struct-declaration-list struct-declaration) #f))
    (struct-declaration
     ((specifier-qualifier-list semi-colon) #f)
     ((specifier-qualifier-list struct-declarator-list semi-colon) #f)
     ((static-assert-declaration) #f))
    (specifier-qualifier-list
     ((type-specifier specifier-qualifier-list) #f)
     ((type-specifier) #f)
     ((type-qualifier specifier-qualifier-list) #f)
     ((type-qualifier) #f))
    (struct-declarator-list
     ((struct-declarator) #f)
     ((struct-declarator-list comma struct-declarator) #f))
    (struct-declarator
     ((: constant-expression) #f)
     ((declarator : constant-expression) #f)
     ((declarator) #f))
    (enum-specifier
     ((enum l-brace enumerator-list r-brace) #f)
     ((enum l-brace enumerator-list comma r-brace) #f)
     ((enum identifier l-brace enumerator-list r-brace) #f)
     ((enum identifier l-brace enumerator-list comma r-brace) #f)
     ((enum identifier) #f))
    (enumerator-list ((enumerator) #f) ((enumerator-list comma enumerator) #f))
    (enumerator
     ((enumeration-constant = constant-expression) #f)
     ((enumeration-constant) #f))
    (atomic-type-specifier ((atomic l-paren type-name r-paren) #f))
    (type-qualifier
     ((const) #f)
     ((restrict) #f)
     ((volatile) #f)
     ((atomic) #f))
    (function-specifier ((inline) #f) ((noreturn) #f))
    (alignment-specifier
     ((alignas l-paren type-name r-paren) #f)
     ((alignas l-paren constant-expression r-paren) #f))
    (declarator ((pointer direct-declarator) #f) ((direct-declarator) #f))
    (direct-declarator
     ((identifier) #f)
     ((l-paren declarator r-paren) #f)
     ((direct-declarator l-bracket r-bracket) #f)
     ((direct-declarator l-bracket * r-bracket) #f)
     ((direct-declarator
       l-bracket
       static
       type-qualifier-list
       assignment-expression
       r-bracket)
      #f)
     ((direct-declarator l-bracket static assignment-expression r-bracket) #f)
     ((direct-declarator l-bracket type-qualifier-list * r-bracket) #f)
     ((direct-declarator
       l-bracket
       type-qualifier-list
       static
       assignment-expression
       r-bracket)
      #f)
     ((direct-declarator l-bracket type-qualifier-list assignment-expression r-bracket)
      #f)
     ((direct-declarator l-bracket type-qualifier-list r-bracket) #f)
     ((direct-declarator l-bracket assignment-expression r-bracket) #f)
     ((direct-declarator l-paren parameter-type-list r-paren) #f)
     ((direct-declarator l-paren r-paren) #f)
     ((direct-declarator l-paren identifier-list r-paren) #f))
    (pointer
     ((* type-qualifier-list pointer) #f)
     ((* type-qualifier-list) #f)
     ((* pointer) #f)
     ((*) #f))
    (type-qualifier-list
     ((type-qualifier) #f)
     ((type-qualifier-list type-qualifier) #f))
    (parameter-type-list
     ((parameter-list comma ellipsis) #f)
     ((parameter-list) #f))
    (parameter-list
     ((parameter-declaration) #f)
     ((parameter-list comma parameter-declaration) #f))
    (parameter-declaration
     ((declaration-specifiers declarator) #f)
     ((declaration-specifiers abstract-declarator) #f)
     ((declaration-specifiers) #f))
    (identifier-list ((identifier) #f) ((identifier-list comma identifier) #f))
    (type-name
     ((specifier-qualifier-list abstract-declarator) #f)
     ((specifier-qualifier-list) #f))
    (abstract-declarator
     ((pointer direct-abstract-declarator) #f)
     ((pointer) #f)
     ((direct-abstract-declarator) #f))
    (direct-abstract-declarator
     ((l-paren abstract-declarator r-paren) #f)
     ((l-bracket r-bracket) #f)
     ((l-bracket * r-bracket) #f)
     ((l-bracket static type-qualifier-list assignment-expression r-bracket) #f)
     ((l-bracket static assignment-expression r-bracket) #f)
     ((l-bracket type-qualifier-list static assignment-expression r-bracket) #f)
     ((l-bracket type-qualifier-list assignment-expression r-bracket) #f)
     ((l-bracket type-qualifier-list r-bracket) #f)
     ((l-bracket assignment-expression r-bracket) #f)
     ((direct-abstract-declarator l-bracket r-bracket) #f)
     ((direct-abstract-declarator l-bracket * r-bracket) #f)
     ((direct-abstract-declarator
       l-bracket
       static
       type-qualifier-list
       assignment-expression
       r-bracket)
      #f)
     ((direct-abstract-declarator l-bracket static assignment-expression r-bracket) #f)
     ((direct-abstract-declarator
       l-bracket
       type-qualifier-list
       assignment-expression
       r-bracket)
      #f)
     ((direct-abstract-declarator
       l-bracket
       type-qualifier-list
       static
       assignment-expression
       r-bracket)
      #f)
     ((direct-abstract-declarator l-bracket type-qualifier-list r-bracket) #f)
     ((direct-abstract-declarator l-bracket assignment-expression r-bracket) #f)
     ((l-paren r-paren) #f)
     ((l-paren parameter-type-list r-paren) #f)
     ((direct-abstract-declarator l-paren r-paren) #f)
     ((direct-abstract-declarator l-paren parameter-type-list r-paren) #f))
    (initializer
     ((l-brace initializer-list r-brace) #f)
     ((l-brace initializer-list comma r-brace) #f)
     ((assignment-expression) #f))
    (initializer-list
     ((designation initializer) #f)
     ((initializer) #f)
     ((initializer-list comma designation initializer) #f)
     ((initializer-list comma initializer) #f))
    (designation ((designator-list =) #f))
    (designator-list ((designator) #f) ((designator-list designator) #f))
    (designator ((l-bracket constant-expression r-bracket) #f) ((period identifier) #f))
    (static-assert-declaration
     ((static-assert l-paren constant-expression comma string-literal r-paren semi-colon) #f))
    (statement
     ((labeled-statement) #f)
     ((compound-statement) #f)
     ((expression-statement) #f)
     ((selection-statement) #f)
     ((iteration-statement) #f)
     ((jump-statement) #f))
    (labeled-statement
     ((identifier : statement) #f)
     ((case constant-expression : statement) #f)
     ((default : statement) #f))
    (compound-statement ((l-brace r-brace) #f) ((l-brace block-item-list r-brace) #f))
    (block-item-list ((block-item) #f) ((block-item-list block-item) #f))
    (block-item ((declaration) #f) ((statement) #f))
    (expression-statement ((semi-colon) #f) ((expression semi-colon) #f))
    (selection-statement
     ((if l-paren expression r-paren statement else statement) #f)
     ((if l-paren expression r-paren statement) #f)
     ((switch l-paren expression r-paren statement) #f))
    (iteration-statement
     ((while l-paren expression r-paren statement) #f)
     ((do statement while l-paren expression r-paren semi-colon) #f)
     ((for l-paren expression-statement expression-statement r-paren statement) #f)
     ((for
          l-paren
        expression-statement
        expression-statement
        expression
        r-paren
        statement)
      #f)
     ((for l-paren declaration expression-statement r-paren statement) #f)
     ((for l-paren declaration expression-statement expression r-paren statement) #f))
    (jump-statement
     ((goto identifier semi-colon) #f)
     ((continue semi-colon) #f)
     ((break semi-colon) #f)
     ((return semi-colon) #f)
     ((return expression semi-colon) #f))
    (translation-unit
     ((external-declaration) #f)
     ((translation-unit external-declaration) #f))
    (external-declaration ((function-definition) #f) ((declaration) #f))
    (function-definition
     ((declaration-specifiers declarator declaration-list compound-statement)
      #f)
     ((declaration-specifiers declarator compound-statement) #f))
    (declaration-list
     ((declaration) #f)
     ((declaration-list declaration) #f)))))


#;
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
