(define-values
  (prec-unkown
   prec-comma
   prec-assignment
   prec-conditional
   prec-logical-or
   prec-logical-and
   prec-inclusive-or
   prec-exclusive-or
   prec-and
   prec-equality
   prec-relational
   prec-shift
   prec-additive
   prec-multiplicative
   prec-pointer-to-member)
  (apply values (range 0 15)))

(define (get-bin-op-precedence tok)
  (case (token-name tok)
    ['COMMA prec-comma]
    [('= '*= '/= '%= '+= '-= '<<= '>>= '&= '^= 'or-assign) prec-assignment]
    ['? prec-conditional]
    ['or-op prec-logical-or]
    ['&& prec-logical-and]
    ['or prec-inclusive-or]
    ['^ prec-exclusive-or]
    ['& prec-and]
    [('!= '==) prec-equality]
    [('<= '< '>= '>) prec-relational]
    [('>> '<<) prec-shift]
    [('+ '-) prec-additive]
    [('% '/ '*) prec-multiplicative]
    [('PERIOD '->) prec-pointer-to-member]))

;; TODO in clang, this is actually either a cast, or a unary expr
;;
;; In clang, it seems to implement:
;; cast-expression
;; unary-expression
;; postfix-expression
;; primary-expression
;; Reason to do this:
;; 1. efficiency
;; 2. parenthesized expression
;;
;; it seems that, in the K&R grammar:
;; cast-expression ::= unary-expression
;; cast-expression ::= ( type-name ) cast-expression
;; while unary-expression has many form, e.g.
;; unary-expression ::= postfix-expression
;; FIXME it seems that this function takes a lot of UnConsume steps
(define (parse-cast-expression ll)
  (case (token-name (ll 0))
    ['l-paren (parse-paren-expression ll)]
    [('i-constant 'f-constant) (consume ll)]
    ;; primary-expression ::= identifier
    ;; unqualified-id ::= identifier
    ;; constant ::= enumeration-constant
    ;; I didn't find any code I can use
    ['identifier #f]
    ['string-literal (consume ll)]
    [('++ '--) (begin
                 (consume ll)
                 (parse-cast-expression ll))]
    [('& '* '+ '- '~ '!) (begin
                           (consume ll)
                           (parse-cast-expression ll))]
    ;; I think I can simplify this
    ['sizeof (parse-unary-expr-or-type-trait-expression ll)]
    ['&& (begin
           (consume ll)
           ;; WTF
           (consume ll))]
    [else (begin
            (parse-postfix-expression-suffix ll))]))

;; parse a binary expression, that has a precedence of at least
;; min-prec
(define (parse-rhs-of-binary-expression ll min-prec)
  (let ([next-prec (get-bin-op-precedence (ll 0))])
    (let loop ([dummy #f])
      (when (>= next-prec min-prec)
        ;; ternary operator
        (when (eq? next-prec prec-conditional)
          ;; middle
          (parse-expression ll))
        ;; rhs
        (parse-cast-expression ll)
        (let ([next-next-prec (get-bin-op-precedence (ll 1))])
          (when (or (< next-prec next-next-prec)
                    (and (= next-prec next-next-prec)
                         ;; next-prec is right associative
                         (or (eq? next-prec prec-conditional)
                             (eq? next-prec prec-assignment))))
            (parse-rhs-of-binary-expression)))
        (loop #f)))))


;; primary-expression ::= ( expression )
;; postfix-expression ::= ( type-name ) { initializer-list, }
;; cast-expression ::= ( type-name ) cast-expression
(define (parse-paren-expression ll)
  (ll 'consume)
  (if (eq? (token-name (ll 0)) 'l-brace)
      (parse-compound-statement ll)
      (if (type-id-in-parens? ll)
          (begin
            (parse-specifier-qualifier-list ll)
            (parse-declarator ll)
            ;; consume ')'
            (ll 'consume)
            (when (eq? (token-name (ll 0)) 'l-brace)
              ;; initializer
              (parse-compound-literal-expression ll))
            (parse-cast-expression ll))
          (begin
            (parse-expression ll))))
  (ll 'consume))

;; FIXME should be merged with parse-paren-expression
(define (parse-paren-expr-or-condition ll)
  (ll 'consume)
  (parse-expression ll)
  (ll 'consume))

;; we are at the left brace
;; postfix-expression ::= ( type-name ) { initializer-list }
;; postfix-expression ::= ( type-name ) { initializer-list , }
(define (parse-compound-literal-expression ll)
  (parse-initializer ll))
;; initializer ::= assignment-expression
;; | { ... }
(define (parse-initializer ll)
  (if (eq? (token-name (ll 0)) 'l-brace)
      (parse-brace-initializer ll)
      (parse-assignment-expression ll)))

;; initializer ::= { initializer-list }
;; initializer ::= { initializer-list , }
;; initializer ::= { }
(define (parse-brace-initializer ll)
  (ll 'consume)
  (let loop ([dummy #f])
    (parse-initializer ll)
    (when (try-consume ll 'comma)
      (loop #f)))
  (ll 'consume))

;; TODO
(define (type-id-in-parens? ll) #f)

;; This is in parsedecl.cpp
;; specifier-qualifier-list ::= type-specifier specifier-qualifier-list[opt]
;; | type-qualifier specifier-qualifier-list[opt
(define (parse-specifier-qualifier-list ll)
  ;; FIXME why this is the only one?
  (parse-declaration-specifiers ll))

;; I should rename it or remove it
;; unary-expression ::= sizeof unary-expression
;; | sizeof ( type-name )
(define (parse-unary-expr-or-type-trait-expression ll)
  (ll 'consume)
  (if (eq? (token-name (ll 0)) 'l-paren)
      (parse-paren-expression ll)
      (begin
        (parse-specifier-qualifier-list ll)
        (parse-declarator ll)
        (parse-cast-expression ll))))

;; [] () . -> ++ --
(define (parse-postfix-expression-suffix ll)
  (case (token-name (ll 0))
    ['l-bracket (begin
                  (ll 'consume)
                  (parse-expression ll)
                  (ll 'consume))]
    ['l-paren (begin
                (ll 'consume)
                (parse-expression-list ll)
                (ll 'consume))]
    [('-> 'period) (begin
                     ;; just consume??
                     (ll 'consume))]
    [('++ '--) (ll 'consume)]))

(define (parse-constant-expression ll)
  (parse-cast-expression ll)
  (parse-rhs-of-binary-expression ll))



;; argument-expression-list ::= argument-expression-list, argument-expression
(define (parse-expression-list ll)
  (let loop ([dummy #f])
    (parse-assignment-expression ll)
    (when (eq? (token-name (ll 0)) 'comma)
      (loop #f))))



