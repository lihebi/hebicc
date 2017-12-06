#lang racket

(require parser-tools/lex)
(require "lexer.rkt")

(define (ll-lexer lexer)
  (let ([cache '()])
    (lambda (k)
      (case k
        [(-1) (set! cache (cdr cache))]
        [(0) (begin
               (when (null? cache)
                 (set! cache (append cache (list (lexer)))))
               (car cache))]
        [(1) (begin
               (when (null? (cdr cache))
                 (set! cache (append cache (list (lexer)))))
               (cadr cache))]))))

(module+ test
  (let* ([lex (string-lexer "int main() {int a;}")]
         [ll-lex (ll-lexer lex)])
    (print (ll-lex 0))
    (print (ll-lex 0))
    (print (ll-lex 1))
    (ll-lex -1)
    (print (ll-lex 0))))

;; (define (parse-program str)
;;   (let ([lexer (string-lexer str)])
;;     (parse lexer)))


(module+ test
  (let* ([lex (string-lexer "int a;")]
         [ll (ll-lexer lex)])
    (parse-statement ll)))


;; TODO general checking EOF
;; TODO general consume token

(define (consume ll)
  (ll -1))

;; FIXME what to return??
(define (try-consume ll target)
  (when (eq? (ll 0) target)
    (ll -1)))

(define (skip-until ll target)
  (case (token-name (ll 0))
    [(target) (token-value (ll 0))]
    ['l-paren (skip-until ll 'r-paren)]
    ['l-bracket (skip-until ll 'r-bracket)]
    ['l-brace (skip-until ll 'r-brace)]
    ;; FIXME unbalanced parens
    ;; semicolon force return
    ['semi-colon #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-statement ll)
  (println "parse-statement")
  (let loop ()
    (when (parse-statement-or-declaration ll)
      #f
      #;(loop)
      )))

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
  (consume ll)
  (consume ll)
  (parse-statement ll))

;; this actually parse all nested case
(define (parse-case-statement ll)
  (let loop ([dummy #f])
    (when (eq? (token-name (ll 0)) 'case)
      (parse-constant-expression ll)
      (consume ll)
      (loop))
    (parse-statement ll)))

(define (parse-default-statement ll)
  (consume ll)
  (parse-statement ll))

(define (parse-compound-statement ll)
  ;; FIXME track the open and close brace
  (consume ll)
  (let loop ([dummy #f])
    (when (not (eq? (token-name (ll 0)) 'r-brace))
      (parse-statement-or-declaration ll))))

(define (parse-null-statement ll)
  (consume ll))

;; FIXME scope
(define (parse-if-statement ll)
  (consume ll)
  (parse-paren-expr-or-condition ll)
  (parse-statement ll)
  (when (eq? (token-name (ll 0)) 'else)
    (consume ll)
    (parse-statement ll)))
(define (parse-switch-statement ll)
  (consume ll)
  (parse-paren-expr-or-condition ll)
  (parse-statement ll))
(define (parse-while-statement ll)
  (consume ll)
  (parse-paren-expr-or-condition ll)
  (parse-statement ll))
(define (parse-do-statement ll)
  (consume ll)
  (parse-statement ll)
  ;; FIXME why use parse-expression here instead of parse-paren-expr-or-condition
  (parse-expression ll))
(define (parse-for-statement ll)
  (consume ll)
  ;; FIXME this might be a simple assignment statement
  ;; FIXME check emptyness
  (parse-declaration ll)
  (parse-expression ll)
  (parse-expression ll)
  ;; FIXME GENERAL when consuming, check if it is expected
  (consume ll)
  (parse-statement ll))
(define (parse-goto-statement ll)
  (consume ll)
  ;; this identifier should be returned
  ;; FIXME not an identifier?
  (when (eq? (token-name (ll 0)) 'identifier)
    (consume ll)))
(define (parse-continue-statement ll)
  (consume ll))
(define (parse-break-statement ll)
  (consume ll))
(define (parse-return-statement ll)
  (consume ll)
  (parse-expression ll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;; OK, I only care about the whole thing inside the expr, so I'm going
;; to use skip-until trick
(define (parse-expr-statement ll)
  ;; (parse-assignment-expression ll)
  ;; (parse-rhs-of-binary-expression ll prec-comma)
  (skip-until ll 'semi-colon))

;; (define (parse-assignment-expression ll) #f)
;; (define (parse-rhs-of-binary-expression ll prec) #f)

(define (parse-expression ll)
  (parse-assignment-expression ll)
  (parse-rhs-of-binary-expression ll prec-comma))
(define (parse-assignment-expression ll)
  (parse-cast-expression ll)
  (parse-rhs-of-binary-expression ll prec-assignment))

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

;; primary-expression ::= ( expression )
;; postfix-expression ::= ( type-name ) { initializer-list, }
;; cast-expression ::= ( type-name ) cast-expression
(define (parse-paren-expression ll)
  (consume ll)
  (if (eq? (token-name (ll 0)) 'l-brace)
      (parse-compound-statement ll)
      (if (type-id-in-parens? ll)
          (begin
            (parse-specifier-qualifier-list ll)
            (parse-declarator ll)
            ;; consume ')'
            (consume ll)
            (when (eq? (token-name (ll 0)) 'l-brace)
              ;; initializer
              (parse-compound-literal-expression ll))
            (parse-cast-expression ll))
          (begin
            (parse-expression ll))))
  (consume ll))


;; FIXME should be merged with parse-paren-expression
(define (parse-paren-expr-or-condition ll)
  (consume ll)
  (parse-expression ll)
  (consume ll))

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
  (consume ll)
  (let loop ([dummy #f])
    (parse-initializer ll)
    (when (try-consume ll 'comma)
      (loop #f)))
  (consume ll))

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
  (consume ll)
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
                  (consume ll)
                  (parse-expression ll)
                  (consume ll))]
    ['l-paren (begin
                (consume ll)
                (parse-expression-list ll)
                (consume ll))]
    [('-> 'period) (begin
                     ;; just consume??
                     (consume ll))]
    [('++ '--) (consume ll)]))

(define (parse-constant-expression ll)
  (parse-cast-expression ll)
  (parse-rhs-of-binary-expression ll))



;; argument-expression-list ::= argument-expression-list, argument-expression
(define (parse-expression-list ll)
  (let loop ([dummy #f])
    (parse-assignment-expression ll)
    (when (eq? (token-name (ll 0)) 'comma)
      (loop #f))))













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

