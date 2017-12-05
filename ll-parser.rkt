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
  (let loop ([res (parse-statement-or-declaration ll)])
    (when res
      (loop (parse-statement-or-declaration ll)))))

(define (parse-statement-or-declaration ll)
  (match (list (token-name (ll 0)))
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



;; TODO TODO TODO
(define (parse-labeled-statement ll) #f)

(define (parse-case-statement ll) #f)
(define (parse-default-statement ll) #f)
(define (parse-compound-statement ll) #f)
(define (parse-null-statement ll) #f)
(define (parse-if-statement ll) #f)
(define (parse-switch-statement ll) #f)
(define (parse-while-statement ll) #f)
(define (parse-do-statement ll) #f)
(define (parse-for-statement ll) #f)
(define (parse-goto-statement ll) #f)
(define (parse-continue-statement ll) #f)
(define (parse-break-statement ll) #f)
(define (parse-return-statement ll) #f)



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

;; TODO TODO TODO
(define (parse-expression ll) #f)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Decl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; corresponding to parse-simple-declaration, the non-simple ones are
;; for C++
(define (parse-declaration ll)
  (parse-declaration-specifiers ll)
  (parse-decl-group))

(define (parse-declaration-specifiers ll)
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

