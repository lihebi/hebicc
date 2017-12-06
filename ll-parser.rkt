#lang racket

(require parser-tools/lex)
(require "lexer.rkt")



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
              (set! res (car (port-cache)))
              (set! port-cache (append port-cache (list (lexer))))
              (set! port-cache (cdr port-cache))))
        res))
    (lambda (k)
      (case k
        [(consume) (let ([res (consume)])
                    (when is-backtracking?
                      (set! back (append back res))))]
        [(track) (begin
                  (set! is-backtracking? #t)
                  (when (not (null? back))
                    (set! back-stack (append back-stack back))
                    (set! back '())))]
        [(pop) (set! back-stack (drop-right back-stack 1))]
        [(restore) (begin
                    (set! active-back (append (take-right back-stack 1)
                                              active-back)))]
        [(0) (if (not (null? active-back))
               (car active-back)
               (car port-cache))]
        [(1) (begin
             (if (not (null? active-back))
                 (if (not (null? (cdr active-back)))
                     (cadr active-back)
                     (car port-cache))
                 (cadr port-cache)))]
        [else (raise-syntax-error "Invalid operation for ll-lexer")]))))

((lambda (x)
   (case x
     [(''a 8) 'a]
     ['5 '5])) 'a)


(module+ test
  (let* ([lex (string-lexer "int main() {int a;}")]
         [ll-lex (ll-lexer lex)])
    (print (ll-lex 0))
    (print (ll-lex 0))
    (print (ll-lex 1))
    (ll-lex 'consume)
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

;; FIXME what to return??
(define (try-consume ll target)
  (when (eq? (ll 0) target)
    (ll 'consume)))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-constant-expression ll)
  (parse-conditional-expression ll))

(define (parse-expression ll)
  (parse-assignment-expression ll)
  (when (try-consume ll 'comma)
    (parse-expression ll)))

(define (parse-assignment-expression ll)
  (parse-conditional-expression ll)
  (case (token-name (ll 0))
    [('= '*= '/= '%= '+= '-= '<<= '>>= '&= '^= 'or-assign)
     (begin
       (ll 'consume)
       (parse-assignment-expression ll))]
    [else #f]))

(define (parse-conditional-expression ll)
  (parse-logical-or-expression ll)
  (when (eq? (token-name (ll 0)) '?)
    (ll 'consume)
    (parse-expression ll)
    (ll 'consume)
    (parse-conditional-expression ll)))
(define (parse-logical-or-expression ll)
  (parse-logical-and-expression ll)
  (when (eq? (token-name (ll 0)) 'or-op)
    (begin
      (ll 'consume)
      (parse-logical-or-expression ll))))
(define (parse-logical-and-expression ll)
  (parse-inclusive-or-expression ll)
  (when (eq? (token-name (ll 0)) '&&)
    (ll 'consume)
    (parse-logical-and-expression ll)))
(define (parse-inclusive-or-expression ll)
  (parse-exclusive-or-expression ll)
  (when (eq? (token-name (ll 0)) 'or)
    (ll 'consume)
    (parse-inclusive-or-expression ll)))
(define (parse-exclusive-or-expression ll)
  (parse-and-expression ll)
  (when (eq? (token-name (ll 0)) '^)
    (ll 'consume)
    (parse-exclusive-or-expression ll)))
(define (parse-and-expression ll)
  (parse-equality-expression ll)
  (when (eq? (token-name (ll 0)) '&)
    (ll 'consume)
    (parse-and-expression ll)))
(define (parse-equality-expression ll)
  (parse-relational-expression ll)
  (when (or (eq? (token-name (ll 0)) '==)
            (eq? (token-name (ll 0)) '!=))
    (ll 'consume)
    (parse-equality-expression ll)))
(define (parse-relational-expression ll)
  (parse-shift-expression ll)
  (when (member (token-name (ll 0)) '(< > <= >=))
    (ll 'consume)
    (parse-relational-expression ll)))
(define (parse-shift-expression ll)
  (parse-additive-expression ll)
  (when (member (token-name (ll 0)) '(<< >>))
    (ll 'consume)
    (parse-shift-expression ll)))
(define (parse-additive-expression ll)
  (parse-multiplicative-expression ll)
  (when (member (token-name (ll 0)) '(+ -))
    (ll 'consume)
    (parse-additive-expression ll)))
(define (parse-multiplicative-expression ll)
  (parse-cast-expression ll)
  (when (member (token-name (ll 0)) '(* / %))
    (ll 'consume)
    (parse-multiplicative-expression ll)))
(define (parse-cast-expression ll)
  (ll 'track)
  (if (parse-unary-expression ll)
      (ll 'pop)
      (begin
        (ll 'restore)
        (when (eq? (token-name (ll 0)) 'l-paren)
          (ll 'consume)
          (skip-until ll 'r-paren)
          (parse-cast-expression ll)))))

(define (parse-unary-expression ll)
  (case (token-name (ll 0))
    [(++ --) (begin
               (ll 'consume)
               (parse-unary-expression ll))]
    [(sizeof) (begin
                (ll 'consume)
                (ll 'track)
                (if (parse-unary-expression ll)
                    (ll 'pop)
                    (begin
                      (ll 'restore)
                      (when (eq? (token-name (ll 0)) 'l-paren)
                        (ll 'consume)
                        (skip-until ll 'r-paren)))))]
    [(& * + - ~ !) (begin
                     (ll 'consume)
                     (parse-cast-expression ll))]
    [else (parse-postfix-expression ll)]))

;; TODO (typename) {initializer-list}
(define (parse-postfix-expression ll)
  (parse-primary-expression ll)
  (case (token-name (ll 0))
    [(l-bracket) (begin
                   (ll 'consume)
                   (parse-expression ll)
                   (ll 'consume))]
    [(l-paren) (begin
                 (ll 'consume)
                 (skip-until ll 'r-paren)
                 (ll 'consume))]
    [(period ->) (begin
                   (ll 'consume)
                   (ll 'consume))]
    [(++ --) (ll 'consume)]))

(define (parse-primary-expression ll)
  (case (token-name (ll 0))
    [(identifier i-constant f-constant string-literal) (ll 'consume)]
    [(l-paren) (begin
                 (ll 'consume)
                 (parse-expression ll)
                 (ll 'consume))]))





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

