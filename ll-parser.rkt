#lang racket

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

(define (parse-labeled-statement ll)
  #f)
(define (parse-expr-statement ll)
  #f)
(define (parse-declaration ll)
  #f)

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
