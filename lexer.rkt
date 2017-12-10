#lang racket

(require parser-tools/lex)
(require "token.rkt")
(require (prefix-in : parser-tools/lex-sre))

(provide string-lexer file-lexer get-lexer get-token-name get-token-value)

(define-lex-abbrevs
  (BlockComment (:: "/*"
                    (complement (:: any-string "*/" any-string))
                    "*/"))
  (LineComment (:: "//" (:* (:~ (:or #\return #\linefeed)))))
  (Comment (:or BlockComment
                LineComment))
  (CondCompile (:: "#if"
                   (complement (:: any-string "#endif" any-string))
                   "#endif"))
  (NewLine (:or #\return #\linefeed
                (:: #\return #\linefeed)))
  (ControlLine (:: "#" (:* (:~ (:or #\return #\linefeed)))))
  (O (:or (:/ "0" "7")))
  (D (:or (:/ "0" "9")))
  (NZ (:or (:/ "1" "9")))
  (L (:or (:/ "A" "Z")
          (:/ "a" "z")
          "_"))
  (A (:or L D))
  (H (:or (:/ "a" "f")
          (:/ "A" "F")
          D))
  (HP (:: "0" (:or "x" "X")))
  (E (:: (:or "E" "e") (:? (:or "+" "-")) (:+ D)))
  (P (:: (:or "P" "p") (:? (:or "+" "-")) (:+ D)))
  (FS (:or "f" "F" "l" "L"))

  (UU (:or "u" "U"))
  (LLLL (:or "l" "L" "ll" "LL"))
  (IS (:or (:: UU (:? LLLL))
           (:: LLLL (:? UU))))
  
  (WS (:or #\space #\tab #\page #\return #\linefeed))
  (CP (:or "u" "U" "L"))
  (SP (:or "u8" "u" "U" "L"))
  (CChar (:or (:~ #\') ES))
  (SChar (:or (:~ #\") ES))
  ;; TODO octal and hex escape
  (ES (:or "\\a" "\\b" "\\f" "\\n" "\\r" "\\t" "\\v"
           "\\'" "\\\"" "\\?" "\\\\")))

(define get-lexer
  (lexer-src-pos
   [Comment (return-without-pos (get-lexer input-port))]
   [(:+ WS) (return-without-pos
             (get-lexer input-port))]
   [CondCompile (return-without-pos
                 (get-lexer input-port))]
   [ControlLine (return-without-pos
                 (get-lexer input-port))]
   ["auto" (token-auto "auto")]
   ["break" (token-break "break")]
   ["case" (token-case "case")]
   ["char" (token-char "char")]

   ["const" (token-const "const")]
   ["continue" (token-continue "continue")]
   ["default" (token-default "default")]
   ["do" (token-do "do")]
   ["double" (token-double "double")]
   ["else" (token-else "else")]
   ["enum" (token-enum "enum")]
   ["extern" (token-extern "extern")]
   ["float" (token-float "float")]
   ["for" (token-for "for")]
   ["goto" (token-goto "goto")]
   ["if" (token-if "if")]
   ["inline" (token-inline "inline")]
   ["int" (token-int "int")]
   ["long" (token-long "long")]
   ["register" (token-register "register")]
   ["restrict" (token-restrict "restrict")]
   ["return" (token-return "return")]
   ["short" (token-short "short")]
   ["signed" (token-signed "signed")]
   ["sizeof" (token-sizeof "sizeof")]
   ["static" (token-static "static")]
   ["struct" (token-struct "struct")]
   ["switch" (token-switch "switch")]
   ["typedef" (token-typedef "typedef")]
   ["union" (token-union "union")]
   ["unsigned" (token-unsigned "unsigned")]
   ["void" (token-void "void")]
   ["volatile" (token-volatile "volatile")]
   ["while" (token-while "while")]
   ["_Alignas" (token-alignas "_Alignas")]
   ["_Alignof" (token-alignof "_Alignof")]
   ["_Atomic" (token-atomic "_Atomic")]
   ["_Bool" (token-bool "_Bool")]
   ["_Complex" (token-complex "_Complex")]
   ["_Generic" (token-generic "_Generic")]
   ["_Imaginary" (token-imaginary "_Imaginary")]
   ["_Noreturn" (token-noreturn "_Noreturn")]
   ["_Static_assert" (token-static-assert "_Static_assert")]
   ["_Thread_local" (token-thread-local "_Thread_local")]
   ["__func__" (token-func-name "__func__")]

   ;; TODO check-type
   [(:: L (:* A)) #;(check-type lexeme)
                  (token-identifier lexeme)]
   
   [(:: HP (:+ H) (:? IS)) (token-i-constant lexeme)]
   [(:: NZ (:* D) (:? IS)) (token-i-constant lexeme)]
   [(:: "0" (:* O) (:? IS)) (token-i-constant lexeme)]
   [(:: (:? CP) #\' (:+ CChar) #\') (token-i-constant lexeme)]

   [(:: (:+ D) E (:? FS)) (token-f-constant lexeme)]
   [(:: (:* D) "." (:+ D) (:? E) (:? FS)) (token-f-constant lexeme)]
   [(:: (:+ D) "." (:? E) (:? FS)) (token-f-constant lexeme)]
   [(:: HP (:+ H) P (:? FS)) (token-f-constant lexeme)]
   [(:: HP (:* H) "." (:+ H) P (:? FS)) (token-f-constant lexeme)]
   [(:: HP (:+ H) "." P (:? FS)) (token-f-constant lexeme)]

   [(:: (:? SP) #\" (:* SChar) #\") (token-string-literal lexeme)]

   ["..." (token-ellipsis "...")]
   [">>=" (token->>= ">>=")]
   ["<<=" (token-<<= "<<=")]
   ["+=" (token-+= lexeme)]
   ["-=" (token--= lexeme)]
   ["*=" (token-*= lexeme)]
   ["/=" (token-/= lexeme)]
   ["%=" (token-%= lexeme)]
   ["&=" (token-&= lexeme)]
   ["^=" (token-^= lexeme)]
   ["|=" (token-or-assign lexeme)]
   [">>" (token->> lexeme)]
   ["<<" (token-<< lexeme)]
   ["++" (token-++ lexeme)]
   ["--" (token--- lexeme)]
   ["->" (token--> lexeme)]
   ["&&" (token-&& lexeme)]
   ["||" (token-or-op lexeme)]
   ["<=" (token-<= lexeme)]
   [">=" (token->= lexeme)]
   ["==" (token-== lexeme)]
   ["!=" (token-!= lexeme)]

   ;; empty tokens
   [";" (token-semi-colon lexeme)]
   [(:or "{" "<%") (token-l-brace "{")]
   [(:or "}" "%>") (token-r-brace "}")]
   ["," (token-comma lexeme)]
   [":" (token-: lexeme)]
   ["=" (token-= lexeme)]
   ["(" (token-l-paren lexeme)]
   [")" (token-r-paren lexeme)]
   [(:or "[" "<:") (token-l-bracket "[")]
   [(:or "]" ":>") (token-r-bracket "]")]
   ["." (token-period lexeme)]
   ["&" (token-& lexeme)]
   ["!" (token-! lexeme)]
   ["~" (token-~ lexeme)]
   ["-" (token-- lexeme)]
   ["+" (token-+ lexeme)]
   ["*" (token-* lexeme)]
   ["/" (token-/ lexeme)]
   ["%" (token-% lexeme)]
   ["<" (token-< lexeme)]
   [">" (token-> lexeme)]
   ["^" (token-^ lexeme)]
   ["|" (token-or lexeme)]
   ["?" (token-? lexeme)]

   ;; [any-string 'bad]                ; discard bad characters
   ))

(define (string-lexer str)
  (let ([in (open-input-string str)])
    (port-count-lines! in)
    (lambda () (get-lexer in))))

(define (file-lexer f)
  (let ([in (open-input-file f)])
    (port-count-lines! in)
    (lambda () (get-lexer in))))

(define (lexer->list lex)
  (for*/list ([i (in-naturals)]
              [tok (list (lex))]
              #:break (eq? (position-token-token tok) 'eof))
    tok))

(define (get-token-name tok)
  (token-name (position-token-token tok)))
(define (get-token-value tok)
  (token-value (position-token-token tok)))

(module+ test
  (lexer->list (string-lexer "int main(char *c) {return 0;}"))
  (let ([str "
#include <\"hello.h\">\n
int a;
#include <hfds>
#if 0
int a; fjids fjald ndd
#elseif
fjdsl dsjf sdj 
#endif
int a;
"])
    (let ([lex (string-lexer str)])
      (lexer->list lex)))
  (length
   (lexer->list
    (file-lexer
     "/home/hebi/github/benchmark/download/findutils/find/parser.c"))))
