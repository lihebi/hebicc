#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(provide t et string-lexer get-lexer)

(define-tokens
  t
  (
   identifier i-constant f-constant string-literal func-name sizeof
   -> ++ -- << >> <= >= == !=
   && or-op *= /= %= +=
   -= <<= >>= &=
   ^= or-assign
   typedef-name enumeration-constant

   typedef extern static auto register inline
   const restrict volatile
   bool char short int long signed unsigned float double void
   complex imaginary 
   struct union enum ellipsis              

   case default if else switch while do for goto continue break return

   alignas alignof atomic generic noreturn static-assert thread-local

   ! % & l-paren r-paren * + comma - period / : semi-colon < = > ? l-bracket r-bracket ^ l-brace or r-brace ~
   ))

(define-empty-tokens
  et
  ())

;; TODO
(define (comment in)
  #f)

(define-lex-abbrevs
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
  (CChar (:or (complement #\') ES))
  (SChar (:or (complement #\") ES))
  ;; TODO octal and hex escape
  (ES (:or "\\a" "\\b" "\\f" "\\n" "\\r" "\\t" "\\v"
           "\\'" "\\\"" "\\?" "\\\\")))

(define get-lexer
  ;; TODO comment
  (lexer ["/*" (comment input-port)]
         [(:: "//" (:* (complement (:or #\return #\linefeed)))) #f]
         
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

         [(:+ WS) (get-lexer input-port)]                   ; remove whitespace
         ;; [any-string 'bad]                ; discard bad characters
         ))

(define (string-lexer str)
  (let* ([in (open-input-string str)])
    (lambda () (get-lexer in))))

(module+ test
  (let ([mylex (string-lexer "int main(char *c) {return 0;}")])
    (let loop ([tok (mylex)])
      (when (not (eq? tok 'eof))
        (print tok)
        (loop (mylex))))))

#;
(define-lex-abbrevs
  ;; (LineTerminator (:or #\return #\linefeed
  ;;                      (:: #\return #\linefeed)))
  (BlockComment (:: "/*" (complement (:: any-string "*/" any-string)) "*/"))
  (LineComment (:: "//" (:* (complement (:or #\return #\linefeed)))))
  (Comment (:or BlockComment
                LineComment))
  ;; Identifier
  (Identifier (:: Letter (:* LetterOrDigit)))
  (Letter (:or (:/ "A" "Z")
               (:/ "a" "z")
               "_"))
  (Digit (:or (:/ "0" "9")))
  (NonZeroDigit (:/ "1" "9"))
  (LetterOrDigit (:or Letter Digit))
  ;; Integer
  (IntegerSuffix (:* (:or "u" "U" "l" "L")))
  (IntegerConstant (:: (:or DecimalConstant HexadecimalConstant)
                       (:? IntegerSuffix)))
  ;; Allow arbitrary leading 0s, this will support Octal numbers, but
  ;; also allow invalid numbers
  (DecimalConstant (:+ Digit))
  (HexadecimalDigit (:or (:/ "a" "f")
                         (:/ "A" "F")
                         (:/ "0" "9")))
  (HexadecimalConstant (:: "0" (:or "x" "X")
                           (:+ HexadecimalDigit)))
  ;; Float
  (FloatSuffix (:or "f" "F" "l" "L"))
  (ExponentPart (:: (:or "E" "e")
                    (:? (:or "+" "-"))
                    (:+ Digit)))
  ;; FIXME Would recursive work??
  (DigitSequence (:+ Digit))
  (FractionalConstant (:or (:: (:? DigitSequence) #\. DigitSequence)
                           (:: DigitSequence #\.)))
  ;; FIXME hex floating constant
  (FloatConstant (:or
                  (:: DigitSequence ExponentPart (:? FloatSuffix))
                  (:: FractionalConstant (:? ExponentPart) (:? FloatSuffix))))
  (Number (:or IntegerConstant
               FloatConstant))
  ;; String
  ;; keywords
  (Keyword (:or
            "auto"
            "break"
            "case"
            "char"
            "continue"
            "default"
            "do"
            "double"
            "else"
            "enum"
            "extern"
            "float"
            "for"
            "goto"
            "if"
            "inline"
            "int"
            "long"
            "register"
            "restrict"
            "return"
            "short"
            "signed"
            "sizeof"
            "static"
            "struct"
            "switch"
            "typedef"
            "union"
            "unsigned"
            "void"
            "volatile"
            "while"
            )))


#;
(define get-token
  ;; take an input port, return a token
  ;; FIXME Will it consume it?
  (lexer-src-pos [(:or "[" "<:") (token-L_BRACKET)]
                 [(:or "]" ":>") (token-R_BRACKET)]
                 ["(" (token-L_PAREN)]
                 [")" (token-R_PAREN)]
                 [(:or "{" "<%") (token-L_BRACE)]
                 [(:or "}" "%>") (token-R_BRACE)]
                 ["." (token-PERIOD)]
                 ["->" (token-->)]
                 ["++" (token-++)]
                 ["--" (token---)]
                 ["&" (token-&)]
                 ["*" (token-*)]
                 ["+" (token-+)]
                 ["-" (token--)]
                 ["~" (token-~)]
                 ["!" (token-!)]
                 ["/" (token-/)]
                 ["%" (token-%)]
                 ["<<" (token-<<)]
                 [">>" (token->>)]
                 ["<" (token-<)]
                 [">" (token->)]
                 ["<=" (token-<=)]
                 [">=" (token->=)]
                 ["==" (token-==)]
                 ["!=" (token-!=)]
                 ["^" (token-^)]
                 ["|" (token-OR)]
                 ["&&" (token-&&)]
                 ["||" (token-OR_OP)]
                 ["?" (token-?)]
                 [":" (token-:)]
                 [";" (token-SEMI_COLON)]
                 ["..." (token-...)]
                 ["=" (token-=)]
                 ["*=" (token-*=)]
                 ["/=" (token-/=)]
                 ["%=" (token-%=)]
                 ["+=" (token-+=)]
                 ["-=" (token--=)]
                 ["<<=" (token-<<=)]
                 [">>=" (token->>=)]
                 ["&=" (token-&=)]
                 ["^=" (token-^=)]
                 ["|=" (token-OR_ASSIGN)]
                 [Keyword (string->symbol lexeme)]
                 ;; FIXME what to return?
                 [Comment (return-without-pos (get-token input-port))]
                 ;; FIXME what to return?
                 [(:+ WhiteSpace) (return-without-pos (get-token input-port))]
                 ;; TODO check type
                 ;; return Identifier or Typename
                 [Identifier (token-IDENTIFIER lexeme)]
                 ;; I'm not using string->number, just the raw string
                 [Number (token-NUMBER lexeme)]
                 ;; I'm using the raw string, instead of trimming the quotes,
                 ;; because I want to re-generate code, I'm not trying to
                 ;; evalute the code
                 [CharacterConstant (token-CHARACTER_CONSTANT lexeme)]
                 [StringLiteral (token-STRING_LITERAL lexeme)]
                 [Comment (token-COMMENT lexeme)]
                 [(eof) 'eof]))


