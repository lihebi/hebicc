#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(provide t et)

(define-tokens
  t
  (
   IDENTIFIER I_CONSTANT F_CONSTANT STRING_LITERAL FUNC_NAME SIZEOF
   PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
   AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
   SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
   XOR_ASSIGN OR_ASSIGN
   TYPEDEF_NAME ENUMERATION_CONSTANT

   TYPEDEF EXTERN STATIC AUTO REGISTER INLINE
   CONST RESTRICT VOLATILE
   BOOL CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE VOID
   COMPLEX IMAGINARY 
   STRUCT UNION ENUM ELLIPSIS              

   CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

   ALIGNAS ALIGNOF ATOMIC GENERIC NORETURN STATIC_ASSERT THREAD_LOCAL

   ! % & L_PAREN R_PAREN * + COMMA - PERIOD / : SEMI_COLON < = > ? L_BRACKET R_BRACKET ^ L_BRACE OR R_BRACE ~
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
         
         ["auto" (token-AUTO "auto")]
         ["break" (token-BREAK "break")]
         ["case" (token-CASE "case")]
         ["char" (token-CHAR "char")]

         ["const" (token-CONST "const")]
         ["continue" (token-CONTINUE "continue")]
         ["default" (token-DEFAULT "default")]
         ["do" (token-DO "do")]
         ["double" (token-DOUBLE "double")]
         ["else" (token-ELSE "else")]
         ["enum" (token-ENUM "enum")]
         ["extern" (token-EXTERN "extern")]
         ["float" (token-FLOAT "float")]
         ["for" (token-FOR "for")]
         ["goto" (token-GOTO "goto")]
         ["if" (token-IF "if")]
         ["inline" (token-INLINE "inline")]
         ["int" (token-INT "int")]
         ["long" (token-LONG "long")]
         ["register" (token-REGISTER "register")]
         ["restrict" (token-RESTRICT "restrict")]
         ["return" (token-RETURN "return")]
         ["short" (token-SHORT "short")]
         ["signed" (token-SIGNED "signed")]
         ["sizeof" (token-SIZEOF "sizeof")]
         ["static" (token-STATIC "static")]
         ["struct" (token-STRUCT "struct")]
         ["switch" (token-SWITCH "switch")]
         ["typedef" (token-TYPEDEF "typedef")]
         ["union" (token-UNION "union")]
         ["unsigned" (token-UNSIGNED "unsigned")]
         ["void" (token-VOID "void")]
         ["volatile" (token-VOLATILE "volatile")]
         ["while" (token-WHILE "while")]
         ["_Alignas" (token-ALIGNAS "_Alignas")]
         ["_Alignof" (token-ALIGNOF "_Alignof")]
         ["_Atomic" (token-ATOMIC "_Atomic")]
         ["_Bool" (token-BOOL "_Bool")]
         ["_Complex" (token-COMPLEX "_Complex")]
         ["_Generic" (token-GENERIC "_Generic")]
         ["_Imaginary" (token-IMAGINARY "_Imaginary")]
         ["_Noreturn" (token-NORETURN "_Noreturn")]
         ["_Static_assert" (token-STATIC_ASSERT "_Static_assert")]
         ["_Thread_local" (token-THREAD_LOCAL "_Thread_local")]
         ["__func__" (token-FUNC_NAME "__func__")]

         ;; TODO check-type
         [(:: L (:* A)) #;(check-type lexeme)
                        (token-IDENTIFIER lexeme)]
         
         [(:: HP (:+ H) (:? IS)) (token-I_CONSTANT lexeme)]
         [(:: NZ (:* D) (:? IS)) (token-I_CONSTANT lexeme)]
         [(:: "0" (:* O) (:? IS)) (token-I_CONSTANT lexeme)]
         [(:: (:? CP) #\' (:+ CChar) #\') (token-I_CONSTANT lexeme)]

         [(:: (:+ D) E (:? FS)) (token-F_CONSTANT lexeme)]
         [(:: (:* D) "." (:+ D) (:? E) (:? FS)) (token-F_CONSTANT lexeme)]
         [(:: (:+ D) "." (:? E) (:? FS)) (token-F_CONSTANT lexeme)]
         [(:: HP (:+ H) P (:? FS)) (token-F_CONSTANT lexeme)]
         [(:: HP (:* H) "." (:+ H) P (:? FS)) (token-F_CONSTANT lexeme)]
         [(:: HP (:+ H) "." P (:? FS)) (token-F_CONSTANT lexeme)]

         [(:: (:? SP) #\" (:* SChar) #\") (token-STRING_LITERAL lexeme)]

         ["..." (token-ELLIPSIS "...")]
         [">>=" (token-RIGHT_ASSIGN ">>=")]
         ["<<=" (token-LEFT_ASSIGN "<<=")]
         ["+=" (token-ADD_ASSIGN lexeme)]
         ["-=" (token-SUB_ASSIGN lexeme)]
         ["*=" (token-MUL_ASSIGN lexeme)]
         ["/=" (token-DIV_ASSIGN lexeme)]
         ["%=" (token-MOD_ASSIGN lexeme)]
         ["&=" (token-AND_ASSIGN lexeme)]
         ["^=" (token-XOR_ASSIGN lexeme)]
         ["|=" (token-OR_ASSIGN lexeme)]
         [">>" (token-RIGHT_OP lexeme)]
         ["<<" (token-LEFT_OP lexeme)]
         ["++" (token-INC_OP lexeme)]
         ["--" (token-DEC_OP lexeme)]
         ["->" (token-PTR_OP lexeme)]
         ["&&" (token-AND_OP lexeme)]
         ["||" (token-OR_OP lexeme)]
         ["<=" (token-LE_OP lexeme)]
         [">=" (token-GE_OP lexeme)]
         ["==" (token-EQ_OP lexeme)]
         ["!=" (token-NE_OP lexeme)]

         ;; empty tokens
         [";" (token-SEMI_COLON lexeme)]
         [(:or "{" "<%") (token-L_BRACE "{")]
         [(:or "}" "%>") (token-R_BRACE "}")]
         ["," (token-COMMA lexeme)]
         [":" (token-: lexeme)]
         ["=" (token-= lexeme)]
         ["(" (token-L_BRACE lexeme)]
         [")" (token-R_BRACE lexeme)]
         [(:or "[" "<:") (token-L_BRACKET "[")]
         [(:or "]" ":>") (token-R_BRACKET "]")]
         ["." (token-PERIOD lexeme)]
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
         ["|" (token-OR lexeme)]
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


