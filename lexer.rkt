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

   ALIGNAS ALIGNOF ATOMIC GENERIC NORETURN STATIC_ASSERT THREAD_LOCAL))

(define-empty-tokens
  et
  (! % & L_PAREN R_PAREN * + COMMA - PERIOD / : SEMI_COLON < = > ? L_BRACKET R_BRACKET ^ L_BRACE OR R_BRACE ~))

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
         
         ["auto" (token-AUTO)]
         ["break" (token-BREAK)]
         ["case" (token-CASE)]
         ["char" (token-CHAR)]

         ["const" (token-CONST)]
         ["continue" (token-CONTINUE)]
         ["default" (token-DEFAULT)]
         ["do" (token-DO)]
         ["double" (token-DOUBLE)]
         ["else" (token-ELSE)]
         ["enum" (token-ENUM)]
         ["extern" (token-EXTERN)]
         ["float" (token-FLOAT)]
         ["for" (token-FOR)]
         ["goto" (token-GOTO)]
         ["if" (token-IF)]
         ["inline" (token-INLINE)]
         ["int" (token-INT)]
         ["long" (token-LONG)]
         ["register" (token-REGISTER)]
         ["restrict" (token-RESTRICT)]
         ["return" (token-RETURN)]
         ["short" (token-SHORT)]
         ["signed" (token-SIGNED)]
         ["sizeof" (token-SIZEOF)]
         ["static" (token-STATIC)]
         ["struct" (token-STRUCT)]
         ["switch" (token-SWITCH)]
         ["typedef" (token-TYPEDEF)]
         ["union" (token-UNION)]
         ["unsigned" (token-UNSIGNED)]
         ["void" (token-VOID)]
         ["volatile" (token-VOLATILE)]
         ["while" (token-WHILE)]
         ["_Alignas" (token-ALIGNAS)]
         ["_Alignof" (token-ALIGNOF)]
         ["_Atomic" (token-ATOMIC)]
         ["_Bool" (token-BOOL)]
         ["_Complex" (token-COMPLEX)]
         ["_Generic" (token-GENERIC)]
         ["_Imaginary" (token-IMAGINARY)]
         ["_Noreturn" (token-NORETURN)]
         ["_Static_assert" (token-STATIC_ASSERT)]
         ["_Thread_local" (token-THREAD_LOCAL)]
         ["__func__" (token-FUNC_NAME)]

         ;; TODO check-type
         [(:: L (:* A)) (check-type lexeme)]
         
         [(:: HP (:+ H) (:? IS)) (token-I_CONSTANT)]
         [(:: NZ (:* D) (:? IS)) (token-I_CONSTANT)]
         [(:: "0" (:* O) (:? IS)) (token-I_CONSTANT)]
         [(:: (:? CP) #\' (:+ CChar) #\') (token-I_CONSTANT)]

         [(:: (:+ D) E (:? FS)) (token-F_CONSTANT)]
         [(:: (:* D) "." (:+ D) (:? E) (:? FS)) (token-F_CONSTANT)]
         [(:: (:+ D) "." (:? E) (:? FS)) (token-F_CONSTANT)]
         [(:: HP (:+ H) P (:? FS)) (token-F_CONSTANT)]
         [(:: HP (:* H) "." (:+ H) P (:? FS)) (token-F_CONSTANT)]
         [(:: HP (:+ H) "." P (:? FS)) (token-F_CONSTANT)]

         [(:: (:? SP) #\" (:* SChar) #\")]

         ["..." (token-ELLIPSIS)]
         [">>=" (token-RIGHT_ASSIGN)]
         ["<<=" (token-LEFT_ASSIGN)]
         ["+=" (token-ADD_ASSIGN)]
         ["-=" (token-SUB_ASSIGN)]
         ["*=" (token-MUL_ASSIGN)]
         ["/=" (token-DIV_ASSIGN)]
         ["%=" (token-MOD_ASSIGN)]
         ["&=" (token-AND_ASSIGN)]
         ["^=" (token-XOR_ASSIGN)]
         ["|=" (token-OR_ASSIGN)]
         [">>" (token-RIGHT_OP)]
         ["<<" (token-LEFT_OP)]
         ["++" (token-INC_OP)]
         ["--" (token-DEC_OP)]
         ["->" (token-PTR_OP)]
         ["&&" (token-AND_OP)]
         ["||" (token-OR_OP)]
         ["<=" (token-LE_OP)]
         [">=" (token-GE_OP)]
         ["==" (token-EQ_OP)]
         ["!=" (token-NE_OP)]

         ;; empty tokens
         [";" (token-SEMI_COLON)]
         [(:or "{" "<%") (token-L_BRACE)]
         [(:or "}" "%>") (token-R_BRACE)]
         ["," (token-COMMA)]
         [":" (token-:)]
         ["=" (token-=)]
         ["(" (token-L_BRACE)]
         [")" (token-R_BRACE)]
         [("["|"<:") (token-L_BRACKET)]
         [("]"|":>") (token-R_BRACKET)]
         ["." (token-PERIOD)]
         ["&" (token-&)]
         ["!" (token-!)]
         ["~" (token-~)]
         ["-" (token--)]
         ["+" (token-+)]
         ["*" (token-*)]
         ["/" (token-/)]
         ["%" (token-%)]
         ["<" (token-<)]
         [">" (token->)]
         ["^" (token-^)]
         ["|" (token-OR)]
         ["?" (token-?)]

         [(:+ WS) #f]                   ; remove whitespace
         [any-string #f]                ; discard bad characters
         ))




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

(module+ test
  (let* ([in (open-input-string "int main() {return 0;}")])
    (let loop ([token (get-token in)])
      (println token)
      (unless (eq? (position-token-token token) 'eof) (loop (get-token in))))))
