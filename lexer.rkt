#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(provide Keywords Basic Special Punctuators get-token)

(define-empty-tokens Keywords
  (auto
   break
   case
   char
   continue
   default
   do
   double
   else
   enum
   extern
   float
   for
   goto
   if
   inline
   int
   long
   register
   restrict
   return
   short
   signed
   sizeof
   static
   struct
   switch
   typedef
   union
   unsigned
   void
   volatile
   while
   _Bool
   _Complex
   _Imaginary))
;; with lexeme
(define-tokens Basic
  (IDENTIFIER NUMBER CHARACTER_CONSTANT STRING_LITERAL COMMENT))
;; without lexeme
(define-empty-tokens Special
  (EOF))
;; (define-empty-tokens Operators
;;   (...
;;    ))

(define-empty-tokens Punctuators
  (L_BRACKET                            ; [
   R_BRACKET                            ; ]
   L_PAREN                              ; (
   R_PAREN                              ; )
   L_BRACE                              ; {
   R_BRACE                              ; }
   PERIOD                               ; .
   ->                                   ; PTR_OP
   ++                                   ; INC_OP
   --                                   ; DEC_OP
   &
   *
   +
   -
   ~
   !
   /
   %
   <<                                   ; LEFT_OP
   >>                                   ; RIGHT_OP
   <
   >
   <=                                   ; LE_OP
   >=                                   ; GE_OP
   ==                                   ; EQ_OP
   !=                                   ; NE_OP
   ^
   OR                                   ; |
   &&                                   ; AND_OP
   OR_OP                                ; ||
   ?
   :
   SEMI_COLON                           ; ;
   ...                                  ; ELLIPSIS
   =
   *=                                   ; MUL_ASSIGN
   /=                                   ; DIV_ASSIGN
   %=                                   ; MOD_ASSIGN
   +=                                   ; ADD_ASSIGN
   -=                                   ; SUB_ASSIGN
   <<=                                  ; LEFT_ASSIGN
   >>=                                  ; RIGHT_ASSIGN
   &=                                   ; AND_ASSIGN
   ^=                                   ; XOR_ASSIGN
   OR_ASSIGN                            ; |=
   ;; digraphs
   <:                                   ; [
   :>                                   ; ]
   <%                                   ; {
   %>                                   ; }
   ;; TODO
   %:                                   ; #
   %:%:                                 ; ##
   ))

(define-lex-abbrevs
  ;; (LineTerminator (:or #\return #\linefeed
  ;;                      (:: #\return #\linefeed)))
  (WhiteSpace (:or #\space #\tab #\page #\return #\linefeed))
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
  (CharacterConstant (:: (:? "L") "'" (:+ CChar) "'"))
  (CChar (:or (complement #\')
              EscapeSequence))
  ;; TODO octal and hex escape
  (EscapeSequence (:or "\\a" "\\b" "\\f" "\\n" "\\r" "\\t" "\\v"
                       "\\'" "\\\"" "\\?" "\\\\"))
  (StringLiteral (:: (:? "L") #\" (:* SChar) #\"))
  (SChar (:or (complement #\")
              EscapeSequence))
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
