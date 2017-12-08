#lang racket

(require parser-tools/lex)

(provide (all-defined-out))

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
