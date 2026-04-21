#lang br
(require brag/support)

(define-lex-abbrev digits (:+ (char-set "0123456789")))

(define basic-lexer
  (lexer-srcloc
   [whitespace (token lexeme #:skip? #t)] ; skip whitespace
   [(from/stop-before ";" "\n") (token lexeme #:skip? #t)] ; skip comments

   ; punctuation tokens
   ["{" (token 'LBRACE lexeme)]
   ["}" (token 'RBRACE lexeme)]
   ["(" (token 'LPAREN lexeme)]
   [")" (token 'RPAREN lexeme)]
   [":" (token 'COLON lexeme)]
   ["," (token 'COMMA lexeme)]

   ; particle system tokens
   ["system" (token 'SYSTEM lexeme)] ; defines a particle system
   ["particles" (token 'PARTICLES lexeme)]
   ["center" (token 'CENTER lexeme)]
   ["velocity" (token 'VELOCITY lexeme)]
   
   ; simulation tokens
   ["simulation" (token 'SIMULATION lexeme)]
   ["size" (token 'SIZE lexeme)]
   ["time" (token 'TIME lexeme)]
   ["simulate" (token 'SIMULATE lexeme)]


   ; data structures
   [(:seq alphabetic (:* (:or alphabetic numeric "$")))
    (token 'ID (string->symbol lexeme))] ; identifier
   [(:seq (:? "-") (:or (:seq (:? digits) "." digits)
                        (:seq digits ".")))
    (token 'DECIMAL (string->number lexeme))] ; decimal
   [(:seq (:? "-") digits) (token 'INTEGER (string->number lexeme))])) ; integer
   

   

(provide basic-lexer)
