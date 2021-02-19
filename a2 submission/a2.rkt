#lang racket

;; The `[data #f]` is a default value for the 2nd positional argument.
;; That way, this function can take 1 arg or 2 args.
(define (token type [data #f])
  (list type data))

;;;; Token creator functions
;;;;
;;;; Given a string matching a regexp, return a token for that input or #f for
;;;; no token.

(define (skip-match str) #f)

(define (punctuation-token str)
  (token
    (case str
      [(";") 'SEMICOLON]
      [(",") 'COMMA]
      [(".") 'PERIOD]
      [("(") 'OPAREN]
      [(")") 'CPAREN]
      [("{") 'OBRACE]
      [("}") 'CBRACE])))

(define (float-token str)
  (token 'FLOAT (string->number str)))

(define (int-token str)
  (token 'INT (string->number str)))

(define (string-token str)
  (token 'STRING (substring str 1 (- (string-length str) 1))))

(define (backslash-token str)
  (token 'STRING (substring str 1))
  )

(define (name-or-keyword-token str)
  (case str
    [("if" "not" "def" "fun" "and" "or")
     (token (string->symbol (string-upcase (string-trim str))))]
    [else (token 'NAME (string->symbol str))]))

;;;; Lexing rules table
;;;;
;;;; Each item in the table is a 2-tuple (i.e. list of 2 elements):
;;;; 1. a regexp to detect a token at the beginning of a string
;;;; 2. a function (from above) to take the matched string and create a token

(define re-table
  (list
    (list #rx"^[ \r\n\t]+" skip-match) ; whitespace
    (list #rx"^//[^\n]+(\n|$)" skip-match) ; // comments
    (list #rx"^/[*].*[*]/(?=[\r\n\t (){},;.]|$)" skip-match);/* comments  */
    (list #rx"^[;(){},.]" punctuation-token)
    (list #rx"^\"[^\"]*\"(?=[\r\n\t (){},;.]|$)" string-token)
    (list #rx"^[+-]?[0-9]*[.][0-9]+(?=[\r\n\t (){},;.]|$)" float-token)
    (list #rx"^[+-]?[0-9]+(?=[\r\n\t (){},;.]|$)" int-token)
    (list #rx"^[A-Za-z]+[A-Za-z0-9]*(?=[\r\n\t (){},;.]|$)" name-or-keyword-token)
    (list #rx"^[+/*<>=-]+(?=[\r\n\t (){},;.]|$)" name-or-keyword-token)
    ))

(define str-length 0)
(define str "")
(define proc null)

(define (find-proc list table)
  (if (empty? list)
      #f
      (if (not (first list))
          (find-proc (rest list) (rest table))
          (begin
            (set! str-length (string-length (first (first list))))
            (set! str (first (first list)))
            (set! proc (first (rest (first table))))
            (first (rest (first table)))
           )
          )
      )
  )

(define (lex-helper string)
  (if (not (find-proc (map (lambda (entry) (regexp-match (first entry) string)) re-table) re-table))
      (begin
       (set! str-length (string-length string))
       (token 'INVALID string)
       )
      (if (not (equal? proc skip-match))
          (proc str)
          null
          )
  )
  )
  
(define val null)
(define (lex string)
  (if (= (string-length string) 0)
      null
      (begin
        (set! val (lex-helper string))
        (if (null? val)
            (lex (substring string str-length))
            (cons val (lex (substring string str-length)))
      )
      ))
  )
