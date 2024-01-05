;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname xml) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)


; An X-Expression is a list:
; - (cons Symbol Content)
; where Content is one of
; – (cons Symbol Body)
; – (cons Symbol (cons Head Body))
; where Body is short for [List-of X-Expression]
; where Head is short for [List-of Attribute]
; in which an Attribute is a list of two items:
;   (cons Symbol (cons String '()))


(define (attribute? xexpr)
  ; X-Expression -> Boolean
  ; returns #true if the X-expression is an Attribute, else #false
  (and
   (list? xexpr)
   (= (length xexpr) 2)
   (symbol? (first xexpr))
   (string? (second xexpr))))


(define (head? xexpr)
  ; X-Expression -> Boolean
  ; returns #true if the X-expression is a Head
  ; (i.e., a [ListOf Attribute]), else #false
  (or
   (empty? xexpr)
   (and
    (attribute? (first xexpr))
    (head? (rest xexpr)))))


(define (parse xexpr)
  ; X-Expression -> String
  ; takes an X-Expression and converts it into XML
  (match xexpr
    [(? empty?) ""]
    [(? attribute?) (string-append " " (symbol->string (first xexpr)) "=\""
                                   (second xexpr) "\"")]
    [(? head?) (string-append (parse (first xexpr)) (parse (rest xexpr)))]
    [(list (? symbol?))
     (string-append "<" (symbol->string (first xexpr)) " />")]
    [(list (? symbol?) (? head?))
     (string-append "<" (symbol->string (first xexpr))
                    (parse (second xexpr)) " />")]
    [(cons (? symbol?) (cons (? head?) rem))
     (string-append "<" (symbol->string (first xexpr)) (parse (second xexpr))
                    ">" (parse rem)
                    "</" (symbol->string (first xexpr)) ">")]
    [(cons (? symbol?) rem)
     (string-append "<" (symbol->string (first xexpr)) ">" (parse rem)
                    "</" (symbol->string (first xexpr)) ">")]
    [(? list?) (string-append (parse (first xexpr)) (parse (rest xexpr)))]))



; ====================
; checks

(check-expect (head? '((from "seen-e") (to "seen-f"))) #t)
(define a0 '((initial "X")))
(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))
(check-expect (parse e0) "<machine />")
(check-expect (parse e1) "<machine initial=\"X\" />")
(check-expect (parse e2) "<machine><action /></machine>")
(check-expect (parse e3) "<machine><action /></machine>")
(check-expect (parse e4) "<machine initial=\"X\"><action /><action /></machine>")
(check-expect (parse '(start)) "<start />")
(check-expect (parse '(server ((name "example.org"))))
              "<server name=\"example.org\" />")
(check-expect (parse '(carcas (board (grass)) (player ((name "sam")))))
              "<carcas><board><grass /></board><player name=\"sam\" /></carcas>")
(check-expect (parse '((transition ((from "seen-e") (to "seen-f")))
                       (ul (li (word) (word)) (li (word)))))
              (string-append 
               "<transition from=\"seen-e\" to=\"seen-f\" />"
               "<ul><li><word /><word /></li><li><word /></li></ul>"))