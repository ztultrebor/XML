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
    [(list (? symbol?))
     (string-append "<" (symbol->string (first xexpr)) " />")]
    [(? attribute?) (string-append " " (symbol->string (first xexpr)) "=\""
                              (second xexpr) "\"")]
    [(? head?) (string-append (parse (first xexpr)) (parse (rest xexpr)))]
    [(list (? symbol?) (? head?))
     (string-append "<" (symbol->string (first xexpr))
                    (parse (second xexpr)) " />")]
    [(list (? symbol?) bd) (string-append "<" (symbol->string (first xexpr))
                                          ">" (parse bd)
                                          "</" (symbol->string (first xexpr)) ">")]    
    [(list sym hd bd) (string-append "<" (symbol->string sym) ">"
                                     (parse hd) (parse bd)
                                     "</" (symbol->string sym) ">")]
    [(list f r) (string-append (parse f) (parse r))]))



; ====================
; checks

(check-expect (head? '((from "seen-e") (to "seen-f"))) #t)
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