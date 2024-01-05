;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname xml) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)


; An X-Expression is a list:
; - (cons Symbol Content)
; where Content is one of
; – Body
; – (cons Head Body)
; where Body is short for [List-of X-Expression]
; where Head is short for [List-of Attribute]
; in which an Attribute is a list of two items:
;   (cons Symbol (cons String '()))
#;
(define (fn-on-xexpr xexpr)
  (local (
          (define sym (first xexpr))
          (define (headbody (rest xexpr))))
    ; - IN -
    (cond
      [(head? headbody)
       ((fn-on-symbol sym)
        ... (fn-on-head (first headbody)))]
      [(head? (first headbody))
       ((fn-on-symbol sym)
        ... (fn-on-head (first headbody))
        ... (fn-on-xexpr (rest headbody)))]
      [else ((fn-on-symbol sym)
             ... (fn-on-xexpr headbody))])))


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
  ; takes an X-Expression and converts it into XML-style text
  (local (
          (define sym (first xexpr))
          (define headbody (rest xexpr)))
    ; - IN -
    (match headbody
      [(cons (? head?) body) (translate sym (first headbody) body)]
      [body (translate sym '() body)])))


(define (parse* xexpr)
  ; Body -> String
  ; takes an [ListOf X-Expression], maps it into XML-style text,
  ; and reduces it into a XML test string
  (foldr string-append "" (map parse xexpr)))


(define (translate sym head body)
  ; Symbol Head Body -> String
  ; translates aspects of an X-expression into XML-style text
  (match body
    [(? empty?)
     (string-append "<" (symbol->string sym) (translate-attributes head) " />")]
    [bd
     (string-append "<" (symbol->string sym) (translate-attributes head) ">"
                   (parse* bd) "</" (symbol->string sym) ">")]))


(define (translate-attributes head)
  ; Head -> String
  ; translates head attributes into XML-style text
  (match head
    [(? empty?) ""]
    [(cons (list sym txt) rem)
     (string-append " " (symbol->string sym) "=\""
                    txt "\"" (translate-attributes rem))]))




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
(check-expect (parse* '((transition ((from "seen-e") (to "seen-f")))
                       (ul (li (word) (word)) (li (word)))))
              (string-append 
               "<transition from=\"seen-e\" to=\"seen-f\" />"
               "<ul><li><word /><word /></li><li><word /></li></ul>"))