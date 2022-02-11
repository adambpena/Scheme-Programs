#lang racket
; Desc: Using car and cdr to extract elements from a list
; Name: Adam Pena
; Date: 10/26/2021

(define myList `((a b) c (d (e f))((g (h) i))))   ; The list that items will be pulled from

(display "Items of the list myList extracted in alphabetical order: ")
(newline)

(car (car myList))  ; a

(car (cdr (car myList)))  ; b

(car (cdr myList)) ; c

(car (car (cdr (cdr myList))))  ; d

(car (car (cdr (car (cdr (cdr myList))))))  ; e

(car (cdr(car (cdr (car (cdr (cdr myList)))))))  ; f

(car (car( car(cdr (cdr (cdr myList))))))  ; g

(car (car (cdr (car( car(cdr (cdr (cdr myList))))))))  ; h

(car (cdr (cdr (car( car(cdr (cdr (cdr myList))))))))  ; i
