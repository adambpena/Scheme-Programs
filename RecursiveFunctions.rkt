#lang racket
; Name: Adam Pena
; Date: 11/23/2021
; Desc:
;   3 functions for scheme:
;      - dotProduct: Evaluates the "dot product" of two lists (assumed vectors)
;      - checkDup: Checks for duplicates in a list. Returns true if so, false if not.
;      - largestDif: Returns the largest difference between corresponding elements in a list of same length

(define dotProduct (lambda (lst1 lst2) ; Define dot product as expression using lst1 and lst2 as local variables
     (if(or(null? lst1)(null? lst2))   ; If either list is null, return nothing
        0
       (+ (* (car lst1) (car lst2)) (dotProduct (cdr lst1) (cdr lst2)))))) ; Return element (lst1[0] * lst2[0]) + (lst1[1] * lst2[1]) + ... + (lst1[n-1] * lst2[n-1]) + (lst1[n] * lst2[n])


(displayln "Checking dot product function: ")

(dotProduct '(1 3 -5) '(4 -2 -1))      ; Function call testing dot product, (1*4)+(3*(-2))+((-5)*(-1)) = 3
(dotProduct '(4 3) '(6 1))             ; (4*6) + (3*1) = 27
(dotProduct '(16 21) '(2 4))           ; (16*2) + (21*4) = 116
(newline)



(define checkDup (lambda (lst)               ; checkDup (check duplicate) function. Defined as function around parameter "lst"
      (cond ((null? lst) #f)                 ; switch: case lst == null, return false
         ((member (car lst) (cdr lst)) #t)   ;         case lst[0] is a member in the rest of the list, return true.
         (else(checkDup (cdr lst)))          ; Else: check duplicate using lst minus first element. Do so recusively until duplicate found or no duplicate found
      )
   )
)


(displayln "Checking test duplicate function: ")

(checkDup '(2 4 4 25 3))               ; Function calls testing check duplicate, #t
(checkDup '(10 9 8 7 6))               ; #f
(checkDup '(202 0 6 0 31))             ; #t

(newline)


(define (largestDif list1 list2)                  ; Define largestDif as function w/ two parameters: list1 and list2
   (define (absListDif lst1 lst2)                      ; Define function within largestDif that returns list of absolute value of difference between elements in list
      (if (or(null? lst1)(null? lst2)) null            ; If either list empty, return null
          (cons (abs (- (car lst1)(car lst2)))         ; Otherwise, return the absolute value of the first element of the list minus the second
          (absListDif (cdr lst1) (cdr lst2)))))        ; Call function again with rest of lists
  
  (define (getLargestElem lst)
      (let sortLoop ((restOfList (cdr lst))            ; Another function within largestDif that finds the largest element of a list (lst)
                     (maxOfList (car lst)))            ; Expression set loop defined w parameters restOfList (end of lst) and maxOfList (first element of list)
        (cond ((null? restOfList) maxOfList)           ; switch: case reached end of list, return the max
              ((> (car restOfList) maxOfList)(sortLoop (cdr restOfList)(car restOfList))) ; case elem in list is greater than max, recall the sortLoop and redefine max
              (else (sortLoop (cdr restOfList) maxOfList))))) ; Otherwise, call sortLoop again with maxOfList kept the same
  
  (if(or(null? list1)(null? list2)) #f                 ; If either list is null, return false
     (getLargestElem(absListDif list1 list2))))        ; Else, call return largest element function with the resulting list from absListDif called w/ the 2 lists passed into the overall function



(displayln "Checking largest difference in list elements function: ")

(largestDif '(2 6 3) '(4 6 2))         ; Function call testing largest difference function, |2 - 4| = 2
(largestDif '(8 4) '(6 7))             ; |5 - 7| = 3
(largestDif '(66 12) '(0 9))           ; |66 - 0| = 66

(newline)