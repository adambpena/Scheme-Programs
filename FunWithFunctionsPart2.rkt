#lang racket
; Name: Adam Pena
; Date: 11/19/2021
; Desc: A group of functions using conditional statements like 'if' and 'cond'


; (1) Water Weight

(define (WaterWeight D L K)           ; Waterweight function defined with 3 parameters: diameter, length, and water density
  (if(equal? K #t)                    ; If water is pure water, water density is 62.4 lbs/ft^3
     (* 3.14159 (/ D 24) L 62.4)
     (* 3.14159 (/ D 24) L 64.08)))   ; Else, water is sea water w/ water density of 64.08 lbs/ft^3

; Function Calls

(display "PW Pipe Diameter 20, Pipe Length 24 ")
(WaterWeight 20 24 #t)
(display "SW Pipe Diameter 20, Pipe Length 24 ")
(WaterWeight 20 24 #f)


; (2) Radio Frequency
 
(define (RadioFrequency f)    ; Function for determining radio freq classification based on frequency passed in MHz
  (set! f (* f 1000))         ; Convert MHz to KHz
  (cond((and(> f 10)(<= f 30)) (string-append "VL" "F"))  ; case 10 < f <= 30, very low frequency
        ((and(> f 30)(<= f 300)) (string-append "L" "F")) ; *String-appends used to avoid #<void> from being returned in siplae
        ((and(> f 300)(<= f 3000)) (string-append "M" "F"))
        ((and(> f 3000)(<= f 30000)) (string-append "H" "F"))
        ((and(> f 30000)(<= f 328600)) (string-append "VH" "F"))
        ((and(> f 328600)(<= f 2009000)) (string-append "UL" "F"))
        ((> f 2009000)(string-append "Invalid" " Value"))))


; Function Calls

(displayln "20 KHz")
(displayln (RadioFrequency .02))
(displayln "50 KHz")
(displayln (RadioFrequency .05))
(displayln "350 KHz")
(displayln (RadioFrequency .350))
(displayln "3500 KHz")
(displayln (RadioFrequency 3.500))
(displayln   "328599 KHz")
(displayln (RadioFrequency 32.8599))
(displayln   "2000000 KHz")
(displayln (RadioFrequency 2000.000))
(displayln   "4000000 KHz")
(displayln (RadioFrequency 4000.000))


;(3) Paycheck

(define (PayCheck payType hourssales rate kissup)  ; PayCheck function, 4 parameters
  (let ((hourlyPay (* hourssales rate)) ; binding calculation for hourly pay to hourlyPay label
     (overtimePay (+ (* hourssales rate)(* (- hourssales 40)(* rate 1.5))))) ; overtimePay = hourlyPay + overtime
    
  (if(and(equal? payType #t)(>= hourssales 40))(- (+ hourlyPay overtimePay) kissup)   ; if hourly & worked more than 40 hrs, return hourlyPay + overtime pay - kissup
     (if(and(equal? payType #t)(<= hourssales 40))(- hourlyPay kissup)                ;    else if hourly & worked less than 40, return hourlyPay - kissup
     (if(and(equal? payType #f)(< hourssales 1000))(- (* hourssales 0.01) kissup)     ;    else if sales and sold less than 1000, calculate pay accordingly
     (if(and(equal? payType #f)(>= hourssales 1000)(< hourssales 10000))(- (* hourssales 0.025) kissup) ; else if sale and btwn 1000 and 10000, calc pay accordingly
     (- (* hourssales 0.06) kissup))))))) ; else, sales and higher than 10000
       

;Function Calls

(display "Pay 35 hours, $10 hr, no kissup $")
(PayCheck #t 35 10 0)
(display "Pay 45 hours, $10 hr, no kissup $")
(PayCheck #t 45 10 0)
(display "Pay 35 hours, $10 hr, $20 kissup $")
(PayCheck #t 35 10 20)
(display "Pay 45 hours, $10 hr, $20 kissup $")
(PayCheck #t 45 10 20)
(display "Pay $500 sales, $0 hr, no kissup $")
(PayCheck #f 500 0 0)
(display "Pay $5000 sales, $0 hr, no kissup $")
(PayCheck #f 5000 0 0)
(display "Pay $15000 sales, $0 hr, no kissup $")
(PayCheck #f 15000 0 0)
(display "Pay $500 sales, $0 hr, $20 kissup $")
(PayCheck #f 500 0 20)
(display "Pay $5000 sales, $0 hr, $20 kissup $")
(PayCheck #f 5000 0 20)
(display "Pay $15000 sales, $0 hr, $20 kissup $")
(PayCheck #f 15000 0 20)