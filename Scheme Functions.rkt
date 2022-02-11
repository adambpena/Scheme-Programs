#lang racket
; Author:  Adam Pena
; Date:    9/30/2021

; Function ex.  The distance function
; Given   r - Rate of Speed
;         t - Time of Travel
; Result  d = rt

(define (distance r t)
  (* r t))

(display "Distance calculated is (Function example): ")
(distance 70 1)
(newline)

; Function 1.  Future Value Function
; Given P payment
;       r interest
;       t num years
; Result  Future Value of your investement at end of t years

(define (FutureValue p r t)
  (let ((Numerator (- (expt (+ 1 r) t) 1)))
       (display "Future Value of the account is: " )
       (* p (/ Numerator r) (+ 1 r))))


; Function 2. Present Value Function
; Given: pymt Regular payment
;           r interest
;           c cost of living adjustment
;           t term
; Result: Present value (P) that must be deposited to receive pymt for the next t years

(define (PresentValue pymt r c t)
  (let ((NumeratorFraction (expt (/ (+ 1 c) (+ 1 r)) t))
        (Denominator (- r c)))
        (display "Present value that must be deposited is: ")
        (* pymt (/ (- 1 NumeratorFraction) Denominator))))

; Function 3. Monthly payment Function
; Given:    i Monthly interest rate
;           N Number of months
;           P Present value
; Result: Monthly payment (m) given above values

(define (MonthlyPayment i N P)
  (let ((Denominator (- 1 (expt (+ 1 i) (* N -1)))))
        (display "Peter Parker's monthly payment is: ")
        (/ (* P i) Denominator)))

; Function 4. Compounded Interest Function
; Given:    m Monthly payment
;           i Monthly interest rate
;           N Number oof mnoths
; Result: Future value (A) of the account

(define (CompoundedInterest m i N)
  (let ((Fraction (/ m i))
        (Brackets (- (expt (+ 1 i) (+ N 1)) 1)))
        (display "Amount of money in Clark Kent's account after 10 years is: ")
        (- (* Fraction Brackets) m)))

; Function 5. Deposit Needed function
; Given:    F Future value
;           r Annual interest rate
;           t Number of years
;           i Periodic interest rate
;           n number of periods
; Result: Monthly payment (pymt) needed to get future value (F) within given time period

(define (DepositNeeded F i n)
  (let ((Denominator (- (expt (+ i 1) n) 1))
        (Numerator (* F i)))
        (display "Payment Carol Brady needs to deposit at the end of each of the next 11 months is: ")
        (/ Numerator Denominator)))

; Function 6. Annuity Value Function
; Given:    m Monthly payment
;           i Interest rate (monthly)
;           N Number of months
; Result: Present value (P) of annuity

(define (AnnuityValue m i N)
  (let ((Parenthesis (/ m i))
        (Brackets (- 1 (expt (+ 1 i) (* N -1)))))
        (display "Present value of ordinary annuity is: ")
        (* Parenthesis Brackets)))


(FutureValue 2000 .10 20)  
(PresentValue 10000 0.05 0.02 10)
(MonthlyPayment 0.01 36 12000)
(CompoundedInterest 200 0.005 120)
(DepositNeeded 1000 0.01 11)
(AnnuityValue 100 0.01 60)