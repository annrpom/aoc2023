#lang racket
(require "../lib/common.rkt"
         rackunit)

;; this is the day we are trying to solve
(define N 1)

;; pt 1

;; solution 1:
;; for each word in our list, w, we can put a pointer in the front
;; and a pointer in the back. increment (front)/decrement (back) either
;; pointer until they encounter their first int, return here.
;; otherwise, go until pointers meet and return either the int value at
;; their meet or 00 if neither pointers saw a value.
;;
;; start:
;; "hi1nrt"
;;  ↑    ↑
;;  f    b   f=0, b=0 <= keep track of final state with some vars
;;
;; step 1:
;; "hi1nrt"
;;   ↑  ↑
;;   f  b    f=0, b=0
;;
;; step 2:
;; "hi1nrt"
;;    ↑↑
;;    fb    f=1, b=0 <= f is done searching; b goes until it "meets" f
;;
;; step 3:
;; "hi1nrt"
;;    ↑
;;    f,b    f=1, b=1 <= final answer is 11

;; solution 2:
;; this is racket. don't think about pointers. traverse a "list of char"

;; decode-string bleh bleh TODO
(define/contract decode-string
  (-> number? number? (*list/c char?) number?)
  (λ (f b cs)
    (cond
      [(empty? cs) (+ (* f 10) (if (= 0 b) f b))]
      [else (define numeric? (char-numeric? (car cs)))
            (define new-f (if (and (= 0 f) numeric?) (string->number (string (car cs))) f))
            (define new-b (if (and (< 0 f) numeric?) (string->number (string (car cs))) b))
            (decode-string new-f new-b (cdr cs))])))

(check-equal? 11 (decode-string 0 0 (string->list "hi1nrt")))
(check-equal? 15 (decode-string 0 0 (string->list "a1b2c3d4e5f")))

(define day1a
  (λ (ls)
    (foldl (λ (s v) (+ v (decode-string 0 0 (string->list s)))) 0 ls)))

(check-equal? 142 (day1a (list "1abc2" "pqr3stu8vwx" "a1b2c3d4e5f" "treb7uchet")))

;; pt 2
(define day1b
  (λ (ls)
    (void)))

;; main
(module+ main
    (call-with-input-file "../data/day1.txt"
      (lambda (prt)
        (define lines (port->lines prt))
        (answer N 1 (day1a lines))
        (answer N 2 (day1b lines)))))