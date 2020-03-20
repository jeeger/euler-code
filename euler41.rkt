#lang racket
(require math/number-theory)

(define (contains-exactly num set)
  (cond
    ((and (empty? set) (> num 0)) #f)
    ((and (= num 0) (not (empty? set))) #f)
    ((and (empty? set) (= num 0)) #t)
    (#t (contains-exactly (quotient num 10) (remq (remainder num 10) set)))))

(define (pandigital-num num)
  (contains-exactly num (range 1 10)))

(define (join-num list)
  (define (join-internal list num)
    (if (empty? list)
        num
        (join-internal (rest list) (+ (first list) (* num 10)))))
  (join-internal list 0))

(define (largest-pandigital-prime digits)
  (apply max (sequence->list (sequence-filter prime? (sequence-map join-num (in-permutations (range digits 0 -1)))))))

(define (find-solution)
  (largest-pandigital-prime 7))

