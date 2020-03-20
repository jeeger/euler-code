#lang racket
(require racket/set)

(define (contains-exactly num set)
  (cond
    ((and (empty? set) (> num 0)) #f)
    ((and (= num 0) (not (empty? set))) #f)
    ((and (empty? set) (= num 0)) #t)
    (#t (contains-exactly (quotient num 10) (remq (remainder num 10) set)))))

(define (pandigital-num num)
  (contains-exactly num (range 1 10)))

(define (concatenate-nums . nums)
  (string->number (apply string-append (map number->string nums))))

(define (largest-possibly-pandigital-concatenated-product num)
  (define (largest-internal mult prod)
    (let ((new-prod (concatenate-nums prod (* num mult))))
      (if (> new-prod (sub1 10e8))
          prod
          (largest-internal (add1 mult) new-prod))))
  (largest-internal 2 num))

(filter pandigital-num (map largest-possibly-pandigital-concatenated-product (range 9000 9999)))