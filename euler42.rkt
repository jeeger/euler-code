#lang racket

(define testword "SKY")

(define (is-triangle num)
  (exact? (sqrt (add1 (* 8 num)))))

(define (read-words filename)
  (map (λ (str) (string-replace str "\"" ""))
       (string-split (file->string filename) ",")))

(define (word-to-num word)
  (foldr + 0 (map (compose (lambda (x) (- x 64)) char->integer) (string->list word))))

(define (filter-list filename)
  (filter (compose is-triangle word-to-num (lambda (l) (drop-right l 1)) (lambda (str) (string-split str ",\""))) (file->lines filename)))

(define (filter-triangular filename)
  (filter (compose is-triangle word-to-num) (read-words filename)))

(define (solution) (length (filter-triangular "words.txt")))