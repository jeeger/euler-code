#lang racket
(require math/number-theory)
(require racket/match)



(define (is-quadratic num)
  (let-values ([(root rest) (integer-sqrt/remainder num)])
    (= rest 0)))

(define (possible-hypothenuse total)
  (range 1 (/ (- total 2) 3)))

(define (make-triple u v)
  (list (- (sqr u) (sqr v))
          (* 2 u v)
          (+ (sqr u) (sqr v))))

(define (triple-sum u v)
  (foldr + 0 (make-triple u v)))

(define (solutions-for perimeter)
  (for*/list ([h (in-range (quotient perimeter 3) perimeter)]
              [s1 (in-range 1 (quotient (- perimeter h) 2))]
              #:when (= (+ (sqr s1)
                           (sqr (- perimeter h s1)))
                        (sqr h)))
    (list h s1 (- perimeter h s1))))

(define (find-problem-solution max)
  (foldr (lambda (old new)
           (if (> (length (rest new))
                  (length (rest old)))
               new
               old))
         '(0 ())
         (for/list ([perimeter (in-range 2 max)])
           (cons perimeter (solutions-for perimeter)))))
