#lang racket


(define (1+ n) (+ n 1))
(define (1- n) (- n 1))

(define (square x) (* x x))

(define (head l) (car l))
(define (tail l) (cdr l))


(define (myLength l)
  (define (helper curr currList)
    (cond [(null? currList) (1- curr) ]
          [else (helper (1+ curr) (tail currList))]
          )
    )
  (helper 1 l)
  )

;; It gives them in reversed order
(define (from-to a b l)
  (define (helper curr currList resultList)
    (cond [(> a curr) (helper (1+ curr) (tail currList) resultList)]
          [(> curr b) resultList]
          [else (helper (1+ curr) (tail currList) (cons (head currList) resultList))]
          )
    )
  (helper 1 l '())
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; IMPORTANT FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (id x) x)

;; Recursive accumulate or right accumulation.
(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))


;; Reverse accumulation of the result. It simulates the work of the for
;; It is iterative process or tail recursion, also it is called left accumulation.
(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))


(define (map* f l)
  (if (null? l)
      '()
      (cons (f (head l)) (map* f (tail l)))
      )
  )

(define (filter* p? l)
  (cond [(null? l) '()]
        [(p? (head l)) (cons (head l) (filter* p? (tail l)))]
        [else (filter* p? (tail l))]
        )
  )

;; This is the rigth version of foldr. The recursive one 

(define (foldr* op nv l)
  (if (null? l)
      nv
      (op (head l) (foldr* op nv (tail l)))
      )
  )

;; This is the left version of foldr. The iterative one

(define (foldr-i* op nv l)
  (if (null? l)
      nv
      (foldr* op (op nv (head l)) (tail l))
      )
  )


(define (reverse* lst)
  (foldr (lambda (el result) (append result (list el))) '() lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UPR 5 Andrei ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;zad 1

;first variant
(define (take-nth n l)
  (define (helper curr currList)
    (cond [(null? currList) '()]
          [(= curr n)(head currList)]
          [(helper (1+ curr) (tail currList))]
          )
    )
  (helper 1 l)
  )


(define (nth n l)
  (cond [(null? l) #f]
        [(= n 1) (head l)]
        [else (nth (1- n) (tail l))]
        )
  )

; zad 2
(define (range from to)
  (accumulate cons '() from to id 1+))

; zad 3
(define (digit-list n)
  (define (helper currN)
    (if (< currN 10)
        (list currN)
        (cons (remainder currN 10)
              (helper (quotient currN 10)))))
  (reverse (helper n))
 )

; zad 4

(define (take n lst)
  (cond [(or (= n 0) (null? lst)) '()]
        [else (cons (head lst) (take (1- n) (tail lst)))]
        )
  )