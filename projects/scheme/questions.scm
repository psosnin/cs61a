(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))


;; Problem 15
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 15
  (define (helper s index) 
    (if (null? s)
      s
      (cons (list index (car s)) 
            (helper (cdr s) (+ index 1))
      )
    )
  )
  (helper s 0)
)

  ; END PROBLEM 15

;; Problem 16

;; Merge two lists LIST1 and LIST2 according to COMP and return
;; the merged lists.
(define (merge comp list1 list2)
  ; BEGIN PROBLEM 16
  (define (helper list1 list2)
    (cond 
      ((null? list1) list2)
      ((null? list2) list1)
      ((comp (car list1) (car list2)) (cons (car list1) (helper (cdr list1) list2)))
      (else (cons (car list2) (helper (cdr list2) list1)))
    )
  )
  (helper list1 list2)
)
  ; END PROBLEM 16


(merge < '(1 5 7 9) '(4 8 10))
; expect (1 4 5 7 8 9 10)
(merge > '(9 7 5 1) '(10 8 4 3))
; expect (10 9 8 7 5 4 3 1)

;; Problem 17
(define (reverse n)
  (define (iter a b)
    (if (null? a) b
        (iter (cdr a) (cons (car a) b))))
  (iter n nil)
)


(define (nondecreaselist lst)
    ; BEGIN PROBLEM 17
  (define (ndl-helper lst sublst)
    (cond ((null? lst) (list (reverse sublst)))
          ((or (null? sublst)
               (>= (car lst) (car sublst)))
           (ndl-helper (cdr lst) (cons (car lst) sublst)))
          (else
           (cons (reverse sublst) (ndl-helper lst '())))))
  (ndl-helper lst '())
)
    ; END PROBLEM 17

;For example, if the input is a list containing elements
;(1 2 3 4 1 2 3 4 1 1 1 2 1 1 0 4 3 2 1)
;the output should contain elements
;((1 2 3 4) (1 2 3 4) (1 1 1 2) (1 1) (0 4) (3) (2) (1))

