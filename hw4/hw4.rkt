#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; these definitions are simply for the purpose of being able to run the tests
;; you MUST replace them with your solutions
;;


; 1
(define (sequence low high stride)
  (if (> low high)
      '()
      (cons low (sequence (+ low stride) high stride))
  )
)

; 2
(define (string-append-map xs suffix)
  (map (lambda (i)
         (string-append i suffix))
       xs)
)

; 3
(define (list-nth-mod xs n)
  (cond
    [(< n 0) (error "list-nth-mod: negative number")]
    [(null? xs) (error "list-nth-mod: empty list")]
    [#t (car (list-tail xs (remainder n (length xs))))]
  )
)

; 4
(define (stream-for-n-steps s n)
  (if (equal? n 0)
      null
      (let ([next (s)])
        (cons (car next)
              (stream-for-n-steps (cdr next) (- n 1)))
      )
  )
)

; 5
(define funny-number-stream
  (letrec ([f (lambda (x) 
                (cons (if (equal? (remainder x 5) 0) (- 0 x) x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

; 6
(define cat-then-dog
  (lambda () (cons "cat.jpg"
                   (lambda () (cons "dog.jpg"
                                    cat-then-dog)))))

; 7
(define (stream-add-zero s)
  (let ([next (s)])
    (lambda () (cons (cons 0 (car next))
                     (stream-add-zero (cdr next))))))

; 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons
                 (cons (list-nth-mod xs n) (list-nth-mod ys n))
                 (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

; 9
(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (cond [(equal? n (vector-length vec)) #f]
                      [(pair? (vector-ref vec n)) (if (equal? (car (vector-ref vec n)) v) (vector-ref vec n) (f (+ n 1)))]
                      [#t (f (+ n 1))]))]) (f 0)))

; 10
(define (cached-assoc xs n) (letrec ([cache (make-vector n #f)]
                                     [cache-index 0])
                              (lambda (v)
                                (letrec ([cached (vector-assoc v cache)])
                                  (if cached
                                      (cdr cached)
                                      (letrec ([newval (assoc v xs)])
                                        (begin (set! cache-index (if (= (+ cache-index 1) n) 0 (+ cache-index 1)))
                                               (vector-set! cache cache-index (cons v newval))
                                               newval)))))))

; 11
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([z e1]
              [f (lambda (x) (if (< x z) (f e2) #t))])
       (f e2))
    ]))