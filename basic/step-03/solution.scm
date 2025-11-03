#lang racket

(require rackunit rackunit/text-ui)

;; 解答: step-03

(define (take-str s n)
  (substring s 0 (min n (string-length s))))

(define (drop-str s n)
  (let* ([len (string-length s)]
         [k (min n len)])
    (substring s k len)))

(define (find-char s ch)
  (let loop ([i 0] [len (string-length s)])
    (cond
      [(= i len) #f]
      [(char=? (string-ref s i) ch) i]
      [else (loop (add1 i) len)])))

(define (join-with sep ss)
  (cond
    [(null? ss) ""]
    [else (let loop ([rest (cdr ss)] [acc (car ss)])
            (if (null? rest)
                acc
                (loop (cdr rest) (string-append acc sep (car rest)))))]))

(define tests
  (test-suite
   "step-03-solution"
   (test-case "string utils"
     (check-equal? (string-length "hello") 5)
     (check-equal? (take-str "hello" 2) "he")
     (check-equal? (drop-str "hello" 2) "llo")
     (check-equal? (find-char "hello" #\l) 2)
     (check-equal? (find-char "hello" #\z) #f)
     (check-equal? (join-with "-" '("a" "b" "c")) "a-b-c"))))

(module+ main
  (displayln "=== step-03 solution ===")
  (run-tests tests))
