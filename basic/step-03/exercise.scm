#lang racket

(require rackunit rackunit/text-ui)

;; 目的: 文字列ユーティリティの実装練習

;; TODO: 実装してください
(define (take-str s n)
  (error "TODO: take-str を実装してください"))

(define (drop-str s n)
  (error "TODO: drop-str を実装してください"))

(define (find-char s ch)
  (error "TODO: find-char を実装してください"))

;; 参考: string-append を自前実装する場合
;; make-string: 可変文字列を作る（書き換え可能な文字列）
;;   (make-string 5) => "\0\0\0\0\0"
;; string-set!: 文字列の位置を書き換える（破壊的操作、! は破壊的を示す）
;;   (string-set! s 0 #\H) => s[0] を 'H' に変更
;; 例: (my-string-append "Hello" "World")
;;   1. result = (make-string 10) で空の箱を作る
;;   2. loop1 で result[0..4] に "Hello" を書き込む
;;   3. loop2 で result[5..9] に "World" を書き込む
;;   4. 結果: "HelloWorld"
(define (my-string-append s1 s2)
  (let* ([len1 (string-length s1)]
         [len2 (string-length s2)]
         [result (make-string (+ len1 len2))])
    ; s1 をコピー
    (let loop1 ([i 0])
      (when (< i len1)
        (string-set! result i (string-ref s1 i))
        (loop1 (+ i 1))))
    ; s2 をコピー
    (let loop2 ([i 0])
      (when (< i len2)
        (string-set! result (+ len1 i) (string-ref s2 i))
        (loop2 (+ i 1))))
    result))

(define (join-with sep ss)
  (error "TODO: join-with を実装してください"))


(define tests
  (test-suite
   "step-03"
   (test-case "string utils"
     (check-equal? (string-length "hello") 5)
     (check-equal? (take-str "hello" 2) "he")
     (check-equal? (drop-str "hello" 2) "llo")
     (check-equal? (find-char "hello" #\l) 2)
     (check-equal? (find-char "hello" #\z) #f)
     (check-equal? (join-with "-" '("a" "b" "c")) "a-b-c"))))

(module+ main
  (displayln "=== step-03: 文字列ユーティリティ ===")
  (run-tests tests))


