## step-04: 関数と高階関数の基本

“関数を材料に、関数を作る”練習です。小さな関数を組み合わせて表現力を上げましょう。

### このステップで書くもの（ゴール）
- 関数合成/生成のユーティリティを実装:
  - `compose` / `partial` / `curry2` / `flip` / `apply-n`
- “何を簡潔に書きたいか（狙い）”を意識し、テストで期待挙動を固定

### 1. compose で合成
- `(compose f g)` は `x` に対して `f(g(x))`
- まずは1引数関数で感覚を掴む

### 2. 部分適用 partial
- `partial` は先頭の引数を固定して新しい関数を返す
- 例: `(partial + 3)` は「3を足す関数」に
- Racketのキーワード引数もそのまま `apply` で渡せる

### 3. curry2 / flip
- `curry2`: 2引数関数を段階適用 `((curry2 f) a) b`
- `flip`: 2引数の順序を入れ替え `((flip -) 10 3) => -7`

### 4. apply-n
- `f` を `n` 回合成した関数を作る
- `n=0` は恒等関数（そのまま返す）にするのがきれい

### 5. 練習と動かし方
- `exercise.scm` を実装し、`make -C basic test-step-04` で即時フィードバック
- 合成の左右どちらの結合か、テストで先に期待を固定すると迷いません

### サンプルと結果
```racket
(define inc (λ (x) (+ x 1)))
(define double (λ (x) (* x 2)))
((compose inc double) 3) ; => 7   ; inc(double(3))
((compose double inc) 3) ; => 8   ; double(inc(3))

(define add3 (partial + 3))
(add3 7) ; => 10

(define curried+ (curry2 +))
((curried+ 2) 5) ; => 7

(define rsub (flip -))
(rsub 10 3) ; => -7  ; (- 3 10)

((apply-n inc 3) 10) ; => 13
```

### 参考リンク
- Scheme Standards（関数適用の基本は各版で共通）: [standards.scheme.org](https://standards.scheme.org/)
- TSPL: [Chapter 5 → 5.5 Mapping and Folding](https://www.scheme.com/tspl4/control.html#./control:h6)
- Racket Guide: Procedures（手続き/適用/可変長引数）
