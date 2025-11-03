## step-02: リストユーティリティと再帰

このステップでは、リストを処理する基本的な関数を自作し、再帰・名前付きlet・高階関数の下地を作ります。

### 学ぶこと
- **基本関数の自作**: `length` / `append` / `map` / `filter` / `foldl`
- **再帰の型**: 自然再帰・末尾再帰、蓄積変数の導入
- **名前付き let**: 反復処理を関数化せず手早く書く

### 実行
- `make -C basic test-step-02`

### 公式ドキュメント
- Racket Guide: [Iteration and Recursion](https://docs.racket-lang.org/guide/loops.html)
- Racket Guide: [Pairs and Lists](https://docs.racket-lang.org/guide/pairs.html)
- Racket Reference: [for/fold などの反復](https://docs.racket-lang.org/reference/for.html)

### ヒント
- `my-append` は `xs` を前からたどって `cons` で積み上げるのが基本です。
- `my-foldl` は畳み込み。`foldl` でほかの関数を表現できると理解が深まります。
