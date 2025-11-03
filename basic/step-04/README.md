## step-04: 関数と高階関数の基本

このステップでは、関数を組み合わせる基本テクニックを学びます。

### 学ぶこと
- **compose**: `(compose f g)` は `x` に対し `f(g(x))`
- **partial**: 先頭引数を一部だけ束縛して新しい関数を作る
- **curry2**: 2引数関数を段階適用できる形に変換
- **flip**: 2引数関数の引数順を入れ替える
- **apply-n**: 関数を `n` 回合成した関数を作る

### 実行
- `make -C basic test-step-04`

### 公式ドキュメント
- Racket Guide: [Procedures](https://docs.racket-lang.org/guide/procedures.html)
- Racket Reference: [Procedure Application](https://docs.racket-lang.org/reference/procedures.html)

### ヒント
- 可変長引数を扱うときは `define (f . args)` の形が便利です。
- 合成は左結合・右結合のどちらで期待しているかをテストで先に決めると迷いません。
