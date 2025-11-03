## step-05: クォートと準クォート（'  `  ,  ,@）

このステップでは、S式をデータとして扱い、テンプレート的に式を生成する方法を学びます。

### 学ぶこと
- **クォート**: `'(a b c)` はシンボル列をそのままデータとして扱う
- **準クォート**: `` `(a ,x ,@xs) `` のように、テンプレート内で一部を評価
  - `,` は1つの値を埋め込み、`,@` はリストを展開（スプライス）
- **テンプレート構築**: 呼び出し式や `let` などのS式を組み立てる

### 実行
- `make -C basic test-step-05`

### 公式ドキュメント
- Racket Guide: [Quotation](https://docs.racket-lang.org/guide/quote.html)
- Racket Reference: [Quote and Quasiquote](https://docs.racket-lang.org/reference/quote.html)

### ヒント
- シンボルを式の先頭に置けば「関数/演算子呼び出し」を表現できます（例: `'(+ 1 2)`）。
- `,@` はリスト以外に使うとエラー。データ形の前提をテストで固定しましょう。
