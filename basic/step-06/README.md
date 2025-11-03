## step-06: マクロ入門（構文テンプレート）

このステップでは、構文変換で小さな糖衣構文を作り、Racketのマクロの雰囲気を掴みます。

### 学ぶこと
- **`define-syntax-rule`**: パターン→テンプレートで簡単な変換を記述
- **`syntax-case`**: 柔軟な分岐や再帰的展開が必要な場合に使う
- **衛生（hygiene）**: マクロ内の識別子衝突を避ける仕組み（概要）

### 練習のお題
- `sum`: `(sum 1 2 3)` ⇒ `(+ 1 2 3)`（空なら0）
- `when-let`: `(when-let (x expr) body ...)` ⇒ `(let ([x expr]) (when x body ...))`
- `->`（スレッディング）: `(-> x (f a) (g b))` ⇒ `(g (f x a) b)`

### 実行
- `make -C basic test-step-06`

### 公式ドキュメント
- Racket Guide: [Macros](https://docs.racket-lang.org/guide/macros.html)
- Racket Reference: [define-syntax, syntax-case など](https://docs.racket-lang.org/reference/syntax-model.html)
- Macro風味の入門: [Syntax Patterns](https://docs.racket-lang.org/guide/pattern-macros.html)

### ヒント
- `define-syntax-rule` は固定パターンに最適。複雑化してきたら `syntax-case` を検討。
- 展開結果は「通常のRacketコード」。テストで展開後の振る舞いを確認しましょう。
