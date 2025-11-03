## step-01: ペアとリストの基本（cons / car / cdr）

このステップでは、S式の基礎となる「ペア」と「リスト」の操作を学びます。

### 学ぶこと
- **ペア（ドット対）**: `cons` で作る2要素の組
- **基本操作**: `car`（先頭）/ `cdr`（残り）
- **リスト**: `'()`（空リスト）と `(list 1 2 3)`、`append` で連結
- **判定**: `pair?` と `list?`

### 練習のねらい
- `pair-head` / `pair-tail` / `prepend` を実装して `rackunit` を通す
- ドット対（`(cons 1 2)`）とリスト（`(cons 1 '(2 3))`）の違いを体験

### 実行
- `make -C basic test-step-01`

### 公式ドキュメント
- Racket Guide: [Pairs and Lists](https://docs.racket-lang.org/guide/pairs.html)
- Racket Reference: [Pairs](https://docs.racket-lang.org/reference/pairs.html)

### ヒント
- `'(1 2 3)` は構文糖衣で、内部的には `cons` の連なりです。
- 末尾に要素を足すより、先頭に `cons` して最後に `reverse` する方が効率的な場合があります。
