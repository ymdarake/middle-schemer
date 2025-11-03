## step-03: 文字列ユーティリティ

このステップでは、文字列の分解・結合・探索といった基本操作を自作します。

### 学ぶこと
- **基本操作**: `string-length` / `string-append` / `substring` / `string-ref`
- **分解・結合**: `take-str` / `drop-str` / `join-with`
- **探索**: `find-char`（見つからなければ `#f` を返す）

### 実行
- `make -C basic test-step-03`

### 公式ドキュメント
- Racket Guide: [Strings](https://docs.racket-lang.org/guide/strings.html)
- Racket Reference: [Strings](https://docs.racket-lang.org/reference/strings.html)

### ヒント
- Racketの文字列はイミュータブル。部分列の取り出しは `substring` を活用。
- 連結が多い場合は、中間リストに貯めて最後に `string-join` も検討しましょう。
