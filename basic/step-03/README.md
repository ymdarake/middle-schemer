## step-03: 文字列ユーティリティ

文字列の分解・結合・探索を“小さな関数”に切り出す練習です。

### 1. 分解（take/drop）
- `take-str s n`: 先頭から `n` 文字（範囲外は切り詰め）
- `drop-str s n`: 先頭から `n` 文字を捨てた残り
- まずは `substring` と `string-length` に慣れる

### 2. 探索（find-char）
- 見つかったらインデックス、なければ `#f`
- ループ（名前付き`let`）で左から走査するのが基本

### 3. 結合（join-with）
- 区切り文字 `sep` で `{ "a" "b" "c" }` を `"a-b-c"` に
- 先頭要素から組み立て、残りをループで結合

### 実行
- `make -C basic test-step-03` で即時フィードバック

### 参考リンク
- Scheme Standards（文字列は各版で基本的に共通）: [standards.scheme.org](https://standards.scheme.org/)
- TSPL: [6.8 Strings](https://www.scheme.com/tspl4/objects.html#./objects:h8)
- Racket Reference: Strings（`substring`/`string-ref` など）
