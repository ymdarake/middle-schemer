## step-01: ペアとリストの基本（cons / car / cdr）

S式の骨格である“ペア（ドット対）”と“リスト”を手で作り、分解してみます。

### 1. ペアを触る
- `cons` は2つの箱をつなぐ（`(cons 1 2)` はリストではなく“対”）
- `car` が先頭、`cdr` が残り

### 2. リストを作る/判定する
- `'(1 2 3)` は糖衣。内部的には `cons` の連なり
- `list?` は“適切な末尾の `null` を持つか”を判定

### 3. 練習
- `pair-head` / `pair-tail` を `car`/`cdr` で実装
- `prepend` で先頭に要素を付け足す

### 実行
- `make -C basic test-step-01`

### 参考リンク
- Scheme Standards: Pairs and Lists（古典から現代まで共通の核） [standards.scheme.org](https://standards.scheme.org/)
- TSPL: [6.3 Lists and Pairs](https://www.scheme.com/tspl4/objects.html#./objects:h3)
- Racket Reference: Pairs / Lists
