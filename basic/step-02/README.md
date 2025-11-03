## step-02: リストユーティリティと再帰

リスト処理を通して、自然再帰→末尾再帰→名前付き`let`の流れを体験します。

### 1. length / append
- `length`: 末尾に着くまで `n` を増やす（自然再帰でもOK）
- `append`: 先頭から `cons` で積み直しながら `ys` を最後に繋ぐ

### 2. map / filter
- `map f`: 各要素に `f` を適用して新リストを作る
- `filter p`: `p` が真の要素だけを残す（`acc` に貯めて最後に `reverse`）

### 3. foldl
- 左畳み込み: `(foldl f init '(a b c))` は `f(f(f init a) b) c)`
- 多くの関数は fold で記述できる（発想の核）

### 実行
- `make -C basic test-step-02`

### 参考リンク
- Scheme Standards: 反復とリストの基本（各版共通） [standards.scheme.org](https://standards.scheme.org/)
- TSPL: [5.4 Recursion and Iteration](https://www.scheme.com/tspl4/control.html#./control:h5), [5.5 Mapping and Folding](https://www.scheme.com/tspl4/control.html#./control:h6)
- Racket Guide: Loops / Pairs and Lists
