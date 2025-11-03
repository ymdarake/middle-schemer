## step-05: クォートと準クォート（'  `  ,  ,@）

S式（リスト）を“データとして扱う”基礎を身につけます。これはマクロ（次ステップ）への登竜門です。

### 1. クォート `'`
- `'(a b c)` は、評価せずそのままの“データ”を指します
- `'(+)` は「+を先頭要素にもつリスト」という“データ”であり、計算ではありません

手を動かす
- `'(1 2 3)` と `(list 1 2 3)` の結果を比べる
- `quote-list` を実装して、リストを“作る”感覚を掴む

### 2. 準クォート `` ` `` と `,` / `,@`
- `` `(a ,x ,@xs) `` で、テンプレートの一部を評価して埋め込めます
  - `,x` は1つの値を差し込む
  - `,@xs` はリストを“展開（スプライス）”する

手を動かす
- `make-call`: `(make-call '+ '(1 2 3))` ⇒ `'(+ 1 2 3)` を生成
- `let-binding`: ``(let ((,name ,expr)) ,body)`` の形をテンプレートで作る
- `alist->hash-expr`: `((a 1) (b 2))` から `(hash 'a 1 'b 2)` を作る（`,@` の練習）

コツ
- 「いま作っているのは“式”のリスト」= 「コードを表すデータ」
- 生成結果が有効なRacketの式になっているか、常に目で確認

### 3. なぜ学ぶの？（マクロの準備）
- 準クォートは“コードの型”をテンプレートで表現する力を与えます
- 次ステップのマクロで、このテンプレートに“展開ロジック”を与えます

### 参考リンク
- Scheme Standards（R5RS/R6RS/R7RS 概観）: [standards.scheme.org](https://standards.scheme.org/)
  - R5RS: Quotation（`syntax-rules` の基盤となる考え）
- TSPL: [6.1 Constants and Quotation](https://www.scheme.com/tspl4/objects.html#./objects:h1)
- Racket Guide: Quasiquote/Quote（Racketのテンプレート作法）
