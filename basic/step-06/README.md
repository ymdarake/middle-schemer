## step-06: マクロ入門（構文テンプレート）

このステップでは、式（コード）をデータとして扱い、テンプレートで「別のコード」に展開する“マクロ”を学びます。まずは安全に使えるテンプレート指向の書き方から始め、必要に応じて柔軟な書き方へ段階的に進みます。

### 0. まず前提を知る（超要約）
- マクロは「コンパイル前にコードを変形する仕組み」。実行時ではなく展開時に働く
- Scheme/Racketのマクロは“衛生的（hygienic）”で、変数の取り違いが起きにくい
- 歴史と標準:
  - R5RS: `syntax-rules` によるテンプレート型マクロ（入門に最適） [R5RS 解説/仕様 | standards.scheme.org](https://standards.scheme.org/)
  - R6RS: `syntax-case` による手続き的・柔軟なマクロ、条件システム等が充実 [R6RS 概要 | standards.scheme.org](https://standards.scheme.org/)
  - R7RS: 小さなコア＋ライブラリ体系、シンプル志向 [R7RS 概要 | standards.scheme.org](https://standards.scheme.org/)

### 1. 一番簡単な型: `define-syntax-rule`
固定パターン→固定テンプレートの“置換”を書けます（R5RS系の考え方）。

ステップ課題の例（概念図）
- `sum`: `(sum 1 2 3)` ⇒ `(+ 1 2 3)`、空は0
- `when-let`: `(when-let (x expr) body ...)` ⇒ `(let ([x expr]) (when x body ...))`

ポイント
- パターンとテンプレートは「式の形」を対応付ける
- 変数の衝突は自動で避けられる（衛生的）ので、`x`などの名前を安心して使える

参考: R5RSのマクロ（`syntax-rules`）はテンプレート型の思想 [R5RS | standards.scheme.org](https://standards.scheme.org/)，TSPL: [Chapter 8 Syntactic Extension → 8.2 Syntax-Rules Transformers](https://www.scheme.com/tspl4/syntax.html#./syntax:h8)

### 2. もう一歩: スレッディング `->` を段階展開
ゴール: `(-> x (f a) (g b))` ⇒ `(g (f x a) b)`

段階的な考え方
1) 入力が1フォームのとき: `(-> x)` ⇒ `x`
2) 先頭フォームを適用して“次の入力”にして再帰: `(-> x (f a) more ...)` ⇒ `(-> (f x a) more ...)`

ここで必要になるのが `syntax-case`（R6RS以降の手続き的マクロ）。テンプレートだけでは表現しにくい“段階展開”や“条件分岐”が簡潔に書けます。

参考: R6RSの `syntax-case`（より柔軟なマクロ） [R6RS | standards.scheme.org](https://standards.scheme.org/)，TSPL: [Chapter 8 → 8.3 Syntax-Case Transformers](https://www.scheme.com/tspl4/syntax.html#./syntax:h9)

### 3. 失敗しがちなポイント（よくある落とし穴）
- 実行時の値で分岐したい ⇒ それは「関数」で書く（マクロは展開時）
- `,@` を使いたくなる ⇒ マクロ内でリストを“展開”するのはテンプレート側の話。生成結果の形を常に意識する
- 無理にマクロにしない ⇒ 単なる関数合成で済む場合は関数でOK。マクロは“構文を作る”ときに使う

### 4. 小さなチェックリスト
- テンプレートだけで書ける？ ⇒ `define-syntax-rule` で始める
- 形が分岐・再帰展開する？ ⇒ `syntax-case` を検討
- 展開後の式は正しいRacketコード？ ⇒ rackunitでテスト

### 5. 実行方法
- 課題（exercise）を編集してテスト: `make -C basic test-step-06`
- 正解（solution）を参考に差分で学ぶ

### 参考リンク
- Scheme Standards（R5RS/R6RS/R7RS まとめ）: [standards.scheme.org](https://standards.scheme.org/)
  - R5RS（`syntax-rules` マクロの出発点）
  - R6RS（`syntax-case` など、より表現力のあるマクロ）
  - R7RS（小さなコア＋ライブラリ体系）
- TSPL（The Scheme Programming Language, 4th Ed.）: [Chapter 8 Syntactic Extension](https://www.scheme.com/tspl4/syntax.html)
  - 8.2 Syntax-Rules Transformers / 8.3 Syntax-Case Transformers
- Racket Guide（Macrology 一般解説）: Macros（Racket公式）
