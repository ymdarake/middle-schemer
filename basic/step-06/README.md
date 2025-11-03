## step-06: マクロ入門（構文テンプレート）

このステップでは、式（コード）をデータとして扱い、テンプレートで「別のコード」に展開する“マクロ”を学びます。まずは安全に使えるテンプレート指向の書き方から始め、必要に応じて柔軟な書き方へ段階的に進みます。

### このステップで書くもの（ゴール）
- `sum` マクロ: 可変長の数を受け取り、`(+ ...)` に展開（引数なしは `0`）
- `when-let` マクロ: 一時変数の束縛と条件付き実行を一つの構文にまとめる
- `->` マクロ: 値を左から右へ関数に通す“スレッディング”（可読性を上げる糖衣）

それぞれ「どんな不便を解消するための構文か」を意識して設計します。

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

### when-let が役に立つ場面
- 失敗するかもしれない“変換”にガードを付けたい（例: 文字列→数値）
- オプショナルな値（ハッシュ/辞書検索、環境変数など）を安全に使いたい
- 正規表現マッチの結果（`#f` かマッチ配列）をパターン化したい

### サンプルと結果
```racket
; sum
(sum)        ; => 0
(sum 1 2 3)  ; => 6

; when-let: 文字列→数値の安全変換
(when-let (n (string->number "42"))
  (+ n 1))  ; => 43
(when-let (n (string->number "oops"))
  (+ n 1))  ; => (void)

; when-let: ハッシュからのオプショナル取得
(define env (hash 'token "abc"))
(when-let (t (hash-ref env 'token #f))
  (string-length t)) ; => 3
(when-let (t (hash-ref env 'missing #f))
  (string-length t)) ; => (void)

; when-let: 正規表現マッチの活用（cadrで第1キャプチャを取り出す）
(when-let (m (regexp-match #rx"^user:(.+)$" "user:alice"))
  (cadr m)) ; => "alice"
(when-let (m (regexp-match #rx"^user:(.+)$" "guest"))
  (cadr m)) ; => (void)

; -> threading
(define (f x a) (+ x a))
(define (g x b) (* x b))
(-> 1 (f 2) (g 3)) ; => 9
(-> "a" (string-append "b") (string-append "c")) ; => "abc"
```

### 参考リンク
- Scheme Standards（R5RS/R6RS/R7RS まとめ）: [standards.scheme.org](https://standards.scheme.org/)
  - R5RS（`syntax-rules` マクロの出発点）
  - R6RS（`syntax-case` など、より表現力のあるマクロ）
  - R7RS（小さなコア＋ライブラリ体系）
- TSPL（The Scheme Programming Language, 4th Ed.）: [Chapter 8 Syntactic Extension](https://www.scheme.com/tspl4/syntax.html)
  - 8.2 Syntax-Rules Transformers / 8.3 Syntax-Case Transformers
- Racket Guide（Macrology 一般解説）: Macros（Racket公式）
