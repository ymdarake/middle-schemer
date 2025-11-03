## step-00: rackunit と実行環境の確認

Racketで“テストを書いて回す”最小サイクルを体験します。

### このステップで書くもの（ゴール）
- 最小の関数 `square` と、エラーパス確認用 `unimplemented`
- `rackunit` を用いたテストスイートと実行エントリ

### 1. 基本の3点
- 先頭に `#lang racket`（モジュールの宣言）
- 関数定義は `define`
- テストは `rackunit`（`test-suite` / `test-case` / `check-*`）

### 2. 動かしてみる
- `make -C basic test-step-00` を実行
- `square` が通り、`unimplemented` が例外になることを確認
- `module+ main` で“実行時の入口”を用意しておくと便利

### 3. 次へ進むために
- 以降の各ステップでも `make test-step-XX` で即時フィードバック
- 失敗→修正→再実行のテンポに慣れよう

### サンプルと結果
```racket
(square 5) ; => 25
(square -3) ; => 9
(with-handlers ([exn:fail? (λ (e) 'raised)])
  (unimplemented)) ; => 'raised
```

### 参考リンク
- Scheme Standards（歴史と標準の俯瞰）: [standards.scheme.org](https://standards.scheme.org/)
- TSPL: [Chapter 2 Getting Started](https://www.scheme.com/tspl4/start.html), [Section 1.1 Scheme Syntax](https://www.scheme.com/tspl4/intro.html#./intro:h1)
- rackunit: rackunit Manual（Racket公式）
- Racket Guide: Modules / Testing
