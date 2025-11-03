## step-00: rackunit と実行環境の確認

このステップでは、Racketの最小実行とテストフレームワーク rackunit の使い方を体験します。

### 学ぶこと
- **モジュールの基本**: `#lang racket`、`(module+ main ...)` のエントリポイント
- **関数定義**: `define`
- **テスト**: `rackunit` の `test-suite` / `test-case` / `check-equal?` / `check-exn`

### 試してみよう
- `square` のテストが通ることを確認
- `unimplemented` は例外になる（`TODO` を実装してテストを書き換えてもOK）
- 実行コマンド: `make -C basic test-step-00`

### 公式ドキュメント
- Racket Guide: [Modules](https://docs.racket-lang.org/guide/modules.html)
- rackunit: [rackunit Manual](https://docs.racket-lang.org/rackunit/)
- Racket Reference: [Definitions](https://docs.racket-lang.org/reference/define.html)

### ヒント
- `rackunit/text-ui` を `require` すると `run-tests` でテストを実行できます。
- エントリポイントは `(module+ main ...)` を使うとテストと同居させやすいです。
