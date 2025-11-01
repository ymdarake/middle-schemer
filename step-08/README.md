# ステップ8: マクロとメタプログラミング（オプション）

## 目的

マクロシステムを実装し、コードを生成するメタプログラミングの基礎を学びます。
これにより、言語を拡張する能力を獲得します。

## 学ぶこと

- マクロの概念
- コード生成とコード変換
- 構文抽象（syntactic abstraction）
- 準クォート（quasiquote）

## 実装内容

以下のようなコードが評価できるようになります：

```scheme
;; quote: 式を評価せずにそのまま返す
(quote (+ 1 2))  ; => (+ 1 2)  （評価されない）

;; quasiquote: 部分的に評価できるquote
(quasiquote (1 (unquote (+ 2 3)) 4))  ; => (1 5 4)

;; unquote: quasiquote内で値に展開
(quasiquote ((unquote (* 2 3)) (unquote (+ 1 2))))  ; => (6 3)
```

## 実装のポイント

1. **quote**: 式を評価せずにそのまま返す（環境を使わない）
2. **quasiquote**: リストを再帰的に展開し、`unquote` を見つけたらその部分だけ評価
3. **unquote**: `quasiquote` 内でのみ有効で、その引数を評価する
4. マクロ展開の基礎: コード変換の仕組みを理解する

## 実行方法

```bash
# Gaucheの場合
gosh step-08/macros.scm

# Chez Schemeの場合
chezscheme step-08/macros.scm
```

## 注意

このステップは上級者向けです。基本的なインタープリターの実装が理解できていることが前提です。

この実装では基本的な `quote`, `quasiquote`, `unquote` のみを実装しています。
実際のSchemeでは、より高度なマクロシステム（`syntax-rules`, `syntax-case` など）が提供されます。

## 次のステップ

ここまで実装すると、小さなSchemeインタープリターの完成です！
さらに拡張する場合は、以下を実装できます：
- リスト操作（`cons`, `car`, `cdr`, `list` など）
- 文字列操作
- 入出力（`read`, `write`）
- エラーハンドリングの強化

