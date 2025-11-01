# ステップ9: 統合インタプリタ

## 目的

これまでに実装したレキサー、パーサー、評価器を統合し、文字列のプログラムコードを実行できる完全なインタプリタを作成します。

## 学ぶこと

- 言語処理系の全体像の理解
- レキサー、パーサー、評価器の統合方法
- エラーハンドリングの実装
- 実用的なインタプリタの構造

## 実装内容

以下のように、文字列のプログラムコードを直接実行できるようになります：

```scheme
(interpret "(+ 1 2)")  ; => 3
(interpret "(define x 10)")  ; => 環境にxを定義
(interpret "(+ x 5)")  ; => 15
```

または、REPL（Read-Eval-Print Loop）として：

```scheme
(repl)  ; インタラクティブにコードを実行
```

## 統合の流れ

1. **レキサー（step-01）**: 文字列 → トークンリスト
2. **パーサー（step-02）**: トークンリスト → 構文木（AST）
3. **評価器（step-03〜08）**: 構文木 → 値

## 実装のポイント

1. 各ステップの実装を組み合わせる
2. 環境をグローバルに管理する
3. エラーハンドリングを統一する
4. ユーザーフレンドリーなインターフェースを提供

## 実行方法

```bash
# Gaucheの場合
gosh step-09/interpreter.scm

# Chez Schemeの場合
chezscheme step-09/interpreter.scm
```

## 完成版としての使い方

このステップで、小さなSchemeインタプリタが完成します。
以下の機能が使えます：

- 基本的な数値演算（+, -, *, /）
- 変数定義（define）
- 関数定義（lambda）
- 条件分岐（if）
- 比較演算（=, <, >, <=, >=）
- 論理演算（and, or, not）
- 局所変数（let, letrec）
- 再帰関数
- マクロ（quote, quasiquote, unquote）
- 高度なマクロ（define-syntax, syntax-rules）

### マクロの使用例

```scheme
;; マクロを定義
(interpret "(define-syntax unless (syntax-rules () ((_ condition body) (if (not condition) body))))")

;; マクロを使用
(interpret "(unless #f (display \"Hello\"))")  ; => "Hello"を表示
```

## 次のステップ

ここまでで基本は完成です！さらに拡張したい場合：

- リスト操作（cons, car, cdr, listなど）
- 文字列操作
- 入出力（read, write, display）
- エラーハンドリングの強化
- REPLの機能拡張（履歴、補完など）

