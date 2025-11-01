# 実用例とデモプログラム

このディレクトリには、統合インタプリタ（step-09）を使った実用例やデモプログラムを置きます。

## 使い方

まず、step-09の統合インタプリタをロードしてから、これらの例を実行します。

```scheme
;; Gaucheの場合
(load "../step-09/interpreter.scm")

;; 例を実行
(load "factorial.scm")
```

## 実用例

### 1. 基本的な計算例

シンプルな計算式の実行例です。

### 2. 再帰関数の例

階乗、フィボナッチ数列などの再帰関数の実装例です。

### 3. 高階関数の例

関数を引数として受け取る、または返す関数の例です。

## 実装済みのサンプル

- `factorial-example.scm`: 階乗の計算（再帰関数の例）
- `fibonacci-example.scm`: フィボナッチ数列（再帰関数の例）

## 使い方

これらのサンプルは、step-09の統合インタプリタで実行できます：

```scheme
;; Gaucheの場合
(load "../step-09/interpreter.scm")
(load "factorial-example.scm")
(interpret "(define factorial (lambda (n) (if (= n 0) 1 (* n (factorial (- n 1))))))")
(interpret "(factorial 5)")  ; => 120
```

## 今後の追加予定

- `calculator.scm`: 簡単な計算機
- `higher-order.scm`: 高階関数の例
- `closure-example.scm`: クロージャの使用例

