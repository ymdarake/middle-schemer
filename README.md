# Middle Schemer - Schemeで作る小さな言語

Schemeの学習を兼ねた、段階的に進められる言語実装プロジェクトです。
"The Little Schemer" と "Essentials of Programming Languages" に触発された、実践的な言語作成チュートリアルです。

## プロジェクトの目標

- Schemeを使って小さな言語インタプリタを実装する
- 言語処理系の基礎（字句解析、構文解析、評価）を理解する
- 関数型プログラミングの思想を学ぶ
- 段階的に機能を追加しながら、理解を深める

## プロジェクト構造

```
middle-schemer/
├── README.md           # このファイル
├── step-00/           # ステップ0: 開発環境の構築
├── step-01/           # ステップ1: レキサー（字句解析）
├── step-02/           # ステップ2: パーサー（構文解析）
├── step-03/           # ステップ3: 基本的な評価器
├── step-04/           # ステップ4: 関数定義と適用
├── step-05/           # ステップ5: クロージャとスコープ
├── step-06/           # ステップ6: 条件分岐とブール値
├── step-07/           # ステップ7: 再帰とループ構文
├── step-08/           # ステップ8: マクロとメタプログラミング
└── step-09/           # ステップ9: 統合インタプリタ（完成版）
```

## 進め方

1. **step-00** で開発環境を構築します
2. **step-01** から順番に進めていきます
3. 各ステップの `README.md` を読んで、目的と実装内容を理解します
4. 実装コードを読み、理解を深めます
5. 練習問題に取り組んで、理解を定着させます
6. 次のステップに進む前に、そのステップで学んだことを復習します
7. **step-09** で統合インタプリタを試して、全体像を理解します

## 完成版の使い方

すべてのステップを完了したら、**step-09** の統合インタプリタを実行できます：

```bash
# Gaucheの場合
gosh step-09/interpreter.scm

# Chez Schemeの場合
chezscheme step-09/interpreter.scm
```

文字列のプログラムコードを直接実行できます：

```scheme
(interpret "(+ 1 2)")        ; => 3
(interpret "(define x 10)")  ; 環境にxを定義
(interpret "(+ x 5)")         ; => 15
```

## サンプルプログラム

`examples/` ディレクトリには、統合インタプリタで動くサンプルプログラムがあります：

- `factorial-example.scm`: 階乗の計算
- `fibonacci-example.scm`: フィボナッチ数列

## 穴埋め式練習問題

各ステップには、`*-exercise.scm` という名前の穴埋め式練習問題ファイルがあります：

- `step-01/lexer-exercise.scm`: レキサーの練習問題
- `step-02/parser-exercise.scm`: パーサーの練習問題
- `step-03/evaluator-exercise.scm`: 評価器の練習問題

`???` の部分を実装して、自分でコードを完成させてください。

## 前提知識

- 基本的なSchemeの構文（`define`, `lambda`, `cons`, `car`, `cdr`など）
- S式（括弧で表現される式）の理解
- 再帰の概念

## 実装言語

- **Scheme** (R6RS互換またはR7RS)

### 推奨Scheme処理系

このプロジェクトでは、**標準的なSchemeの学習**に集中するため、以下の処理系を推奨します：

1. **Gauche**（最推奨・初心者向け）
   - 日本語ドキュメントが充実
   - 標準的なSchemeに準拠
   - 学習に適した実装

2. **Chez Scheme**（上級者向け）
   - 非常に高速
   - 実用的な実装

**注意**: RacketはSchemeのスーパーセットで独自拡張が多いため、このプロジェクトでは使用しません。
Racketの学習は別途コンテンツとして扱う予定です。

詳細は **step-00/README.md** を参照してください。

## 参考資料

- [The Little Schemer](https://mitpress.mit.edu/9780262560993/)
- [Essentials of Programming Languages](https://eopl3.com/)
- [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/9780262510875/)

## ライセンス

このプロジェクトは学習目的のため、自由に使用・改変できます。

