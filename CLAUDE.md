# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## プロジェクト概要

**Middle Schemer**は、Schemeを使用した言語処理系（インタプリタ）の段階的な学習プロジェクトです。字句解析から評価器まで、9つのステップに分けて実装を進めます。

- **実装言語**: Racket (Schemeのスーパーセット)
- **主な目的**: 言語処理系の基礎（レキサー、パーサー、評価器）を理解する
- **学習スタイル**: 各ステップに実装コードと穴埋め式練習問題を用意

## 重要コマンド

### Racketファイルの実行

```bash
# 個別ステップのファイルを実行
racket step-01/lexer.scm

# 統合インタプリタを実行（最終版）
racket step-09/interpreter.scm

# 対話的REPLを起動
racket

# DrRacket IDEを起動（GUI）
drracket
```

### 動作確認

```bash
# 環境セットアップの確認
racket step-00/hello.scm

# バージョン確認
racket --version
```

## プロジェクト構造

各ステップは独立したディレクトリで、以下の構成になっています：

```
step-XX/
├── README.md                # ステップの目的と学習内容
├── <機能名>.scm            # 実装コード
└── <機能名>-exercise.scm   # 穴埋め式練習問題（一部ステップのみ）
```

### ステップの内容

- **step-00**: 開発環境の構築
- **step-01**: レキサー（字句解析） - 文字列をトークンに分割
- **step-02**: パーサー（構文解析） - トークンをS式（抽象構文木）に変換
- **step-03**: 評価器（基本） - S式を評価して結果を返す
- **step-04**: 関数定義と適用 - `lambda`と関数呼び出しの実装
- **step-05**: クロージャとスコープ - 環境と変数束縛の管理
- **step-06**: 条件分岐とブール値 - `if`と真偽値の実装
- **step-07**: 再帰とループ構文 - 再帰関数の実装
- **step-08**: マクロとメタプログラミング - `define-syntax`の実装
- **step-09**: 統合インタプリタ - 全機能を統合した完成版

### サンプルプログラム

`examples/`ディレクトリに、完成した統合インタプリタで動作するサンプルプログラムが含まれています：

- `factorial-example.scm`: 階乗の計算
- `fibonacci-example.scm`: フィボナッチ数列

## コードベースのアーキテクチャ

### 3層構造の言語処理系

1. **レキサー（Lexer）**: `step-01/`
   - 文字列 → トークンリストへの変換
   - 主要関数: `tokenize`, `read-number`, `read-identifier`

2. **パーサー（Parser）**: `step-02/`
   - トークンリスト → S式（抽象構文木）への変換
   - 主要関数: `parse`, `parse-expression`

3. **評価器（Evaluator）**: `step-03/` 以降
   - S式 → 評価結果への変換
   - 主要関数: `eval-expr`, `apply-proc`

### 統合インタプリタ（step-09）

`step-09/interpreter.scm`は、上記3層を統合した完全なインタプリタです：

```scheme
(interpret "(+ 1 2)")        ; => 3
(interpret "(define x 10)")  ; 環境にxを定義
(interpret "(+ x 5)")         ; => 15
```

レキサー、パーサー、評価器のすべてのコードが1ファイルに含まれており、文字列のプログラムコードを直接実行できます。

### Racket固有の記法

- **`#lang racket`**: すべての`.scm`ファイルの先頭に記述（Racketの言語指定）
- **`define`**: 関数・変数の定義
- **`lambda`**: 無名関数
- **`let`ループ**: 名前付きletによる再帰処理（`(let loop ((i 0)) ...)`）
- **文字列操作**: `string-ref`, `string-length`, `list->string`, `string->number`
- **リスト操作**: `cons`, `car`, `cdr`, `reverse`

## 開発時の注意点

### ファイル編集時

- 必ず`#lang racket`をファイルの先頭に保持すること
- 日本語コメントはUTF-8エンコーディングで保存すること
- DrRacketを使う場合は、ファイルを開いた際に自動的に`#lang racket`が認識される

### テストの実行

- 各ステップのファイルを`racket`コマンドで実行すると、サンプル実行結果が表示される
- 穴埋め式練習問題（`*-exercise.scm`）は、`???`の部分を実装してから実行する

### コーディングスタイル

- 関数には必ずdocstringコメントを記述する（例: `"空白文字かどうかを判定"`）
- 再帰処理には`let`ループを使用する（Racketの慣用句）
- リスト操作では`cons`, `car`, `cdr`を基本とする
- 関数名は`kebab-case`（例: `skip-whitespace`, `read-number`）

## よくある作業パターン

### 新しいステップの機能を追加する場合

1. 既存のステップ（特に`step-09`）のコードを読んで構造を理解する
2. 対応するREADME.mdで機能の目的と実装方針を確認する
3. 同様のパターンで新しい関数を実装する
4. 必要に応じて`step-09/interpreter.scm`に統合する

### 練習問題（exercise）に取り組む場合

1. `*-exercise.scm`ファイルを開く
2. `???`の部分を実装する
3. `racket step-XX/*-exercise.scm`で動作確認する
4. 対応する完成版（`step-XX/*.scm`）と比較して理解を深める

### デバッグ時

```scheme
;; デバッグ出力
(displayln "デバッグ情報")
(displayln (format "変数の値: ~a" variable))

;; 型チェック
(number? x)
(list? x)
(symbol? x)
(string? x)
```

## 参考情報

- Racket公式ドキュメント: https://docs.racket-lang.org/
- The Little Schemer: S式の再帰的処理の基礎
- Essentials of Programming Languages: インタプリタ実装の理論的背景
