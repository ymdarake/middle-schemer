# ステップ0: 開発環境の構築

## 目的

Schemeの開発環境を構築し、コードを実行できるようにします。

## このステップで行うこと

1. Scheme処理系のインストール
2. 動作確認
3. エディタの設定（推奨）
4. 基本的なSchemeの復習

## 推奨Scheme処理系

このプロジェクトでは、**標準的なSchemeの学習**に集中するため、純粋なScheme処理系を推奨します。

### 1. Gauche（最推奨・初心者向け）

**特徴**:
- 日本語ドキュメントが充実
- 標準的なScheme（R7RS準拠）に準拠
- シンプルで学習に適している
- 開発が活発
- 標準ライブラリが豊富

**インストール方法**:

macOS (Homebrew):
```bash
brew install gauche
```

Linux (apt):
```bash
sudo apt-get install gauche
```

Windows:
[公式サイト](https://practical-scheme.net/gauche/)からインストーラーをダウンロード

**動作確認**:
```bash
gosh --version
```

### 2. Chez Scheme（上級者向け）

**特徴**:
- 非常に高速
- 実用的な実装
- 商用利用も可能

**インストール方法**:

macOS (Homebrew):
```bash
brew install chezscheme
```

Linux:
```bash
# ソースからビルド、またはリポジトリから
# 詳しくは https://github.com/cisco/ChezScheme
```

**動作確認**:
```bash
chezscheme --version
```

### 3. その他の選択肢

**Chicken Scheme**:
- 実用的な実装
- 多くのライブラリが利用可能

**MIT Scheme**:
- 教育機関でよく使用される
- SICP（Structure and Interpretation of Computer Programs）で使用

### なぜRacketを使わないのか？

RacketはSchemeのスーパーセットで、独自の拡張機能が多く含まれています。
このプロジェクトは**標準的なSchemeの理解**に焦点を当てているため、Racketは使用しません。

- Racketは独自の構文やライブラリが多い
- 標準的なSchemeとは異なる動作をする場合がある
- このプロジェクトの目的（言語処理系の基礎理解）には、標準的なSchemeの方が適している

**Racketの学習は別途コンテンツとして扱う予定です。**

## 動作確認

以下のコマンドで動作確認を行います：

```bash
# Gaucheの場合（推奨）
gosh step-00/hello.scm

# Chez Schemeの場合
chezscheme step-00/hello.scm
```

正常に動作すれば、以下のような出力が表示されます：

```
=== Scheme開発環境の動作確認 ===
Hello, Scheme!
1 + 2 = 3
再帰テスト: 10
✓ すべてのテストが成功しました
```

## エディタの設定（推奨）

### Visual Studio Code

拡張機能:
- **Scheme** または **Geiser** (Scheme言語サポート)
- **Lisp** (Lisp系言語のサポート)

### Emacs

`geiser-mode` をインストール:
```elisp
(require 'geiser)
```

### Vim/Neovim

`vim-scheme` などのプラグインを使用

## 基本的なSchemeの復習

このプロジェクトで使用する基本的なSchemeの構文を確認します。

### 基本的な構文

```scheme
;; 変数の定義
(define x 10)
(define y 20)

;; 関数の定義
(define (add a b)
  (+ a b))

;; lambda式
(define multiply (lambda (a b) (* a b)))

;; リスト操作
(define lst '(1 2 3 4 5))
(car lst)      ; => 1 (最初の要素)
(cdr lst)      ; => (2 3 4 5) (残りの要素)
(cons 0 lst)   ; => (0 1 2 3 4 5) (先頭に追加)

;; 条件分岐
(if (> 5 3)
    "大きい"
    "小さい")

;; 再帰
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
```

### よく使う関数

- `cons`, `car`, `cdr`: リスト操作
- `list`: リストの作成
- `length`: リストの長さ
- `map`, `filter`, `fold`: 高階関数
- `eq?`, `equal?`: 等価性の判定
- `string->symbol`, `symbol->string`: シンボルと文字列の変換

## トラブルシューティング

### コマンドが見つからない場合

- PATH環境変数にScheme処理系のパスが含まれているか確認
- インストールが正しく完了しているか確認

### 実行時にエラーが出る場合

- Schemeのバージョンを確認（R6RSまたはR7RS互換が必要）
- ファイルの文字エンコーディングがUTF-8になっているか確認

### macOSでの注意

Homebrewを使用する場合：
```bash
brew install gauche
```

もし `gosh` コマンドが見つからない場合：
```bash
export PATH="/opt/homebrew/bin:$PATH"  # Apple Silicon Macの場合
# または
export PATH="/usr/local/bin:$PATH"      # Intel Macの場合
```

## 次のステップ

開発環境の構築が完了したら、**step-01** に進み、レキサー（字句解析）の実装を始めましょう！

