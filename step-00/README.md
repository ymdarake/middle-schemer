# ステップ0: 開発環境の構築

## 目的

Racketの開発環境を構築し、コードを実行できるようにします。

## このステップで行うこと

1. Racketのインストール
2. 動作確認
3. エディタの設定（推奨）
4. 基本的なRacket/Schemeの復習

## 推奨処理系

このプロジェクトでは、**Racket**を使用します。

### Racket

**特徴**:
- Schemeのスーパーセットで、標準的なSchemeの機能に加えて豊富なライブラリを提供
- 教育向けの実装で、ドキュメントが充実
- マクロシステムが強力（syntax-rules、syntax-case等）
- 実用的で、言語処理系の実装に適している
- DrRacketという統合開発環境が利用可能（対話的デバッグが便利）
- パターンマッチングなどの便利な機能も利用可能

**インストール方法**:

macOS (Homebrew):
```bash
brew install racket
```

Linux:
```bash
# パッケージマネージャーからインストール、または
# https://download.racket-lang.org/ からインストーラーをダウンロード
```

Windows:
```bash
# https://download.racket-lang.org/ からインストーラーをダウンロード
```

**動作確認**:
```bash
racket --version
```

**Racketの実行方法**:
```bash
# ファイルを実行
racket hello.scm

# 対話的REPLを起動
racket

# DrRacket IDEを起動（GUI）
drracket
```

## 動作確認

以下のコマンドで動作確認を行います：

```bash
# Racketの場合
racket step-00/hello.scm
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

### DrRacket（推奨）

Racketに標準で含まれる統合開発環境です。初心者にも使いやすく、対話的デバッグが可能です。

起動方法:
```bash
drracket
```

### Visual Studio Code

拡張機能:
- **Magic Racket** (Racket言語サポート)
- **Geiser** (Scheme言語サポート、Racketでも使用可能)

### Emacs

`geiser-mode` をインストール（Racketに対応）:
```elisp
(require 'geiser)
```

### Vim/Neovim

`vim-racket` などのプラグインを使用

## 基本的なRacket/Schemeの復習

このプロジェクトで使用する基本的なRacket/Schemeの構文を確認します。

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

- Racketのバージョンを確認
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

