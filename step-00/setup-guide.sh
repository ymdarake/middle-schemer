#!/bin/bash
# 開発環境セットアップスクリプト
# このスクリプトは、Racketのインストールを支援します

echo "=== Middle Schemer 開発環境セットアップ ==="
echo ""

# OSの検出
if [[ "$OSTYPE" == "darwin"* ]]; then
    OS="macOS"
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    OS="Linux"
else
    OS="Other"
fi

echo "検出されたOS: $OS"
echo ""

# Homebrewの確認（macOS）
if [[ "$OS" == "macOS" ]]; then
    if command -v brew &> /dev/null; then
        echo "✓ Homebrewが見つかりました"
        echo ""
        echo "以下のコマンドでRacketをインストールできます:"
        echo "  brew install racket"
        echo ""
    else
        echo "Homebrewが見つかりません。"
        echo "https://brew.sh/ からインストールしてください。"
        echo ""
    fi
fi

# インストール済みのRacketを確認
echo "=== インストール済みのRacketを確認 ==="
echo ""

if command -v racket &> /dev/null; then
    echo "✓ Racket: $(racket --version 2>&1 | head -1)"
else
    echo "✗ Racket: インストールされていません"
fi

echo ""
echo "=== 推奨処理系 ==="
echo "このプロジェクトではRacketを使用します。"
echo ""

echo "=== 動作確認 ==="
echo ""

if command -v racket &> /dev/null; then
    echo "Racketで動作確認を実行します..."
    echo ""
    racket step-00/hello.scm
else
    echo "Racketが見つかりません。"
    echo ""
    echo "以下のコマンドでRacketをインストールしてください:"
    if [[ "$OS" == "macOS" ]]; then
        echo "  brew install racket"
    elif [[ "$OS" == "Linux" ]]; then
        echo "  # パッケージマネージャーからインストール、または"
        echo "  # https://download.racket-lang.org/ からインストーラーをダウンロード"
    else
        echo "  https://download.racket-lang.org/ からインストーラーをダウンロード"
    fi
    echo ""
    echo "詳細は step-00/README.md を参照してください。"
fi

echo ""
echo "=== セットアップ完了 ==="
echo "詳細は step-00/README.md を参照してください。"

