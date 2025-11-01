#!/bin/bash
# 開発環境セットアップスクリプト
# このスクリプトは、Scheme処理系のインストールを支援します

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
        echo "以下のコマンドでGaucheをインストールできます:"
        echo "  brew install gauche"
        echo ""
        echo "以下のコマンドでChez Schemeをインストールできます:"
        echo "  brew install chezscheme"
        echo ""
    else
        echo "Homebrewが見つかりません。"
        echo "https://brew.sh/ からインストールしてください。"
        echo ""
    fi
fi

# インストール済みのScheme処理系を確認
echo "=== インストール済みのScheme処理系を確認 ==="
echo ""

if command -v gosh &> /dev/null; then
    echo "✓ Gauche: $(gosh --version 2>&1 | head -1)"
else
    echo "✗ Gauche: インストールされていません"
fi

if command -v chezscheme &> /dev/null; then
    echo "✓ Chez Scheme: $(chezscheme --version 2>&1 | head -1)"
else
    echo "✗ Chez Scheme: インストールされていません"
fi

echo ""
echo "=== 推奨処理系 ==="
echo "このプロジェクトでは標準的なSchemeに集中するため、"
echo "GaucheまたはChez Schemeを推奨します。"
echo "（Racketは独自拡張が多いため、このプロジェクトでは使用しません）"
echo ""

echo "=== 動作確認 ==="
echo ""

if command -v gosh &> /dev/null; then
    echo "Gaucheで動作確認を実行します..."
    echo ""
    gosh step-00/hello.scm
elif command -v chezscheme &> /dev/null; then
    echo "Chez Schemeで動作確認を実行します..."
    echo ""
    chezscheme --script step-00/hello.scm
else
    echo "Scheme処理系が見つかりません。"
    echo ""
    echo "以下のコマンドでGaucheをインストールしてください:"
    if [[ "$OS" == "macOS" ]]; then
        echo "  brew install gauche"
    elif [[ "$OS" == "Linux" ]]; then
        echo "  sudo apt-get install gauche"
    else
        echo "  step-00/README.mdを参照してください"
    fi
fi

echo ""
echo "=== セットアップ完了 ==="
echo "詳細は step-00/README.md を参照してください。"

