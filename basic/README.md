# 基礎から学ぶ Racket 練習プロジェクト（basic）

本プロジェクトは、Racket/Scheme の基礎をステップバイステップで練習しながら、テストを通して身につけることを目的としています。`exercise.scm` に実装を書き、`racket` で実行すると付属の rackunit テストが走ります。

## 構成

```
basic/
  ├── Makefile
  ├── README.md
  ├── step-00/  # 環境確認・rackunitの使い方
  ├── step-01/  # cons/pair/list の基本
  ├── step-02/  # リストユーティリティ（length, append, map, filter, fold）
  └── step-03/  # 文字列ユーティリティ（substring, 検索, 結合）
```

今後は `step-04` 以降で `cond`/`let`、再帰パターン、マクロ入門・応用へ拡張予定です。

## 実行例

```
make -C basic test            # すべてのステップのテスト
make -C basic test-step-01    # step-01 のみ

# 直接実行も可
racket basic/step-01/exercise.scm
```

## 学習方針
- まずテストを読み、期待される挙動を理解する
- `???` の箇所を実装で置き換える（未実装のままでもテストは意味のある失敗メッセージを出します）
- 実装は読みやすさ重視（名前・分割・境界条件）


