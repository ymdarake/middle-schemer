# Middle Schemer - Makefile for Testing
# 
# 使い方:
#   make test            # すべてのテストを実行
#   make test-step-01    # step-01のテストを実行
#   make test-step-02    # step-02のテストを実行
#   make test-step-03    # step-03のテストを実行
#   make help            # ヘルプを表示

.PHONY: test test-all test-step-01 test-step-02 test-step-03 help clean

# Racketコマンドの確認
RACKET := racket
RACKET_VERSION := $(shell $(RACKET) --version 2>/dev/null | head -1 || echo "")

# テスト用のexerciseファイル
STEP_01_EXERCISE := step-01/lexer-exercise.scm
STEP_02_EXERCISE := step-02/parser-exercise.scm
STEP_03_EXERCISE := step-03/evaluator-exercise.scm

# デフォルトターゲット
.DEFAULT_GOAL := help

# すべてのテストを実行
test: test-all

test-all: test-step-01 test-step-02 test-step-03
	@echo ""
	@echo "=========================================="
	@echo "すべてのテストが完了しました"
	@echo "=========================================="

# step-01のテストを実行
test-step-01:
	@echo ""
	@echo "=========================================="
	@echo "step-01: レキサーのテスト"
	@echo "=========================================="
	@if [ -f $(STEP_01_EXERCISE) ]; then \
		$(RACKET) $(STEP_01_EXERCISE) || exit 1; \
	else \
		echo "エラー: $(STEP_01_EXERCISE) が見つかりません"; \
		exit 1; \
	fi

# step-02のテストを実行
test-step-02:
	@echo ""
	@echo "=========================================="
	@echo "step-02: パーサーのテスト"
	@echo "=========================================="
	@if [ -f $(STEP_02_EXERCISE) ]; then \
		$(RACKET) $(STEP_02_EXERCISE) || exit 1; \
	else \
		echo "エラー: $(STEP_02_EXERCISE) が見つかりません"; \
		exit 1; \
	fi

# step-03のテストを実行
test-step-03:
	@echo ""
	@echo "=========================================="
	@echo "step-03: 評価器のテスト"
	@echo "=========================================="
	@if [ -f $(STEP_03_EXERCISE) ]; then \
		$(RACKET) $(STEP_03_EXERCISE) || exit 1; \
	else \
		echo "エラー: $(STEP_03_EXERCISE) が見つかりません"; \
		exit 1; \
	fi

# Racketのバージョンを確認
check-racket:
	@echo "Racketのバージョンを確認しています..."
	@if [ -z "$(RACKET_VERSION)" ]; then \
		echo "エラー: Racketがインストールされていないか、パスが通っていません"; \
		echo "インストール方法: brew install racket (macOS) または https://download.racket-lang.org/"; \
		exit 1; \
	else \
		echo "$(RACKET_VERSION)"; \
	fi

# ヘルプを表示
help:
	@echo "Middle Schemer - テスト用Makefile"
	@echo ""
	@echo "利用可能なコマンド:"
	@echo "  make test            - すべてのテストを実行"
	@echo "  make test-all        - すべてのテストを実行（testと同じ）"
	@echo "  make test-step-01    - step-01のレキサーテストを実行"
	@echo "  make test-step-02    - step-02のパーサーテストを実行"
	@echo "  make test-step-03    - step-03の評価器テストを実行"
	@echo "  make check-racket    - Racketのインストールを確認"
	@echo "  make help            - このヘルプメッセージを表示"
	@echo ""
	@echo "各ステップの個別テスト実行例:"
	@echo "  racket step-01/lexer-exercise.scm whitespace?"
	@echo "  racket step-02/parser-exercise.scm string->symbol-safe"
	@echo "  racket step-03/evaluator-exercise.scm eval-expr-number"
	@echo ""

# クリーンアップ（バックアップファイルを削除）
clean:
	@echo "バックアップファイルを削除しています..."
	@find . -name "*.scm~" -type f -delete 2>/dev/null || true
	@echo "完了しました"

