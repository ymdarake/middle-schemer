# Middle Schemer - Makefile for Testing
# 
# 使い方:
#   make test            # すべてのテストを実行
#   make test-step-01    # step-01のテストを実行
#   make test-step-02    # step-02のテストを実行
#   make test-step-03    # step-03のテストを実行
#   make help            # ヘルプを表示

.PHONY: test test-all test-step-01 test-step-02 test-step-03 help clean summary

# Racketコマンドの確認
RACKET := racket
RACKET_VERSION := $(shell $(RACKET) --version 2>/dev/null | head -1 || echo "")

# テスト用のexerciseファイル
STEP_01_EXERCISE := step-01/lexer-exercise.scm
STEP_02_EXERCISE := step-02/parser-exercise.scm
STEP_03_EXERCISE := step-03/evaluator-exercise.scm

# デフォルトターゲット
.DEFAULT_GOAL := help

# テスト結果を保存する一時ディレクトリ
TEST_RESULTS_DIR := .test-results
$(shell mkdir -p $(TEST_RESULTS_DIR))

# すべてのテストを実行
test: test-all

test-all: test-step-01 test-step-02 test-step-03
	@echo ""
	@echo "=========================================="
	@echo "テスト結果サマリ"
	@echo "=========================================="
	@$(MAKE) --no-print-directory summary
	@echo ""

# step-01のテストを実行
test-step-01:
	@echo ""
	@echo "=========================================="
	@echo "step-01: レキサーのテスト"
	@echo "=========================================="
	@if [ -f $(STEP_01_EXERCISE) ]; then \
		$(RACKET) $(STEP_01_EXERCISE) 2>&1 | tee $(TEST_RESULTS_DIR)/step-01.txt | grep -v "^$$" | tail -10; \
		$(RACKET) $(STEP_01_EXERCISE) 2>&1 | grep -E "^[0-9]+ success|failure|error|test\(s\)" > $(TEST_RESULTS_DIR)/step-01-stats.txt || echo "0 success(es) 0 failure(s) 0 error(s) 0 test(s) run" > $(TEST_RESULTS_DIR)/step-01-stats.txt; \
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
		$(RACKET) $(STEP_02_EXERCISE) 2>&1 | tee $(TEST_RESULTS_DIR)/step-02.txt | grep -v "^$$" | tail -10; \
		$(RACKET) $(STEP_02_EXERCISE) 2>&1 | grep -E "^[0-9]+ success|failure|error|test\(s\)" > $(TEST_RESULTS_DIR)/step-02-stats.txt || echo "0 success(es) 0 failure(s) 0 error(s) 0 test(s) run" > $(TEST_RESULTS_DIR)/step-02-stats.txt; \
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
		$(RACKET) $(STEP_03_EXERCISE) 2>&1 | tee $(TEST_RESULTS_DIR)/step-03.txt | grep -v "^$$" | tail -10; \
		$(RACKET) $(STEP_03_EXERCISE) 2>&1 | grep -E "^[0-9]+ success|failure|error|test\(s\)" > $(TEST_RESULTS_DIR)/step-03-stats.txt || echo "0 success(es) 0 failure(s) 0 error(s) 0 test(s) run" > $(TEST_RESULTS_DIR)/step-03-stats.txt; \
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

# テスト結果のサマリを表示
summary:
	@max_len=0; \
	for step in 01 02 03; do \
		case $$step in \
			01) name="step-01/lexer-exercise.scm";; \
			02) name="step-02/parser-exercise.scm";; \
			03) name="step-03/evaluator-exercise.scm";; \
		esac; \
		len=$$(printf "%s" "$$name" | wc -c); \
		if [ $$len -gt $$max_len ]; then \
			max_len=$$len; \
		fi; \
	done; \
	header_len=$$(printf "%s" "ファイル" | wc -c); \
	if [ $$header_len -gt $$max_len ]; then \
		max_len=$$header_len; \
	fi; \
	total_len=$$(printf "%s" "合計" | wc -c); \
	if [ $$total_len -gt $$max_len ]; then \
		max_len=$$total_len; \
	fi; \
	width=$$((max_len + 2)); \
	printf "%-$${width}s" "ファイル"; \
	printf "       %12s %12s %12s %12s %12s\n" "成功" "失敗" "エラー" "合計" "成功率"; \
	line_width=$$((width + 12 + 12 + 12 + 12 + 12)); \
	printf "%*s\n" $$line_width "" | tr " " "-"; \
	for step in 01 02 03; do \
		if [ -f $(TEST_RESULTS_DIR)/step-$$step-stats.txt ]; then \
			stats=$$(cat $(TEST_RESULTS_DIR)/step-$$step-stats.txt); \
			success=$$(echo "$$stats" | grep -oE "[0-9]+ success" | grep -oE "[0-9]+" || echo "0"); \
			failure=$$(echo "$$stats" | grep -oE "[0-9]+ failure" | grep -oE "[0-9]+" || echo "0"); \
			error=$$(echo "$$stats" | grep -oE "[0-9]+ error" | grep -oE "[0-9]+" || echo "0"); \
			total=$$(echo "$$stats" | grep -oE "[0-9]+ test" | grep -oE "[0-9]+" || echo "0"); \
			if [ "$$total" -gt 0 ]; then \
				percent=$$(echo "scale=1; $$success * 100 / $$total" | bc 2>/dev/null || echo "0"); \
			else \
				percent="0.0"; \
			fi; \
			case $$step in \
				01) name="step-01/lexer-exercise.scm";; \
				02) name="step-02/parser-exercise.scm";; \
				03) name="step-03/evaluator-exercise.scm";; \
			esac; \
			printf "%-$${width}s %10s %10s %10s %10s %11s%%\n" "$$name" "$$success" "$$failure" "$$error" "$$total" "$$percent"; \
		fi; \
	done; \
	printf "%*s\n" $$line_width "" | tr " " "-"; \
	total_success=0; \
	total_failure=0; \
	total_error=0; \
	total_tests=0; \
	for step in 01 02 03; do \
		if [ -f $(TEST_RESULTS_DIR)/step-$$step-stats.txt ]; then \
			stats=$$(cat $(TEST_RESULTS_DIR)/step-$$step-stats.txt); \
			success=$$(echo "$$stats" | grep -oE "[0-9]+ success" | grep -oE "[0-9]+" || echo "0"); \
			failure=$$(echo "$$stats" | grep -oE "[0-9]+ failure" | grep -oE "[0-9]+" || echo "0"); \
			error=$$(echo "$$stats" | grep -oE "[0-9]+ error" | grep -oE "[0-9]+" || echo "0"); \
			total=$$(echo "$$stats" | grep -oE "[0-9]+ test" | grep -oE "[0-9]+" || echo "0"); \
			total_success=$$((total_success + success)); \
			total_failure=$$((total_failure + failure)); \
			total_error=$$((total_error + error)); \
			total_tests=$$((total_tests + total)); \
		fi; \
	done; \
	if [ $$total_tests -gt 0 ]; then \
		total_percent=$$(echo "scale=1; $$total_success * 100 / $$total_tests" | bc 2>/dev/null || echo "0"); \
	else \
		total_percent="0.0"; \
	fi; \
	printf "%-$${width}s %12s %10s %10s %10s %11s%%\n" "合計" "$$total_success" "$$total_failure" "$$total_error" "$$total_tests" "$$total_percent"

# クリーンアップ（バックアップファイルとテスト結果を削除）
clean:
	@echo "バックアップファイルとテスト結果を削除しています..."
	@find . -name "*.scm~" -type f -delete 2>/dev/null || true
	@rm -rf $(TEST_RESULTS_DIR) 2>/dev/null || true
	@echo "完了しました"

