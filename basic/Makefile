# Basic Project - Makefile

.PHONY: help test test-all test-step-00 test-step-01 test-step-02 test-step-03 test-step-04 test-step-05 test-step-06

RACKET := racket

help:
	@echo "Basic Make targets:"
	@echo "  make test            - Run all basic tests (alias of test-all)"
	@echo "  make test-all        - Run step-00..06 tests"
	@echo "  make test-step-00    - Run step-00 tests"
	@echo "  make test-step-01    - Run step-01 tests"
	@echo "  make test-step-02    - Run step-02 tests"
	@echo "  make test-step-03    - Run step-03 tests"
	@echo "  make test-step-04    - Run step-04 tests"
	@echo "  make test-step-05    - Run step-05 tests"
	@echo "  make test-step-06    - Run step-06 tests"

test: test-all

test-all: test-step-00 test-step-01 test-step-02 test-step-03 test-step-04 test-step-05 test-step-06

test-step-00:
	@echo "\n=== basic/step-00 ===" && $(RACKET) step-00/exercise.scm

test-step-01:
	@echo "\n=== basic/step-01 ===" && $(RACKET) step-01/exercise.scm

test-step-02:
	@echo "\n=== basic/step-02 ===" && $(RACKET) step-02/exercise.scm

test-step-03:
	@echo "\n=== basic/step-03 ===" && $(RACKET) step-03/exercise.scm

test-step-04:
	@echo "\n=== basic/step-04 ===" && $(RACKET) step-04/exercise.scm

test-step-05:
	@echo "\n=== basic/step-05 ===" && $(RACKET) step-05/exercise.scm

test-step-06:
	@echo "\n=== basic/step-06 ===" && $(RACKET) step-06/exercise.scm


