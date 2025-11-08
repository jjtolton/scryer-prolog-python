.PHONY: all test test-unit test-phase1 test-examples test-ci validate-docs clean deps help

SCRYER := scryer-prolog --no-add-history

RED = \033[0;31m
GREEN = \033[0;32m
YELLOW = \033[1;33m
NC = \033[0m

help:
	@echo "ScryPy - Python Integration for Scryer Prolog"
	@echo ""
	@echo "Targets:"
	@echo "  test          - Run all tests (Phase 1 unit tests + examples)"
	@echo "  test-unit     - Run Phase 1 unit tests only"
	@echo "  test-phase1   - Alias for test-unit"
	@echo "  test-examples - Run example/smoke tests"
	@echo "  validate-docs - Validate documentation and string/atom usage"
	@echo "  test-ci       - Test CI workflow locally (Docker)"
	@echo "  clean         - Remove generated files"
	@echo "  deps          - Check/install dependencies"

deps:
	@echo "Checking dependencies..."
	@command -v scryer-prolog >/dev/null 2>&1 || \
		{ echo "$(RED)scryer-prolog not found.$(NC) Install fork from https://github.com/jjtolton/scryer-prolog (branch: rtld-global-support)"; exit 1; }
	@echo "$(GREEN)✓ scryer-prolog found$(NC)"
	@echo ""
	@echo "Checking Python configuration..."
	@test -f python.pl || \
		{ echo "$(YELLOW)⚠ python.pl not found.$(NC) Copy python.pl.example to python.pl and configure your Python library path."; exit 1; }
	@echo "$(GREEN)✓ python.pl configured$(NC)"

test: deps test-unit test-examples
	@echo ""
	@echo "$(GREEN)✅ All tests passed!$(NC)"

test-unit: deps
	@echo "$(YELLOW)Running Phase 1 unit tests...$(NC)"
	@cd tests && $(SCRYER) test_all.pl

test-phase1: test-unit

test-examples: deps
	@echo "$(YELLOW)Running example/smoke tests...$(NC)"
	@echo "  $(YELLOW)→$(NC) test_all_types.pl"
	@$(SCRYER) examples/tests/test_all_types.pl
	@echo "  $(YELLOW)→$(NC) test_dict.pl"
	@$(SCRYER) examples/tests/test_dict.pl
	@echo "  $(YELLOW)→$(NC) test_dict_to_list.pl"
	@$(SCRYER) examples/tests/test_dict_to_list.pl
	@echo "  $(YELLOW)→$(NC) test_globals_locals.pl"
	@$(SCRYER) examples/tests/test_globals_locals.pl
	@echo "  $(YELLOW)→$(NC) test_memory_management.pl"
	@$(SCRYER) examples/tests/test_memory_management.pl
	@echo ""
	@echo "$(GREEN)✅ Example tests passed!$(NC)"

validate-docs:
	@echo "$(YELLOW)Validating documentation and string/atom usage...$(NC)"
	@$(SCRYER) utils/validate_all.pl
	@echo ""
	@echo "$(GREEN)✅ Documentation validation passed!$(NC)"

test-ci:
	@echo "$(YELLOW)Testing CI workflow locally with Docker...$(NC)"
	@test -f test-ci.sh || \
		{ echo "$(RED)test-ci.sh not found$(NC)"; exit 1; }
	@bash test-ci.sh

clean:
	@echo "Cleaning..."
	@find . -name '*~' -delete
	@find . -name '*.swp' -delete
	@find . -name '.DS_Store' -delete
	@echo "$(GREEN)✓ Clean complete$(NC)"
