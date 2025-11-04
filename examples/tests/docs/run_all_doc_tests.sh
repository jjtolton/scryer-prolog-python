#!/bin/bash
# Run all documentation example tests

set -e

echo "=========================================="
echo "Documentation Example Test Suite"
echo "=========================================="
echo ""

TESTS_PASSED=0
TESTS_FAILED=0

run_test() {
    local test_file=$1
    local test_name=$(basename "$test_file" .pl)

    echo "Running: $test_name"
    echo "------------------------------------------"

    if scryer-prolog "$test_file"; then
        TESTS_PASSED=$((TESTS_PASSED + 1))
        echo "✓ $test_name PASSED"
    else
        TESTS_FAILED=$((TESTS_FAILED + 1))
        echo "✗ $test_name FAILED"
    fi
    echo ""
}

# Run all test files
for test_file in examples/tests/docs/test_*.pl; do
    if [ -f "$test_file" ]; then
        run_test "$test_file"
    fi
done

echo "=========================================="
echo "Test Summary"
echo "=========================================="
echo "Tests Passed: $TESTS_PASSED"
echo "Tests Failed: $TESTS_FAILED"
echo "Total Tests:  $((TESTS_PASSED + TESTS_FAILED))"
echo ""

if [ $TESTS_FAILED -eq 0 ]; then
    echo "✓ All documentation examples are working!"
    exit 0
else
    echo "✗ Some documentation examples are broken!"
    exit 1
fi
