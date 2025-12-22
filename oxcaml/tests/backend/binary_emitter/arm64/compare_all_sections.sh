#!/bin/bash
# Compare all binary emitter outputs against assembler outputs from a testsuite run
# Usage: compare_all_sections.sh <testsuite_dir>
# Example: compare_all_sections.sh _runtest/testsuite

set -e

TESTSUITE_DIR="${1:-.}"

if [ ! -d "$TESTSUITE_DIR" ]; then
    echo "ERROR: Directory $TESTSUITE_DIR not found"
    exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
COMPARE_SCRIPT="$SCRIPT_DIR/compare_sections.sh"

if [ ! -x "$COMPARE_SCRIPT" ]; then
    echo "ERROR: compare_sections.sh not found or not executable"
    exit 1
fi

echo "Searching for binary-sections directories in $TESTSUITE_DIR..."

# Find all .binary-sections directories
BINARY_SECTIONS_DIRS=$(find "$TESTSUITE_DIR" -type d -name "*.binary-sections" 2>/dev/null)

if [ -z "$BINARY_SECTIONS_DIRS" ]; then
    echo "No binary-sections directories found"
    exit 0
fi

TOTAL=0
PASSED=0
FAILED=0
SKIPPED=0
FAILED_TESTS=""

while IFS= read -r dir; do
    # Get the test name by removing .binary-sections suffix
    TEST_PATH="${dir%.binary-sections}"
    TEST_DIR=$(dirname "$TEST_PATH")
    TEST_NAME=$(basename "$TEST_PATH")

    TOTAL=$((TOTAL + 1))

    # Check if corresponding .o file exists
    OBJ_FILE="${TEST_PATH}.o"
    if [ ! -f "$OBJ_FILE" ]; then
        echo "SKIP: $TEST_NAME (no .o file)"
        SKIPPED=$((SKIPPED + 1))
        continue
    fi

    # Run comparison
    echo -n "Comparing $TEST_NAME... "
    cd "$TEST_DIR"
    if "$COMPARE_SCRIPT" "$TEST_NAME" > /tmp/compare_output.txt 2>&1; then
        echo "OK"
        PASSED=$((PASSED + 1))
    else
        echo "FAILED"
        cat /tmp/compare_output.txt | sed 's/^/  /'
        FAILED=$((FAILED + 1))
        FAILED_TESTS="$FAILED_TESTS\n  $TEST_PATH"
    fi
    cd - > /dev/null
done <<< "$BINARY_SECTIONS_DIRS"

echo ""
echo "=========================================="
echo "Binary Emitter Comparison Summary"
echo "=========================================="
echo "Total:   $TOTAL"
echo "Passed:  $PASSED"
echo "Failed:  $FAILED"
echo "Skipped: $SKIPPED"

if [ $FAILED -gt 0 ]; then
    echo ""
    echo "Failed tests:"
    echo -e "$FAILED_TESTS"
    exit 1
fi

exit 0
