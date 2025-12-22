#!/bin/bash
# Compare binary emitter output against assembler output
# Usage: compare_sections.sh <test_name>
# Expects: <test_name>.o and <test_name>.binary-sections/ to exist

set -e

TEST_NAME="$1"

if [ -z "$TEST_NAME" ]; then
    echo "Usage: $0 <test_name>"
    exit 1
fi

OBJ_FILE="${TEST_NAME}.o"
BINARY_SECTIONS_DIR="${TEST_NAME}.binary-sections"

if [ ! -f "$OBJ_FILE" ]; then
    echo "ERROR: Object file $OBJ_FILE not found"
    exit 1
fi

if [ ! -d "$BINARY_SECTIONS_DIR" ]; then
    echo "ERROR: Binary sections directory $BINARY_SECTIONS_DIR not found"
    exit 1
fi

# Create temp directory for extracted sections
TMPDIR=$(mktemp -d)
trap "rm -rf $TMPDIR" EXIT

FAILED=0

# Extract and compare __TEXT __text section
if [ -f "$BINARY_SECTIONS_DIR/section_text.bin" ]; then
    segedit "$OBJ_FILE" -extract __TEXT __text "$TMPDIR/asm_text.bin" 2>/dev/null || {
        echo "WARNING: Could not extract __TEXT __text from $OBJ_FILE"
    }

    if [ -f "$TMPDIR/asm_text.bin" ]; then
        if ! cmp -s "$TMPDIR/asm_text.bin" "$BINARY_SECTIONS_DIR/section_text.bin"; then
            echo "MISMATCH: __TEXT __text section differs"
            echo "  Assembler size: $(wc -c < "$TMPDIR/asm_text.bin")"
            echo "  Binary emitter size: $(wc -c < "$BINARY_SECTIONS_DIR/section_text.bin")"

            # Find first difference
            DIFF_OFFSET=$(cmp -l "$TMPDIR/asm_text.bin" "$BINARY_SECTIONS_DIR/section_text.bin" 2>/dev/null | head -1 | awk '{print $1}')
            if [ -n "$DIFF_OFFSET" ]; then
                # Convert to instruction offset (ARM64 instructions are 4 bytes)
                INSTR_OFFSET=$(( (DIFF_OFFSET - 1) / 4 * 4 ))
                echo "  First difference at byte offset: $((DIFF_OFFSET - 1)) (instruction at offset $INSTR_OFFSET)"
                echo ""
                echo "  Assembler bytes at offset $INSTR_OFFSET:"
                xxd -s $INSTR_OFFSET -l 16 "$TMPDIR/asm_text.bin" | sed 's/^/    /'
                echo "  Binary emitter bytes at offset $INSTR_OFFSET:"
                xxd -s $INSTR_OFFSET -l 16 "$BINARY_SECTIONS_DIR/section_text.bin" | sed 's/^/    /'
            fi
            FAILED=1
        else
            echo "OK: __TEXT __text section matches ($(wc -c < "$TMPDIR/asm_text.bin") bytes)"
        fi
    fi
fi

# Extract and compare __DATA __data section
if [ -f "$BINARY_SECTIONS_DIR/section_data.bin" ]; then
    segedit "$OBJ_FILE" -extract __DATA __data "$TMPDIR/asm_data.bin" 2>/dev/null || {
        # Try __DATA __const for read-only data
        segedit "$OBJ_FILE" -extract __DATA __const "$TMPDIR/asm_data.bin" 2>/dev/null || {
            echo "WARNING: Could not extract __DATA section from $OBJ_FILE"
        }
    }

    if [ -f "$TMPDIR/asm_data.bin" ]; then
        if ! cmp -s "$TMPDIR/asm_data.bin" "$BINARY_SECTIONS_DIR/section_data.bin"; then
            echo "MISMATCH: __DATA section differs"
            echo "  Assembler size: $(wc -c < "$TMPDIR/asm_data.bin")"
            echo "  Binary emitter size: $(wc -c < "$BINARY_SECTIONS_DIR/section_data.bin")"

            # Find first difference
            DIFF_OFFSET=$(cmp -l "$TMPDIR/asm_data.bin" "$BINARY_SECTIONS_DIR/section_data.bin" 2>/dev/null | head -1 | awk '{print $1}')
            if [ -n "$DIFF_OFFSET" ]; then
                OFFSET=$((DIFF_OFFSET - 1))
                echo "  First difference at byte offset: $OFFSET"
                echo ""
                echo "  Assembler bytes at offset $OFFSET:"
                xxd -s $OFFSET -l 16 "$TMPDIR/asm_data.bin" | sed 's/^/    /'
                echo "  Binary emitter bytes at offset $OFFSET:"
                xxd -s $OFFSET -l 16 "$BINARY_SECTIONS_DIR/section_data.bin" | sed 's/^/    /'
            fi
            FAILED=1
        else
            echo "OK: __DATA section matches ($(wc -c < "$TMPDIR/asm_data.bin") bytes)"
        fi
    fi
fi

if [ $FAILED -eq 1 ]; then
    exit 1
fi

echo "All sections match!"
exit 0
