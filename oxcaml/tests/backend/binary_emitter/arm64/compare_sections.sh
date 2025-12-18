#!/bin/bash
# Compare binary emitter output against system assembler output
# Usage: ./compare_sections.sh <test_prefix>
#
# Expects:
#   <test_prefix>.o        - Object file from system assembler
#   <test_prefix>.binary-sections/  - Directory with binary emitter output

set -e

if [ $# -lt 1 ]; then
    echo "Usage: $0 <test_prefix>"
    exit 1
fi

PREFIX="$1"
OBJ_FILE="${PREFIX}.o"
BINARY_DIR="${PREFIX}.binary-sections"

if [ ! -f "$OBJ_FILE" ]; then
    echo "Error: Object file $OBJ_FILE not found"
    exit 1
fi

if [ ! -d "$BINARY_DIR" ]; then
    echo "Error: Binary sections directory $BINARY_DIR not found"
    exit 1
fi

# Detect objdump variant
if objdump --version 2>&1 | grep -q "LLVM"; then
    OBJDUMP_TYPE="llvm"
else
    OBJDUMP_TYPE="gnu"
fi

echo "Using $OBJDUMP_TYPE objdump"

# Create temp directory for extracted sections
TMPDIR=$(mktemp -d)
trap "rm -rf $TMPDIR" EXIT

# Map binary emitter section names to Mach-O section names
# On macOS, .text becomes __TEXT,__text, etc.
map_section_name() {
    local bin_name="$1"
    case "$bin_name" in
        ".text")
            echo "__text"
            ;;
        ".rodata")
            echo "__const"
            ;;
        ".data")
            echo "__data"
            ;;
        *)
            # Strip leading dot and try as-is
            echo "${bin_name#.}"
            ;;
    esac
}

# Extract section content from object file using objdump + awk + xxd
extract_section() {
    local obj_file="$1"
    local section="$2"
    local output="$3"

    if [ "$OBJDUMP_TYPE" = "llvm" ]; then
        # LLVM objdump: use --full-contents
        objdump --full-contents -j "$section" "$obj_file" 2>/dev/null | \
            awk '
            /^ [0-9a-f]+ / {
                # Skip the address (first column) and ASCII dump (last column)
                # Format: " addr hexbytes... ascii"
                line = $0
                # Remove leading space and address
                sub(/^ [0-9a-f]+ /, "", line)
                # Remove trailing ASCII part (everything after two or more spaces followed by printable chars)
                sub(/  +[^ ].*$/, "", line)
                # Remove all spaces
                gsub(/ /, "", line)
                hex = hex line
            }
            END {
                for (i = 1; i <= length(hex); i += 2) {
                    printf "%c", strtonum("0x" substr(hex, i, 2))
                }
            }' > "$output"
    else
        # GNU objdump
        objdump -s -j "$section" "$obj_file" 2>/dev/null | \
            awk '
            /^ [0-9a-f]+ / {
                for (i = 2; i <= 5 && i <= NF; i++) {
                    if ($i ~ /^[0-9a-fA-F]+$/) {
                        hex = hex $i
                    }
                }
            }
            END {
                for (i = 1; i <= length(hex); i += 2) {
                    printf "%c", strtonum("0x" substr(hex, i, 2))
                }
            }' > "$output"
    fi
}

# Compare two binary files, showing hex diff on mismatch
compare_binary() {
    local file1="$1"
    local file2="$2"
    local name="$3"

    if cmp -s "$file1" "$file2"; then
        echo "  $name: OK"
        return 0
    else
        echo "  $name: MISMATCH"
        echo "  System assembler:"
        xxd "$file1" | head -20
        echo "  Binary emitter:"
        xxd "$file2" | head -20
        return 1
    fi
}

# Compare text sections by disassembling
compare_text_section() {
    local sys_bin="$1"
    local be_bin="$2"
    local name="$3"

    # First check if sizes match
    local sys_size=$(wc -c < "$sys_bin")
    local be_size=$(wc -c < "$be_bin")

    if [ "$sys_size" != "$be_size" ]; then
        echo "  $name: SIZE MISMATCH (system: $sys_size, binary emitter: $be_size)"
        return 1
    fi

    # Binary comparison for exact match
    if cmp -s "$sys_bin" "$be_bin"; then
        echo "  $name: OK ($sys_size bytes)"
        return 0
    else
        echo "  $name: CONTENT MISMATCH ($sys_size bytes)"
        # Show disassembly diff
        echo "  Disassembling..."

        # Disassemble system binary
        llvm-objdump --disassemble-all -b binary -m aarch64 --no-show-raw-insn "$sys_bin" > "$TMPDIR/sys_dis.txt" 2>/dev/null || \
            objdump -D -b binary -m aarch64 --no-show-raw-insn "$sys_bin" > "$TMPDIR/sys_dis.txt" 2>/dev/null || \
            xxd "$sys_bin" > "$TMPDIR/sys_dis.txt"

        # Disassemble binary emitter output
        llvm-objdump --disassemble-all -b binary -m aarch64 --no-show-raw-insn "$be_bin" > "$TMPDIR/be_dis.txt" 2>/dev/null || \
            objdump -D -b binary -m aarch64 --no-show-raw-insn "$be_bin" > "$TMPDIR/be_dis.txt" 2>/dev/null || \
            xxd "$be_bin" > "$TMPDIR/be_dis.txt"

        # Show diff
        diff -u "$TMPDIR/sys_dis.txt" "$TMPDIR/be_dis.txt" | head -40 || true
        return 1
    fi
}

ERRORS=0

# Process each binary emitter section
for bin_file in "$BINARY_DIR"/*.bin; do
    if [ ! -f "$bin_file" ]; then
        continue
    fi

    # Get section name (remove .bin extension and path)
    section_name=$(basename "$bin_file" .bin)
    macho_section=$(map_section_name "$section_name")

    echo "Comparing section: $section_name (Mach-O: $macho_section)"

    # Extract section from system-assembled object
    sys_section="$TMPDIR/${section_name}_sys.bin"
    extract_section "$OBJ_FILE" "$macho_section" "$sys_section"

    if [ ! -s "$sys_section" ]; then
        echo "  Warning: Could not extract $macho_section from $OBJ_FILE"
        continue
    fi

    # Compare based on section type
    case "$section_name" in
        .text)
            compare_text_section "$sys_section" "$bin_file" "$section_name" || ERRORS=$((ERRORS + 1))
            ;;
        *)
            compare_binary "$sys_section" "$bin_file" "$section_name" || ERRORS=$((ERRORS + 1))
            ;;
    esac
done

if [ $ERRORS -eq 0 ]; then
    echo "All sections match!"
    exit 0
else
    echo "$ERRORS section(s) had mismatches"
    exit 1
fi
