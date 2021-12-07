#!/bin/bash

set -eu -o pipefail

results_dir=/usr/local/home/mshinwell/flambda-backend/perf-stat-results

tmp=$(mktemp)
trap "rm -f $tmp" ERR
src=$(echo "$*" | sed 's/^.* \(.*\)/\1/')
bin="$0"
perf stat -x, -o "$tmp" -e cycles -- "${bin}-real" "$@"

mkdir -p "$results_dir"
file=$(mktemp -p $results_dir)
echo -n "${src}," > "$file"
grep cycles "$tmp" | cut -d, -f1 >> "$file"
