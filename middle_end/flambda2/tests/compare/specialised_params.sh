#!/bin/sh
set -eu

# All generated inputs stay in scratch.
case $1 in
  /*) fldiff=$1 ;;
  *) fldiff=$(pwd)/$1 ;;
esac
scratch=$(mktemp -d)
trap 'rm -rf "$scratch"' EXIT HUP INT TERM
cd "$scratch"

cat > specialised.fl <<'EOF'
let code size(1) f (x : val, y : val)
      specialised { x = sx; y = sy }
      my_closure &my_alloc_region my_depth -> k * e : val =
  cont k (x)
in
let $f = closure f &toplevel.alloc_region synthetic { sx = 0; sy = 1 } in
let $camlCompare = Block 0 ($f) in
cont done ($camlCompare)
EOF

sed '/specialised {/d' specialised.fl > missing.fl
sed 's/x = sx; y = sy/x = sx/' specialised.fl > missing_one.fl
sed 's/x = sx; y = sy/x = sy; y = sx/' specialised.fl > swapped.fl
sed 's/ synthetic {/ with {/' missing.fl > ordinary.fl
sed -e 's/(x : val, y : val)/(u : val, v : val)/' \
    -e 's/x = sx; y = sy/u = su; v = sv/' \
    -e 's/cont k (x)/cont k (u)/' \
    -e 's/sx = 0; sy = 1/su = 0; sv = 1/' \
    specialised.fl > renamed.fl
sed 's/x = sx; y = sy/y = sy; x = sx/' specialised.fl > reordered.fl
sed 's/cont k (x)/cont k (y)/' specialised.fl > different_body.fl
sed 's/cont k (u)/cont k (v)/' renamed.fl > renamed_different_body.fl
sed 's/closure f/closure specialisation_site f/' specialised.fl > site.fl
sed 's/cont k (x)/cont k (y)/' site.fl > site_different_body.fl
sed 's/ synthetic { sx = 0; sy = 1 }//' missing.fl > empty.fl
sed 's/closure f/closure specialisation_site f/' empty.fl > empty_site.fl
sed 's/cont k (x)/cont k (y)/' empty_site.fl > empty_site_different_body.fl
sed 's/closure f/closure specialisation_site f/' ordinary.fl > malformed_site.fl

sed 's/sy = 1/sy = 0/' specialised.fl > duplicate_values.fl
sed 's/x = sx; y = sy/y = sy; x = sx/' \
  duplicate_values.fl > duplicate_values_reordered.fl
sed 's/sx = 0; sy = 0/sx = 0; sy = 0; sz = 0/' \
  duplicate_values.fl > mixed_duplicate_values.fl
sed 's/x = sx; y = sy/y = sy; x = sx/' \
  mixed_duplicate_values.fl > mixed_duplicate_values_reordered.fl
sed 's/sz = 0/sz = 1/' \
  mixed_duplicate_values_reordered.fl > mixed_duplicate_values_changed.fl
sed 's/sx = 0; sy = 0/sx = 0/' duplicate_values.fl > missing_sy.fl
sed 's/sx = 0; sy = 0/sy = 0/' duplicate_values.fl > missing_sx.fl

sed -e 's/y : val/y : float/' -e 's/sy = 1/sy : float = 1.0/' \
  specialised.fl > mixed_kinds.fl
sed 's/cont k (x)/cont k (0)/' mixed_kinds.fl > mixed_kinds_different_body.fl
sed 's/sx = 0; sy = 1/sx = $f; sy = $f/' specialised.fl > cyclic.fl
sed -e 's/\$f/\$g/g' -e 's/sx/su/g' \
    -e 's/sy =/sv =/g' -e 's/= sy/= sv/g' cyclic.fl > cyclic_renamed.fl

failures=0
check_comparison () {
  name=$1
  expected=$2
  left=$3
  right=$4
  output=$5
  if "$fldiff" "$left" "$right" > "$output.raw" 2> errors; then
    status=0
  else
    status=$?
  fi
  # fldiff does not initialize colour flags from the environment.
  escape=$(printf '\033')
  sed "s/$escape\\[[0-9;]*m//g" "$output.raw" > "$output"
  if [ "$status" -ne "$expected" ] || [ -s errors ]; then
    printf 'FAIL %s: expected exit %s, got %s\n' \
      "$name" "$expected" "$status"
    cat errors
    failures=$((failures + 1))
  fi
}

check_both_directions () {
  name=$1
  expected=$2
  left=$3
  right=$4
  check_comparison "$name" "$expected" "$left" "$right" approximant.fl
  check_comparison "$name (reverse)" "$expected" \
    "$right" "$left" approximant.fl
}

check_both_directions 'missing specialised parameters' 1 \
  specialised.fl missing.fl
check_both_directions 'missing one specialised parameter' 1 \
  specialised.fl missing_one.fl
check_both_directions 'swapped specialised parameters' 1 \
  specialised.fl swapped.fl
check_both_directions 'ordinary versus synthetic slots' 1 ordinary.fl missing.fl
check_both_directions 'alpha-renamed parameters and slots' 0 \
  specialised.fl renamed.fl
check_both_directions 'reordered annotation entries' 0 specialised.fl reordered.fl
check_both_directions 'specialisation-site marker' 1 specialised.fl site.fl
check_both_directions 'empty specialisation-site marker' 1 empty.fl empty_site.fl
check_both_directions 'reordered equal-valued slots' 0 \
  duplicate_values.fl duplicate_values_reordered.fl
check_both_directions 'mapped and unmapped equal-valued slots' 0 \
  mixed_duplicate_values.fl mixed_duplicate_values_reordered.fl
check_both_directions 'changed unmapped slot alongside equal-valued slots' 1 \
  mixed_duplicate_values.fl mixed_duplicate_values_changed.fl
check_both_directions 'mapped slots missing from the opposite set' 1 \
  missing_sy.fl missing_sx.fl
check_both_directions 'alpha-renamed cyclic synthetic slots' 0 \
  cyclic.fl cyclic_renamed.fl

check_approximant () {
  name=$1
  original=$2
  changed=$3
  # fldiff prints an approximant of its second input, using the first input's
  # names where possible. Check metadata independently of Compare itself.
  check_comparison "$name" 1 "$original" "$changed" approximant.fl
  for annotation in specialised specialisation_site; do
    if grep -q "$annotation" "$changed"; then
      expected_annotation=0
    else
      expected_annotation=1
    fi
    if grep -q "$annotation" approximant.fl; then
      actual_annotation=0
    else
      actual_annotation=1
    fi
    if [ "$actual_annotation" -ne "$expected_annotation" ]; then
      printf 'FAIL %s: approximant changed %s annotation\n' \
        "$name" "$annotation"
      failures=$((failures + 1))
    fi
  done
  check_comparison "$name (approximant equivalent to second input)" 0 \
    "$changed" approximant.fl unused.fl
}

check_approximant 'different body' specialised.fl different_body.fl
check_approximant 'different body (reverse)' different_body.fl specialised.fl
check_approximant 'alpha-renamed different body' \
  specialised.fl renamed_different_body.fl
check_approximant 'alpha-renamed different body (reverse)' \
  renamed_different_body.fl specialised.fl
check_approximant 'specialisation-site different body' \
  site.fl site_different_body.fl
check_approximant 'specialisation-site different body (reverse)' \
  site_different_body.fl site.fl
check_approximant 'empty specialisation-site roundtrip' \
  empty_site.fl empty_site_different_body.fl
check_approximant 'empty specialisation-site roundtrip (reverse)' \
  empty_site_different_body.fl empty_site.fl
check_approximant 'missing annotation' missing.fl specialised.fl
check_approximant 'missing annotation (reverse)' specialised.fl missing.fl
check_approximant 'missing one annotation' missing_one.fl specialised.fl
check_approximant 'missing one annotation (reverse)' specialised.fl missing_one.fl
check_approximant 'swapped annotations' specialised.fl swapped.fl
check_approximant 'swapped annotations (reverse)' swapped.fl specialised.fl
check_approximant 'marker difference' specialised.fl site.fl
check_approximant 'marker difference (reverse)' site.fl specialised.fl
check_approximant 'empty marker difference' empty.fl empty_site.fl
check_approximant 'empty marker difference (reverse)' empty_site.fl empty.fl
check_approximant 'equal-valued slot approximant' \
  missing_one.fl duplicate_values_reordered.fl
check_approximant 'equal-valued slot approximant (reverse)' \
  duplicate_values_reordered.fl missing_one.fl
check_approximant 'changed unmapped slot approximant' \
  mixed_duplicate_values.fl mixed_duplicate_values_changed.fl
check_approximant 'changed unmapped slot approximant (reverse)' \
  mixed_duplicate_values_changed.fl mixed_duplicate_values.fl
check_approximant 'missing mapped slot approximant' missing_sy.fl missing_sx.fl
check_approximant 'missing mapped slot approximant (reverse)' \
  missing_sx.fl missing_sy.fl
check_approximant 'mixed value/float slot roundtrip' \
  mixed_kinds.fl mixed_kinds_different_body.fl
check_approximant 'mixed value/float slot roundtrip (reverse)' \
  mixed_kinds_different_body.fl mixed_kinds.fl

if "$fldiff" malformed_site.fl malformed_site.fl > unused.fl 2> errors; then
  printf 'FAIL marked site with ordinary value slots was accepted\n'
  failures=$((failures + 1))
elif ! grep -q 'A specialisation site cannot have runtime value slots' \
    errors; then
  printf 'FAIL marked site with ordinary value slots: unexpected error\n'
  cat errors
  failures=$((failures + 1))
fi

if [ "$failures" -ne 0 ]; then
  printf '%s comparison checks failed\n' "$failures"
  exit 1
fi
printf 'Specialised-parameter comparison checks passed\n'
