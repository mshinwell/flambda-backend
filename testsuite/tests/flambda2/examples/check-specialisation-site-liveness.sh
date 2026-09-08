#!/bin/sh
# Ordinary liveness may include debug roots/edges; site liveness must not.
site_info=$(sed -n '/(specialisation_site_info/,/(aliases_result/p' "$1")
simplified=$(sed -n '/^After simplify:/,$p' "$1")
if printf '%s\n' "$site_info" | grep -q 'live_helper.*_code' &&
   ! printf '%s\n' "$site_info" | grep -q 'dead_helper.*_code' &&
   printf '%s\n' "$simplified" | grep -q 'live_hint' &&
   ! printf '%s\n' "$simplified" | grep -q 'dead_hint'; then
  exit "${TEST_PASS}"
else
  printf '%s\n' "$site_info" "$simplified"
  exit "${TEST_FAIL}"
fi
