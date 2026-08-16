#!/usr/bin/env bash

set -euo pipefail

repository_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
tests_root="$repository_root/tests"
source_root="$repository_root/src"
units_out="$repository_root/build-temp/tests/units"
bin_out="$repository_root/build-temp/tests/bin"

case "$(uname -s)" in
  CYGWIN*|MINGW*|MSYS*)
    source_for_fpc="$(cygpath -w "$source_root")"
    tests_for_fpc="$(cygpath -w "$tests_root")"
    units_for_fpc="$(cygpath -w "$units_out")"
    bin_for_fpc="$(cygpath -w "$bin_out")"
    ;;
  *)
    source_for_fpc="$source_root"
    tests_for_fpc="$tests_root"
    units_for_fpc="$units_out"
    bin_for_fpc="$bin_out"
    ;;
esac

skip_leak_check=0
if [[ "${1:-}" == "--skip-leak-check" ]]; then
  skip_leak_check=1
fi

if ! command -v fpc >/dev/null 2>&1; then
  echo "Free Pascal compiler (fpc) was not found on PATH." >&2
  exit 1
fi

mkdir -p -- "$units_out" "$bin_out"

echo "Compiling the FPCUnit test runner (debug checks + HeapTrc)..."
fpc -B -MObjFPC -Sh -gl -gh -Cr -Co \
  "-Fu$source_for_fpc" \
  "-Fu$tests_for_fpc" \
  "-FU$units_for_fpc" \
  "-FE$bin_for_fpc" \
  "$tests_root/TestRunner.lpr"

runner="$bin_out/TestRunner"
output_file="$bin_out/test-output.txt"

echo "Running the FPCUnit suite (collision and stress cases can take several minutes)..."
set +e
"$runner" --all --format=plain >"$output_file" 2>&1
runner_exit_code=$?
set -e

extract() {
  sed -n "s/.*$1:[[:space:]]*\([0-9][0-9]*\).*/\1/p" "$output_file" | tail -n 1
}

run_tests="$(extract 'Number of run tests')"
errors="$(extract 'Number of errors')"
failures="$(extract 'Number of failures')"
unfreed="$(sed -n 's/.*\([0-9][0-9]*\) unfreed memory blocks.*/\1/p' "$output_file" | tail -n 1)"

echo ""
echo "FPCUnit summary:"
echo "  run tests : ${run_tests:-<not found>}"
echo "  errors    : ${errors:-<not found>}"
echo "  failures  : ${failures:-<not found>}"
echo "  HeapTrc   : ${unfreed:-<not found>} unfreed memory blocks"

failed=0

if [[ -z "$run_tests" || -z "$errors" || -z "$failures" ]]; then
  echo ""
  echo "FAIL: could not parse the FPCUnit summary from '$output_file'."
  echo "Full output:"
  cat "$output_file"
  exit 1
fi

if (( runner_exit_code != 0 )); then
  echo "The test runner exited with code $runner_exit_code."
  failed=1
fi

if (( errors > 0 || failures > 0 )); then
  echo "The FPCUnit suite reported errors or failures."
  failed=1
fi

if (( skip_leak_check == 0 )); then
  if [[ -z "$unfreed" ]]; then
    echo "FAIL: no HeapTrc summary was found. The runner must be built with -gh"
    echo "and exit normally for leak verification."
    echo "Last 40 lines of the test output:"
    tail -n 40 "$output_file"
    failed=1
  elif (( unfreed > 0 )); then
    echo "FAIL: HeapTrc reports $unfreed unfreed memory blocks."
    failed=1
  fi
fi

if (( failed == 1 )); then
  echo ""
  echo "FAILURE: the FPCUnit suite reported problems. Full output: $output_file"
  exit 1
fi

echo ""
echo "SUCCESS: the FPCUnit suite passed, and HeapTrc reported no unfreed blocks."
