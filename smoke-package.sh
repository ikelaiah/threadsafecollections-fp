#!/usr/bin/env bash

set -euo pipefail

repository_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
package_file="$repository_root/package/lazarus/ThreadSafeCollections.lpk"
package_dir="$repository_root/package/lazarus"
source_root="$repository_root/src"
consumer_source="$repository_root/tools/package-smoke-consumer.lpr"
smoke_dir="$repository_root/build-temp/package-smoke"
lib_dir="$package_dir/lib"

case "$(uname -s)" in
  CYGWIN*|MINGW*|MSYS*)
    package_file_for_lazbuild="$(cygpath -w "$package_file")"
    source_for_fpc="$(cygpath -w "$source_root")"
    consumer_for_fpc="$(cygpath -w "$consumer_source")"
    smoke_units_for_fpc="$(cygpath -w "$smoke_dir/units")"
    smoke_bin_for_fpc="$(cygpath -w "$smoke_dir/bin")"
    ;;
  *)
    package_file_for_lazbuild="$package_file"
    source_for_fpc="$source_root"
    consumer_for_fpc="$consumer_source"
    smoke_units_for_fpc="$smoke_dir/units"
    smoke_bin_for_fpc="$smoke_dir/bin"
    ;;
esac

expected_version="${1:-}"
if [[ -z "$expected_version" ]]; then
  echo "Usage: $0 <expected-version>  (for example: $0 0.8.7)" >&2
  exit 2
fi

if ! command -v lazbuild >/dev/null 2>&1; then
  echo "lazbuild was not found on PATH." >&2
  exit 1
fi

if ! command -v fpc >/dev/null 2>&1; then
  echo "Free Pascal compiler (fpc) was not found on PATH." >&2
  exit 1
fi

version="$(sed -n 's/.*<Version Minor="\([0-9][0-9]*\)" Release="\([0-9][0-9]*\)".*/0.\1.\2/p' "$package_file" | head -n 1)"
if [[ "$version" != "$expected_version" ]]; then
  echo "FAIL: package version is '$version', expected '$expected_version'." >&2
  exit 1
fi
echo "Package version: $version (matches $expected_version)"

mapfile -t source_units < <(find "$source_root" -maxdepth 1 -type f -name '*.pas' -printf '%f\n' | sort)
missing_unit=0
for unit_file in "${source_units[@]}"; do
  if ! grep -q -- "$unit_file" "$package_file"; then
    echo "FAIL: $unit_file is not listed in the package files." >&2
    missing_unit=1
  fi
done
if (( missing_unit == 1 )); then
  exit 1
fi
echo "All ${#source_units[@]} source units are listed in the package."

echo "Building the Lazarus package with lazbuild..."
lazbuild --build-all "$package_file_for_lazbuild"

mapfile -t output_dirs < <(find "$lib_dir" -mindepth 1 -maxdepth 1 -type d | sort)
if ((${#output_dirs[@]} == 0)); then
  echo "FAIL: lazbuild produced no output directory below '$lib_dir'." >&2
  exit 1
fi
package_units="$lib_dir/${output_dirs[0]##*/}"
echo "Package units written to: $package_units"

case "$(uname -s)" in
  CYGWIN*|MINGW*|MSYS*)
    package_units_for_fpc="$(cygpath -w "$package_units")"
    ;;
  *)
    package_units_for_fpc="$package_units"
    ;;
esac

missing_ppu=0
for unit_file in "${source_units[@]}"; do
  unit_base="${unit_file%.pas}"
  if [[ ! -f "$package_units/$unit_base.ppu" ]]; then
    echo "FAIL: expected compiled unit '$package_units/$unit_base.ppu' was not produced." >&2
    missing_ppu=1
  fi
done
if (( missing_ppu == 1 )); then
  exit 1
fi
echo "All ${#source_units[@]} package units were compiled."

mkdir -p -- "$smoke_dir/units" "$smoke_dir/bin"
echo "Compiling and running the tiny package consumer..."
fpc -B -MObjFPC -Sh \
  "-Fu$package_units_for_fpc" \
  "-FU$smoke_units_for_fpc" \
  "-FE$smoke_bin_for_fpc" \
  "$consumer_for_fpc"
"$smoke_dir/bin/package-smoke-consumer"

echo "Package smoke build passed."
