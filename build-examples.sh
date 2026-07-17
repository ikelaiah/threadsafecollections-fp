#!/usr/bin/env bash

set -euo pipefail

configuration="${1:-Release}"
case "$configuration" in
  Debug|Release)
    ;;
  *)
    echo "Usage: $0 [Debug|Release]" >&2
    exit 2
    ;;
esac

repository_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
examples_root="$repository_root/examples"
source_root="$repository_root/src"
output_root="$repository_root/example-bin"
units_root="$output_root/units"

case "$(uname -s)" in
  CYGWIN*|MINGW*|MSYS*)
    source_for_fpc="$(cygpath -w "$source_root")"
    output_for_fpc="$(cygpath -w "$output_root")"
    ;;
  *)
    source_for_fpc="$source_root"
    output_for_fpc="$output_root"
    ;;
esac

if ! command -v fpc >/dev/null 2>&1; then
  echo "Free Pascal compiler (fpc) was not found on PATH." >&2
  exit 1
fi

shopt -s nullglob
projects=("$examples_root"/*/*.lpr)
shopt -u nullglob

if ((${#projects[@]} == 0)); then
  echo "No example projects were found below '$examples_root'." >&2
  exit 1
fi

mkdir -p -- "$output_root" "$units_root"

compiler_options=(-B -MObjFPC -Sh)
if [[ "$configuration" == "Release" ]]; then
  compiler_options+=(-O3 -XX)
else
  compiler_options+=(-O1 -gl -gh -Cr -Co)
fi

output_keys=()
echo "Compiling ${#projects[@]} examples ($configuration)..."

for project in "${projects[@]}"; do
  filename="${project##*/}"
  output_name="${filename%.lpr}"
  output_key="$(printf '%s' "$output_name" | LC_ALL=C tr '[:upper:]' '[:lower:]')"

  for existing_key in "${output_keys[@]:-}"; do
    if [[ "$existing_key" == "$output_key" ]]; then
      echo "More than one example would produce '$output_name'. Rename one of the projects." >&2
      exit 1
    fi
  done
  output_keys+=("$output_key")

  case "$(uname -s)" in
    CYGWIN*|MINGW*|MSYS*)
      executable_name="$output_name.exe"
      ;;
    *)
      executable_name="$output_name"
      ;;
  esac

  project_directory="${project%/*}"
  unit_output="$units_root/$output_name"
  mkdir -p -- "$unit_output"

  case "$(uname -s)" in
    CYGWIN*|MINGW*|MSYS*)
      project_for_fpc="$(cygpath -w "$project")"
      project_directory_for_fpc="$(cygpath -w "$project_directory")"
      unit_output_for_fpc="$(cygpath -w "$unit_output")"
      ;;
    *)
      project_for_fpc="$project"
      project_directory_for_fpc="$project_directory"
      unit_output_for_fpc="$unit_output"
      ;;
  esac

  echo "  -> $output_name"
  fpc \
    "${compiler_options[@]}" \
    "-Fu$source_for_fpc" \
    "-Fu$project_directory_for_fpc" \
    "-FU$unit_output_for_fpc" \
    "-FE$output_for_fpc" \
    "-o$executable_name" \
    "$project_for_fpc"
done

echo "Compiled ${#projects[@]} examples into '$output_root'."
