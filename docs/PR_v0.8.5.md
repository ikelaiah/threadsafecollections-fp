# Pull Request: v0.8.5 Performance and Cross-Platform Build Tooling

## Summary

This PR prepares ThreadSafeCollections-FP v0.8.5 with focused performance work in
Dictionary and HashSet, regression coverage for large intersections, portable benchmark
timing, reproducible example builds, and the repository's first GitHub Actions workflow.

The release keeps the public collection behavior and snapshot-first locking model intact.
It also retains the original HashSet comparer delegate name for source compatibility while
using an unambiguous comparer type internally to avoid an FPC generic-specialization collision.

## Motivation

- `TThreadSafeHashSet.IntersectWith` scanned every source item for every destination item,
  giving it O(n*m) average behavior even though both collections are hash sets.
- Dictionary insertion paths recalculated hashes and bucket positions after already performing
  the same work during lookup.
- Example compilation was manual and was not protected by CI.
- The benchmark depended directly on Windows timing and affinity APIs.
- Three Dictionary examples referenced the obsolete `TDictionaryPair` name.

## Changes

### HashSet intersection

`TThreadSafeHashSet.IntersectWith` still snapshots the source before locking the destination.
It now builds an array-backed hash index from that snapshot using the destination set's hash
and equality delegates.

| Characteristic | Before | v0.8.5 |
|---|---|---|
| Average complexity | O(n*m) | O(n+m) |
| Full-collision worst case | O(n*m) | O(n*m) |
| Source access while destination is locked | None | None |
| Self-intersection safety | Preserved | Preserved |

The change preserves the snapshot-first approach introduced to prevent ABBA lock ordering and
does not call the source collection while the destination lock is held.

### Dictionary insertion reuse

A new private `InternalInsertNew` helper accepts the precomputed hash and bucket index. The
following paths now reuse lookup work when inserting a new key:

- `AddOrSetValue`;
- `TryAdd`;
- `AddRange`.

This removes redundant hashing and bucket selection without changing duplicate-key or update
semantics.

### FPC generic comparer compatibility

HashSet constructors now use the uniquely named `THashSetEqualityComparer<T>` delegate. The
legacy public `TEqualityComparer<T>` delegate remains declared and assignment-compatible, with
regression coverage proving it can still be passed to the constructor.

This avoids late-specialization collisions with `Generics.Defaults.TEqualityComparer<T>` when
Dictionary and HashSet specializations are used in the same program.

### Benchmarks and regression coverage

The benchmark now includes direct scenarios for:

- Dictionary `AddRange`;
- Dictionary `AddOrSetValue` with new keys;
- HashSet `AddRange`;
- HashSet `IntersectWith` with 50% overlap.

Setup and teardown can be excluded from timed regions, and `--size=N` provides a quick
single-size run. Windows continues to use `QueryPerformanceCounter`; POSIX builds use
`gettimeofday`. The optional `--affinity` switch is explicitly Windows-only.

The new HashSet test covers a 10,000-item half-overlap intersection, legacy comparer
compatibility, self-intersection, and intersection with an empty source.

### Cross-platform example builds

Two repository-root scripts discover `examples/<project>/*.lpr` and compile every real example:

| Script | Intended environment | Output |
|---|---|---|
| [`build-examples.ps1`](../build-examples.ps1) | PowerShell on Windows or POSIX | `example-bin/` |
| [`build-examples.sh`](../build-examples.sh) | Bash on Linux, macOS, or Git Bash | `example-bin/` |

Compiler units are isolated per project under `example-bin/units/`. Generated files are ignored,
while `example-bin/.gitkeep` preserves the output directory in the repository.

The examples were also updated for current APIs and POSIX execution:

- Dictionary iterators use `Generics.Collections.TPair`;
- the benchmark and chat queue initialize `cthreads` on Unix;
- the benchmark conditionally selects Windows or POSIX timing support.

### Continuous integration

The new [GitHub Actions workflow](../.github/workflows/ci.yml) adds:

- an Ubuntu job that compiles all examples through the Bash script;
- a Windows job that compiles all examples through the PowerShell script;
- downloadable Linux and Windows example-binary artifacts.

### Documentation and branding

- package metadata and generated API documentation target v0.8.5;
- the changelog and HashSet documentation describe the new complexity and compatibility behavior;
- the README documents the example build commands;
- a scalable SVG project logo is displayed in the README;
- v0.8.5 PR and release-note documents are included.

## Compatibility

- No collection method was removed or renamed.
- Existing HashSet equality functions remain constructor-compatible.
- Dictionary duplicate-key and update behavior is unchanged.
- HashSet intersection retains snapshot-first deadlock safety and destination comparer semantics.
- `--affinity` remains supported on Windows and now reports its platform restriction elsewhere.

## Validation

| Check | Result |
|---|---|
| HashSet large-intersection regression | Passed; zero HeapTrc leaks |
| Dictionary test suite | 38 tests passed; zero HeapTrc leaks |
| Targeted HashSet regression tests | Passed; zero HeapTrc leaks |
| Lazarus package | Compiled successfully as v0.8.5 |
| PowerShell example build | 16/16 projects compiled |
| Bash/Git Bash example build | 16/16 projects compiled |
| Benchmark smoke test | Passed with microsecond-resolution output |
| Script and workflow syntax | PowerShell, Bash, and YAML checks passed |

Indicative 50%-overlap HashSet intersection timings on the development machine, excluding setup:

| Items | Average time |
|---:|---:|
| 10,000 | 371 us |
| 100,000 | 7,995 us |

The complete all-tests run was not finished in this work session because the aggressive collision
stress test exceeded the 120-second local window. The focused suites covering the changed code
passed. The GitHub-hosted Linux and Windows jobs will run after the branch is pushed.

## Review Focus

1. HashSet snapshot-index construction and collision-chain traversal.
2. Dictionary reuse of precomputed hashes and bucket indices.
3. Compatibility of the legacy HashSet comparer delegate.
4. Platform path handling in both example-build scripts.
5. First-run results from both GitHub Actions jobs.

## Checklist

- [x] Performance changes implemented
- [x] Focused regression tests added and passing
- [x] Package metadata updated to v0.8.5
- [x] All 16 examples compile through both local scripts
- [x] Changelog and API documentation updated
- [x] PR and release notes added
- [ ] GitHub-hosted Linux and Windows jobs confirmed after push
