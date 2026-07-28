# ThreadSafeCollections-FP v0.8.5 Release Notes

[Documentation home](README.md) · [Current changelog](../CHANGELOG.md)

> **Historical release snapshot.** This document records v0.8.5 and may contain
> release-specific measurements or setup details. Use the
> [current documentation](README.md) for the present checkout.

**Audience:** users researching the v0.8.5 release and project maintainers.

**Status**: Released

**Release date**: 2026-07-17

## Overview

Version 0.8.5 is a performance and build-tooling release. It improves the scaling of HashSet
intersection, removes repeated work from common Dictionary insertion paths, expands benchmark and
regression coverage, and adds reproducible cross-platform builds for every example project.

The release also introduces the repository's first GitHub Actions workflow and refreshes the
project branding with a scalable SVG logo.

## Highlights

- HashSet intersection improves from O(n*m) to O(n+m) average complexity.
- Dictionary new-key insertion reuses hashes and bucket positions already computed by lookup.
- All 16 examples can be built with one PowerShell or Bash command.
- GitHub Actions compiles and publishes example binaries for Linux and Windows.
- Benchmark timing now compiles on Windows and POSIX platforms.

## Performance Improvements

### HashSet `IntersectWith`

`TThreadSafeHashSet.IntersectWith` now creates an array-backed hash index over the source snapshot,
then filters the destination set against that index. It uses the destination set's configured hash
and equality functions.

- Average complexity changes from O(n*m) to O(n+m).
- Worst-case complexity remains O(n*m) when every source value has the same hash.
- The source is still snapshotted before the destination lock is acquired.
- Self-intersection remains safe.
- No source collection method is called while the destination lock is held.

Indicative 50%-overlap timings on the development machine, with setup excluded:

| Items | Average time |
|---:|---:|
| 10,000 | 371 us |
| 100,000 | 7,995 us |

### Dictionary new-key insertion

`AddOrSetValue`, `TryAdd`, and `AddRange` now reuse their precomputed hash and bucket position when
inserting a missing key. This removes a second hash calculation and bucket lookup from each new-key
path without changing public behavior.

## Compiler Compatibility

HashSet constructors now use the unambiguous `THashSetEqualityComparer<T>` delegate name. This
prevents FPC late generic specialization from binding the constructor to
`Generics.Defaults.TEqualityComparer<T>` when Dictionary and HashSet are specialized together.

The original public `TEqualityComparer<T>` delegate remains available and constructor-compatible,
so existing equality functions do not need to change.

## Benchmark Updates

The benchmark adds scenarios for:

- Dictionary `AddRange`;
- Dictionary `AddOrSetValue` with new keys;
- HashSet `AddRange`;
- 50%-overlap HashSet `IntersectWith`.

Use `--size=N` to run a single collection size. Setup and teardown can be kept outside the measured
region, producing more representative bulk-operation timings.

Windows uses `QueryPerformanceCounter` and retains the optional `--affinity` switch. POSIX builds
use `gettimeofday`; `--affinity` reports that it is Windows-only.

## Example Build Tooling

From the repository root, compile every example in Release mode with either command:

```powershell
.\build-examples.ps1 -Configuration Release
```

```bash
bash ./build-examples.sh Release
```

Executables are written to `example-bin/`, with compiler units isolated by project below
`example-bin/units/`. The scripts compile only `examples/<project>/*.lpr`, so Lazarus backup copies
are not treated as independent examples.

The Bash script supports native POSIX paths and Windows path conversion under Git Bash.

## Continuous Integration

The new `.github/workflows/ci.yml` workflow runs on pushes, pull requests, and manual dispatches:

- Ubuntu compiles all examples with `build-examples.sh`;
- Windows compiles all examples with `build-examples.ps1`;
- each job publishes its example executables as a seven-day artifact.

## Example and POSIX Fixes

- Dictionary examples now use `Generics.Collections.TPair` instead of the removed
  `TDictionaryPair` name.
- `ChatMessageQueue` and the benchmark load `cthreads` first on Unix.
- the benchmark conditionally imports Windows or POSIX timing units.

## Documentation and Branding

- Added a repository-native SVG logo to the README.
- Updated the README with cross-platform example build commands.
- Updated the HashSet API documentation and generated cheat sheet.
- Updated Lazarus package metadata to v0.8.5.
- Added the [v0.8.5 pull request summary](PR_v0.8.5.md).

## Compatibility

Version 0.8.5 is intended to be source-compatible with v0.8.4.

- No collection methods were removed or renamed.
- Existing HashSet equality functions remain accepted.
- Lock ordering and source-snapshot behavior are preserved.
- Dictionary duplicate-key and value-update semantics are unchanged.

## Validation

Local verification used Free Pascal 3.2.2 on Win64:

| Check | Result |
|---|---|
| Full FPCUnit suite | 118 tests passed; 0 errors, 0 failures, zero HeapTrc leaks |
| HashSet large-intersection test | Passed; zero HeapTrc leaks |
| Dictionary suite | 38 tests passed; zero HeapTrc leaks |
| Targeted HashSet tests | Passed; zero HeapTrc leaks |
| Lazarus package | Compiled successfully as v0.8.5 |
| PowerShell example build | 16/16 projects compiled |
| Bash/Git Bash example build | 16/16 projects compiled |
| GitHub Actions example builds | Linux/Bash and Windows/PowerShell passed for push and pull request |
| Benchmark smoke test | Passed |
| Static checks | PowerShell, Bash, YAML, SVG/XML, and diff checks passed |

The complete FPCUnit suite passed with 118 tests, 0 errors, 0 failures, and zero unfreed
HeapTrc blocks. It includes the 100,000-item aggressive collision stress test and the new
large-snapshot intersection regression. All four GitHub-hosted example build checks passed.

## Upgrading from v0.8.4

Replace the source files and rebuild the Lazarus package. Existing application code should not
require changes.

If code explicitly names the HashSet comparer delegate, the legacy `TEqualityComparer<T>` name
continues to work. New code may use `THashSetEqualityComparer<T>` to avoid ambiguity in mixed-unit
generic programs.

## Resources

- **Pull Request Summary**: [PR_v0.8.5.md](PR_v0.8.5.md)
- **Full Changelog**: [CHANGELOG.md](../CHANGELOG.md)
- **Generated Cheat Sheet**: [CHEATSHEET.md](CHEATSHEET.md)
- **HashSet Documentation**: [ThreadSafeCollections.HashSet.md](ThreadSafeCollections.HashSet.md)
- **Repository**: https://github.com/ikelaiah/threadsafecollections-fp
- **Issues**: https://github.com/ikelaiah/threadsafecollections-fp/issues
