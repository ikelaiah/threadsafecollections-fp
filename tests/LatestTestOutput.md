# Test Snapshot — 2026-08-17

[Documentation home](../docs/README.md) ·
[Current test instructions](../docs/BUILDING.md#running-the-tests)

> **Historical test snapshot.** These results describe one FPC 3.2.2 Win64 run
> on 17 August 2026. Run the current checkout to establish its present status.

## Recorded environment

Environment:

- Free Pascal Compiler 3.2.2 for x86_64
- Target OS: Win64 for x64

Build command (from `run-tests.ps1`):

```powershell
fpc -B -MObjFPC -Sh -gl -gh -Cr -Co -Fusrc -Futests -FUbuild-temp\tests\units -FEbuild-temp\tests\bin tests\TestRunner.lpr
```

Test command:

```powershell
.\build-temp\tests\bin\TestRunner.exe --all --format=plain
```

Summary:

```text
Number of run tests: 146
Number of errors:    0
Number of failures:  0
```

HeapTrc summary (from `build-temp/tests/bin/heaptrc.log`):

```text
5597246 memory blocks allocated : 787123238/818082736
5597246 memory blocks freed     : 787123238/818082736
0 unfreed memory blocks : 0
```

Suite summary:

| Suite | Tests | Errors | Failures |
|---|---:|---:|---:|
| `TThreadSafeListTest` | 47 | 0 | 0 |
| `TThreadSafeListStudentTest` | 2 | 0 | 0 |
| `TThreadSafeDictionaryTest` | 38 | 0 | 0 |
| `TThreadSafeHashSetTest` | 20 | 0 | 0 |
| `TThreadSafeDequeTests` | 11 | 0 | 0 |
| `TThreadSafeConcurrencyTests` | 11 | 0 | 0 |
| `TThreadSafeDeadlockTests` | 11 | 0 | 0 |
| `TThreadSafeRandomizedStressTests` | 6 | 0 | 0 |

Notes:

- `TThreadSafeListTest.Test47_DescendingSearch` covers direction-aware binary search, first-match
  duplicate behavior, and sorted-state maintenance after descending sort.
- Dictionary coverage verifies semantic key/value equality for separately allocated strings and
  self-source `AddRange` behavior.
- HashSet coverage verifies self-source `AddRange` and `RemoveRange` behavior.
- `TThreadSafeHashSetTest.Test20_IntersectWithLargeSnapshot` covers a 10,000-item,
  half-overlapping intersection plus legacy comparer, self-intersection, and empty-source behavior.
- The full suite includes long-running lock and collision stress tests, including the
  100,000-item aggressive-collision thread pool test.
- The v0.8.8 concurrency hardening suites are included: deterministic
  event-coordinated concurrency tests, bounded-completion deadlock
  regressions, and seeded randomized stress tests (seed, threads, iterations,
  collection, and mix are logged; `STRESS_SEED` overrides the seed). See
  `docs/Thread-Safety-and-Iteration.md` for the contracts they enforce.
- The same suite runs automatically on Windows and Linux CI through
  `run-tests.ps1` and `run-tests.sh`, which also fail the job if HeapTrc
  reports any unfreed memory block.
