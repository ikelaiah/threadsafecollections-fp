# Latest Test Output

## Output as of 2026-07-16

Environment:

- Free Pascal Compiler 3.2.2 for x86_64
- Target OS: Win64 for x64

Build command:

```powershell
fpc -B -Fusrc -FUbuild-temp\audit-units -FEbuild-temp\audit-bin tests\TestRunner.lpr
```

Test command:

```powershell
.\build-temp\audit-bin\TestRunner.exe --all --progress --format=plain
```

Summary:

```text
Number of run tests: 117
Number of errors:    0
Number of failures:  0
```

Suite summary:

| Suite | Tests | Errors | Failures |
|---|---:|---:|---:|
| `TThreadSafeListTest` | 47 | 0 | 0 |
| `TThreadSafeListStudentTest` | 2 | 0 | 0 |
| `TThreadSafeDictionaryTest` | 38 | 0 | 0 |
| `TThreadSafeHashSetTest` | 19 | 0 | 0 |
| `TThreadSafeDequeTests` | 11 | 0 | 0 |

Notes:

- `TThreadSafeListTest.Test47_DescendingSearch` covers direction-aware binary search, first-match
  duplicate behavior, and sorted-state maintenance after descending sort.
- Dictionary coverage verifies semantic key/value equality for separately allocated strings and
  self-source `AddRange` behavior.
- HashSet coverage verifies self-source `AddRange` and `RemoveRange` behavior.
- The full suite includes long-running lock and collision stress tests.
