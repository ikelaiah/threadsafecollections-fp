# Latest Test Output

## Output as of 2026-05-11

Environment:

- Free Pascal Compiler 3.2.2 for x86_64
- Target OS: Win64 for x64

Build command:

```powershell
fpc -B -Fusrc -FUbuild-temp\units -FEbuild-temp\bin tests\TestRunner.lpr
```

Test command:

```powershell
.\build-temp\bin\TestRunner.exe -a -p --format=plain
```

Summary:

```text
Number of run tests: 116
Number of errors:    0
Number of failures:  0
```

Suite summary:

| Suite | Tests | Errors | Failures |
|---|---:|---:|---:|
| `TThreadSafeListTest` | 46 | 0 | 0 |
| `TThreadSafeListStudentTest` | 2 | 0 | 0 |
| `TThreadSafeDictionaryTest` | 38 | 0 | 0 |
| `TThreadSafeHashSetTest` | 19 | 0 | 0 |
| `TThreadSafeDequeTests` | 11 | 0 | 0 |

Notes:

- `TThreadSafeListTest.Test14_DuplicateElements` passed after fixing sorted-list `IndexOf` to
  return the first matching duplicate.
- The full suite includes long-running lock and collision stress tests; this run took about
  12 minutes 37 seconds.
