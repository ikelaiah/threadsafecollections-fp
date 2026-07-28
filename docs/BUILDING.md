# Building and verification

[Documentation home](README.md) · [Project README](../README.md) ·
[API cheat sheet](CHEATSHEET.md)

**Audience:** developers building the project for the first time, contributors,
and maintainers reproducing example, test, benchmark, package, or generated-doc
workflows.

Run every command from the repository root unless a section says otherwise.
Build outputs below stay in `example-bin/`, `build-temp/`, or the Lazarus
package's configured `lib/` directory.

## Prerequisites and verified environments

Required for direct compiler builds:

- Free Pascal Compiler (`fpc`) on `PATH`
- The standard FPC units used by the source, including `SyncObjs` from
  `fcl-base` and `Generics.Collections` from `rtl-generics`

Optional tools:

- Lazarus and `lazbuild` for the package build
- Windows PowerShell 5.1 or PowerShell 7 (`pwsh`) for the PowerShell build and
  cheat-sheet generator
- Bash for `build-examples.sh`

The current documentation revision was verified with FPC 3.2.2 and Lazarus 4.8
on Win64, including the full 118-test suite and the Lazarus package build.
Current CI compiles all examples on `ubuntu-latest` with the distribution `fpc`
package. On `windows-latest`, CI installs Lazarus 4.0.0 to obtain its bundled
FPC, then invokes `build-examples.ps1`; it does not invoke `lazbuild`. Neither CI
job currently builds the Lazarus package or runs the FPCUnit suite. macOS and
other targets are not tested by the repository.

`Generics.Collections` is not a newly introduced external package requirement.
The current implementation imports it in
`ThreadSafeCollections.Interfaces.pas` and
`ThreadSafeCollections.Dictionary.pas` for `TPair`; it ships with the tested
FPC distribution. Separately, `package/lazarus/ThreadSafeCollections.lpk`
declares Lazarus's `FCL` package as a requirement.

Check the tools available in your shell:

```powershell
fpc -iV
Get-Command fpc
```

```bash
fpc -iV
command -v fpc
```

## First build

The shortest verified path compiles one existing example without modifying
project files.

On Windows PowerShell:

```powershell
New-Item -ItemType Directory -Force build-temp\first-run\units, build-temp\first-run\bin | Out-Null
fpc -B -Fusrc -FUbuild-temp\first-run\units -FEbuild-temp\first-run\bin examples\SimpleNumberList\SimpleNumberList.lpr
.\build-temp\first-run\bin\SimpleNumberList.exe
```

On Linux or macOS with Bash:

```bash
mkdir -p build-temp/first-run/units build-temp/first-run/bin
fpc -B -Fusrc -FUbuild-temp/first-run/units -FEbuild-temp/first-run/bin examples/SimpleNumberList/SimpleNumberList.lpr
./build-temp/first-run/bin/SimpleNumberList
```

The example prints unsorted and sorted integer lists, then waits for Enter.

## Building all examples

On Windows PowerShell:

```powershell
.\build-examples.ps1 -Configuration Release
```

On Linux, macOS, or Git Bash:

```bash
./build-examples.sh Release
```

The scripts accept exactly two configurations:

- `Release` (the default): `-O3 -XX`
- `Debug`: `-O1 -gl -gh -Cr -Co`

Both use `-B -MObjFPC -Sh`. Programs are written to `example-bin/`; compiled
units are separated under `example-bin/units/<program>/`.

The scripts discover and compile these 16 tracked programs:

| Program | Focus |
|---|---|
| `Benchmark` | Single-thread and multi-thread collection measurements |
| `ChatMessageQueue` | Multi-threaded message queue; runs until Ctrl+C |
| `DequeWithCustomType` | Record values in a deque |
| `DictionaryIterator` | Snapshot iteration over basic key/value pairs |
| `DictionaryIteratorRecord` | Snapshot iteration with record values |
| `DictionaryWithCustomType` | Custom record-key hash and equality functions |
| `HashSetClientDemo` | Custom values, bulk operations, sets, and iteration |
| `HashSetIterate` | Locked iteration over integers |
| `HashSetIterateRecord` | Locked iteration over records |
| `InterfaceTest` | Interface-backed list and deque |
| `IterateList` | Locked list iteration |
| `SimpleDeque` | Front and back queue operations |
| `SimpleHashSet` | Specialized integer and string sets |
| `SimpleNumberList` | Comparer, indexing, and sorting |
| `SimpleShoppingCart` | Records and a custom comparer |
| `SimpleToDoList` | Interactive add, list, and sort commands |

Some interactive examples wait for input. `SimpleToDoList` currently implements
`add`, `list`, `sort`, and `quit`; its prompt also mentions `remove`, but no
remove command is implemented.

## Building the Lazarus package

Open `package/lazarus/ThreadSafeCollections.lpk` in Lazarus and choose
**Compile**. For a command-line package build:

```text
lazbuild --build-all package/lazarus/ThreadSafeCollections.lpk
```

The package points Lazarus at `src`, writes compiled units below
`package/lazarus/lib/<target-cpu>-<target-os>/`, and declares the standard FCL
package as a requirement. The package version in the current checkout is 0.8.5.

## Running the tests

The test runner is an FPCUnit console application. These Windows PowerShell
commands keep the executable and newly compiled units in `build-temp/tests/`:

```powershell
New-Item -ItemType Directory -Force build-temp\tests\units, build-temp\tests\bin | Out-Null
fpc -B -MObjFPC -Sh -gl -gh -Cr -Co -Fusrc -Futests -FUbuild-temp\tests\units -FEbuild-temp\tests\bin tests\TestRunner.lpr
.\build-temp\tests\bin\TestRunner.exe --all --format=plain
```

The equivalent compiler and runner syntax for Bash is:

```bash
mkdir -p build-temp/tests/units build-temp/tests/bin
fpc -B -MObjFPC -Sh -gl -gh -Cr -Co -Fusrc -Futests -FUbuild-temp/tests/units -FEbuild-temp/tests/bin tests/TestRunner.lpr
./build-temp/tests/bin/TestRunner --all --format=plain
```

The Bash form is provided for portability but is not exercised by current CI.
Threaded test execution on Unix has not been claimed as verified.

The FPCUnit console runner accepts:

- `-l` or `--list` — list registered tests
- `-a` or `--all` — run all tests
- `-p` or `--progress` — show progress while tests run
- `--suite=SuiteName` — run one registered suite
- `--format=plain`, `plainnotiming`, `xml`, or `latex` — select output format

The program's custom help footer also advertises `-t TestName`, but FPCUnit
3.2.2 rejects that option; use `--suite=SuiteName`. The suite is not quick:
collision and high-volume concurrency cases can take several minutes. The
latest preserved result is a
[historical 118-test Win64 snapshot](../tests/LatestTestOutput.md).

## Running the benchmark

Build all examples in `Release` mode first, then run a small benchmark:

```powershell
.\example-bin\Benchmark.exe --size=1000
```

```bash
./example-bin/Benchmark --size=1000
```

The benchmark recognizes:

- `--size=N` — run one size from 1 through 1,000,000 instead of the default
  1,000, 10,000, 100,000, and 1,000,000 sizes
- `--affinity` — pin the timing thread to CPU core 0 on Windows; other platforms
  print a warning

There is no benchmark help flag. Unrecognized arguments are currently ignored.
Each run performs five timing samples per scenario, omits the fastest and
slowest, and writes a timestamped `benchmark_*.csv` file in the current working
directory. Treat benchmark results as measurements of that machine and build,
not universal performance guarantees.

## Regenerating the API cheat sheet

The generator reads public declarations and complexity annotations from the
source units plus the package version from the Lazarus package.

With Windows PowerShell:

```powershell
powershell -ExecutionPolicy Bypass -File .\tools\generate-cheatsheet.ps1
```

With PowerShell 7 on any supported PowerShell host:

```powershell
pwsh -File ./tools/generate-cheatsheet.ps1
```

The default output is `docs/CHEATSHEET.md`. To check generation without
overwriting it:

```powershell
pwsh -File ./tools/generate-cheatsheet.ps1 -OutputPath build-temp/cheatsheet-check.md
```

A relative `-OutputPath` is resolved from the repository root, not from the
caller's current directory. The script creates a missing output directory.

## Before submitting a documentation change

At minimum:

1. Compile every command or complete example added to current documentation.
2. Run the relevant FPCUnit suites.
3. Regenerate `docs/CHEATSHEET.md` if source declarations or its generator
   changed.
4. Check local Markdown links, heading fragments, and balanced fences.
5. Run `git diff --check`.

Historical results are evidence for that recorded checkout only; they do not
replace verification of the current tree.
