<p align="center">
  <img src="docs/assets/threadsafe-collections-fp-logo.svg" width="920" alt="ThreadSafeCollections-FP — concurrent collections for Free Pascal">
</p>

<h1 align="center">ThreadSafeCollections-FP</h1>

<p align="center">
  Thread-safe generic lists, deques, dictionaries, and hash sets for Free Pascal.
</p>

<p align="center">
  <a href="https://github.com/ikelaiah/threadsafecollections-fp/actions/workflows/ci.yml"><img alt="CI example builds" src="https://github.com/ikelaiah/threadsafecollections-fp/actions/workflows/ci.yml/badge.svg"></a>
  <a href="CHANGELOG.md"><img alt="Version 0.8.8" src="https://img.shields.io/badge/version-0.8.8-8B5CF6.svg"></a>
  <a href="LICENSE"><img alt="License: MIT" src="https://img.shields.io/badge/license-MIT-1E3A8A.svg"></a>
  <a href="#supported-and-verified-environments"><img alt="Free Pascal 3.2.2 verified" src="https://img.shields.io/badge/Free%20Pascal-3.2.2%20verified-3B82F6.svg"></a>
  <a href="#supported-and-verified-environments"><img alt="Example builds: Windows and Linux" src="https://img.shields.io/badge/example%20builds-Windows%20%7C%20Linux-F59E0B.svg"></a>
  <a href="docs/README.md"><img alt="Documentation" src="https://img.shields.io/badge/docs-current-10B981.svg"></a>
</p>

ThreadSafeCollections-FP provides familiar collection APIs with per-instance
synchronization, bulk operations, and documented iteration behavior. It is a
learning-focused project with a tested core, runnable examples, and no required
packages beyond the Free Pascal standard distribution.

> [!IMPORTANT]
> The library is intended for learning and experimentation. Evaluate its behavior,
> performance, and test coverage against your application's requirements before
> adopting it in production.

## Start Here

| Your goal | Best starting point |
|---|---|
| See it work in five minutes | [Five-minute first run](#five-minute-first-run) |
| Add it to a Free Pascal or Lazarus project | [Installation](#installation) |
| Learn the collection APIs | [Feature tour](#feature-tour) and [API cheat sheet](docs/CHEATSHEET.md) |
| Understand locking and iteration | [Thread-safety model](#thread-safety-model) |
| Build examples, tests, or generated docs | [Building and verification](docs/BUILDING.md) |
| Browse all current and historical docs | [Documentation home](docs/README.md) |
| See planned compatibility and API work | [Roadmap](ROADMAP.md) |

## Five-minute first run

You need Free Pascal (`fpc`) on your `PATH`. The commands below compile the
existing number-list example directly from source and keep all generated files
under `build-temp/`.

Clone the repository:

```text
git clone https://github.com/ikelaiah/threadsafecollections-fp.git
cd threadsafecollections-fp
```

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

The program prints ten random integers, sorts them in both directions, and waits
for Enter before exiting. The Windows commands and example were verified with
FPC 3.2.2 on Win64. The same Bash build path is exercised for all examples by
Linux CI; macOS is expected to use the same FPC command but is not tested in CI.

## Installation

### Requirements

- Free Pascal Compiler. FPC 3.2.2 is the version used for the maintained local
  test snapshot.
- A Free Pascal installation containing `SyncObjs` (from `fcl-base`) and
  `Generics.Collections` (from `rtl-generics`). Both are standard units included
  in the tested FPC 3.2.2 distribution; they are not third-party dependencies.
- The Lazarus package metadata separately declares the `FCL` package as a
  requirement.
- Lazarus is optional. The package build for this documentation revision was
  verified locally with Lazarus 4.8.
- Windows CI installs Lazarus 4.0.0 to obtain its bundled FPC, then compiles the
  examples directly with `fpc`; it does not invoke `lazbuild` or build the
  Lazarus package.
- Git and PowerShell are needed only for the corresponding clone, build, or
  documentation-generator workflows.

There are no required third-party Pascal packages.

### Use the source units directly

Add this repository's `src` directory to your compiler's unit search path.
From the repository root, `-Fusrc` does that for a command-line build:

```text
fpc -Fusrc path/to/YourProgram.lpr
```

In Lazarus, add `src` to **Project Options > Compiler Options > Paths > Other
unit files**, or install/open
`package/lazarus/ThreadSafeCollections.lpk` and add the package as a project
requirement.

The library units use dotted names:

```pascal
uses
  ThreadSafeCollections.List,
  ThreadSafeCollections.Deque,
  ThreadSafeCollections.Dictionary,
  ThreadSafeCollections.HashSet;
```

Import only the units your program needs.

## Your first collection

This complete program creates an integer list, adds values, sorts them, and
prints the result:

```pascal
program FirstThreadSafeList;

{$mode objfpc}{$H+}{$J-}

uses
  ThreadSafeCollections.List;

var
  Numbers: specialize TThreadSafeList<Integer>;
  Number: Integer;
begin
  Numbers := specialize TThreadSafeList<Integer>.Create(@IntegerComparer);
  try
    Numbers.AddRange([30, 10, 20]);
    Numbers.Sort;

    for Number in Numbers do
      WriteLn(Number);
  finally
    Numbers.Free;
  end;
end.
```

Save it as `build-temp/first-list/FirstThreadSafeList.lpr`, then compile it with
the same `-Fusrc`, `-FU`, and `-FE` pattern used in the
[first-build commands](docs/BUILDING.md#first-build).

### Free Pascal orientation

If Free Pascal generics are new to you, these are the conventions used above:

- `{$mode objfpc}` selects the language mode used throughout this repository.
- `uses` imports units. Unit filenames and paths must keep their exact casing on
  case-sensitive filesystems.
- `specialize` turns a generic type such as `TThreadSafeList<T>` into a concrete
  type such as `TThreadSafeList<Integer>`.
- `@IntegerComparer` passes the comparison function needed for list searching
  and sorting. Custom element types need a comparer with the same signature.
- Classes are explicitly freed. `try..finally` ensures `Free` runs if an
  operation raises an exception.
- Interface-backed collections use reference counting instead; see
  `examples/InterfaceTest`.
- A threaded Unix program should place `cthreads` first in its program `uses`
  clause before it creates threads. The chat and benchmark examples demonstrate
  this pattern.

## Feature tour

### List

`TThreadSafeList<T>` is a resizable array with indexed access, add/insert/delete
operations, ranges, sorting, searching, reversing, moving, array conversion, and
capacity management. It requires a comparer at construction.

[List guide](docs/ThreadSafeCollections.List.md) ·
[Simple example](examples/SimpleNumberList/SimpleNumberList.lpr)

### Deque

`TThreadSafeDeque<T>` is a circular-buffer double-ended queue. It supports
push, pop, peek, and `Try*` operations at both ends, plus range and array
operations.

[Deque guide](docs/ThreadSafeCollections.Deque.md) ·
[Simple example](examples/SimpleDeque/SimpleDeque.lpr)

### Dictionary

`TThreadSafeDictionary<TKey, TValue>` stores key/value pairs in chained hash
buckets. It supports add, update, lookup, removal, bulk operations, key/value
snapshots, and custom hash and equality functions.

[Dictionary guide](docs/ThreadSafeCollections.Dictionary.md) ·
[Iterator example](examples/DictionaryIterator/DictionaryIterator.lpr)

### Hash set

`TThreadSafeHashSet<T>` stores unique values and supports single-item and bulk
updates, lookup, removal, intersection, union, difference, overlap checks, and
set equality. Specialized integer, string, Boolean, and real set classes provide
built-in hash/equality choices.

[Hash-set guide](docs/ThreadSafeCollections.HashSet.md) ·
[Client example](examples/HashSetClientDemo/HashSetClientDemo.lpr)

### Interfaces and lock tokens

Each collection has an interface form, and `Lock` returns an interface token
that releases the collection lock when the token leaves scope. The implementation
uses these tokens for lock-holding enumerators and dictionary snapshot creation.
Because public methods acquire the same lock themselves, do not hold a manual
token and then call public methods on that collection; this can deadlock on
non-reentrant implementations.

[RAII-style locking guide](docs/RAII-style-locking-through-interface-counting.md) ·
[Interface example](examples/InterfaceTest/InterfaceTest.lpr)

## Thread-safety model

Each collection instance owns one `TCriticalSection`. Public collection
operations synchronize access to that instance.

That boundary matters:

- One method call is synchronized; a check followed by a separate update is not
  automatically one atomic operation. Prefer a combined operation such as
  `TryAdd`, `AddOrSetValue`, or `TryPop*` where applicable, or coordinate the
  sequence with external synchronization you control.
- List, deque, and hash-set `for..in` enumerators retain the collection lock for
  the enumerator's lifetime. Keep loop bodies short and do not hand an
  enumerator to another thread.
- Dictionary iteration snapshots the pairs first, then releases the dictionary
  lock. The snapshot does not reflect later updates.
- Synchronization protects collection structure, not mutable objects referenced
  by stored pointers, classes, or interfaces.
- Class instances stored as elements are not automatically freed by the
  collection.
- A collection must outlive every thread, enumerator, and lock token using it.

The implementation uses mutual exclusion; it is not a lock-free or
reader/writer-lock design. The iterator difference between lock-holding
(List, Deque, HashSet) and snapshot (Dictionary) enumeration is an intentional,
tested policy documented in
[Thread-safety, iteration, and lock policy](docs/Thread-Safety-and-Iteration.md).

## Supported and verified environments

| Environment | What is verified |
|---|---|
| Windows x86-64, FPC 3.2.2 | Current 146-test FPCUnit run with HeapTrc, documented examples, and both all-example scripts |
| Windows x86-64, Lazarus 4.8 | Command-line build of the Lazarus package and the package smoke consumer |
| Windows CI (`windows-latest`), FPC bundled with Lazarus 4.0.0 | All tracked examples compile and the full FPCUnit suite, including the concurrency, deadlock, and stress suites, runs through the PowerShell test script |
| Linux CI (`ubuntu-latest`) | All tracked examples compile with the distribution FPC package, the full FPCUnit suite runs, the Lazarus package builds with `lazbuild`, and the documentation checks pass |
| macOS and other FPC targets | Not currently tested by this repository |

The source is written for FPC's `objfpc` mode. A platform being supported by
FPC does not by itself mean this repository has tested that platform.

## Build, test, benchmark, and generated docs

The repository includes scripts to compile every example:

```powershell
.\build-examples.ps1 -Configuration Release
```

```bash
./build-examples.sh Release
```

Valid configurations are `Debug` and `Release`; omitting the value selects
`Release`. The scripts place programs in `example-bin/`.

The FPCUnit suite, benchmark options, Lazarus package build, and cheat-sheet
generator have separate commands and prerequisites. Follow
[Building and verification](docs/BUILDING.md) for their exact scope. The same
test, package-smoke, and documentation-check commands that CI runs are
available locally through `run-tests.*`, `smoke-package.*`, and
`tools/check-*.ps1`.

## Example learning path

1. [SimpleNumberList](examples/SimpleNumberList/SimpleNumberList.lpr) — generic
   specialization, a comparer, indexed access, and sorting.
2. [SimpleDeque](examples/SimpleDeque/SimpleDeque.lpr) — front/back queue
   operations and `TryPop`.
3. [SimpleHashSet](examples/SimpleHashSet/SimpleHashSet.lpr) — uniqueness,
   membership, removal, and specialized set types.
4. [DictionaryIterator](examples/DictionaryIterator/DictionaryIterator.lpr) —
   snapshot iteration with `Generics.Collections.TPair`.
5. [SimpleShoppingCart](examples/SimpleShoppingCart/SimpleShoppingCart.lpr) —
   records and a custom comparer.
6. [ChatMessageQueue](examples/ChatMessageQueue/ChatMessageQueue.lpr) —
   a multi-threaded queue demonstration; stop it with Ctrl+C.

All 16 tracked examples and their build status are listed in the
[building guide](docs/BUILDING.md#building-all-examples).

## Documentation

- [Documentation home](docs/README.md) — current guides, learning paths, and
  historical records
- [API cheat sheet](docs/CHEATSHEET.md) — generated public API summary
- [Building and verification](docs/BUILDING.md) — examples, tests, package,
  benchmark, and generator
- [Roadmap to 2.0](ROADMAP.md) — planned compatibility, quality, and release work
- [Changelog](CHANGELOG.md) — released and unreleased changes

## Contributing

Bug reports, focused changes, tests, examples, and documentation improvements
are welcome. Before proposing a change, build the affected examples and run the
relevant tests described in [Building and verification](docs/BUILDING.md).

## License

ThreadSafeCollections-FP is available under the [MIT License](LICENSE).
