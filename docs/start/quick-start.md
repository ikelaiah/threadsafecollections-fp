# Quick Start

You can have a thread-safe list compiled and running in about five minutes. You
need Free Pascal (`fpc`) on your `PATH`.

## Five-minute first run

The commands below compile the existing number-list example directly from source
and keep all generated files under `build-temp/`.

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

On Linux with Bash:

```bash
mkdir -p build-temp/first-run/units build-temp/first-run/bin
fpc -B -Fusrc -FUbuild-temp/first-run/units -FEbuild-temp/first-run/bin examples/SimpleNumberList/SimpleNumberList.lpr
./build-temp/first-run/bin/SimpleNumberList
```

The program prints ten random integers, sorts them in both directions, and waits
for Enter before exiting. The Windows commands and example were verified with
FPC 3.2.2 on Win64. The same Bash build path is exercised for all examples by
Linux CI. The maintained target platforms are **Windows x86-64** and **Linux**;
macOS is not currently targeted or tested.

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
[first-build commands](../project/building.md#first-build).

Expected output:

```text
10
20
30
```

## Free Pascal orientation

If Free Pascal generics are new to you, these are the conventions used by the
library and its examples:

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
  [Lock tokens (RAII)](../guides/lock-tokens.md) and
  `examples/InterfaceTest`.
- A threaded Unix program should place `cthreads` first in its program `uses`
  clause before it creates threads. The chat and benchmark examples demonstrate
  this pattern.

## Next steps

- [Learning Path](learning-path.md) — the recommended example order.
- [Installation](installation.md) — requirements and package setup.
- [List guide](../guides/list.md) — the full `TThreadSafeList<T>` story.