# Installation

## Requirements

- **Free Pascal Compiler.** FPC 3.2.2 is the version used for the maintained
  local test snapshot.
- A Free Pascal installation containing `SyncObjs` (from `fcl-base`) and
  `Generics.Collections` (from `rtl-generics`). Both are standard units included
  in the tested FPC 3.2.2 distribution; they are not third-party dependencies.
- **Lazarus is optional.** The Lazarus package metadata separately declares the
  `FCL` package as a requirement. The package build for this documentation
  revision was verified locally with Lazarus 4.8.
- Windows CI installs Lazarus 4.0.0 to obtain its bundled FPC, then compiles the
  examples directly with `fpc`; it does not invoke `lazbuild` or build the
  Lazarus package.
- Git and PowerShell are needed only for the corresponding clone, build, or
  documentation-generator workflows.

There are no required third-party Pascal packages.

## Use the source units directly

Add this repository's `src` directory to your compiler's unit search path.
From the repository root, `-Fusrc` does that for a command-line build:

```text
fpc -Fusrc path/to/YourProgram.lpr
```

In Lazarus, add `src` to **Project Options > Compiler Options > Paths > Other
unit files**, or install/open
`package/lazarus/ThreadSafeCollections.lpk` and add the package as a project
requirement.

## Units

The library units use dotted names:

```pascal
uses
  ThreadSafeCollections.List,
  ThreadSafeCollections.Deque,
  ThreadSafeCollections.Dictionary,
  ThreadSafeCollections.HashSet;
```

Import only the units your program needs. `ThreadSafeCollections.Interfaces`
declares the shared interface forms and `ILockToken`; the concrete units import
it themselves, but import it explicitly if your own code names `ILockToken`
directly.

## Next steps

- [Quick Start](quick-start.md) — compile your first collection in five minutes.
- [API Overview](../reference/api-overview.md) — the public types and interfaces.
- [Building & Verification](../project/building.md) — examples, tests, package,
  and benchmark commands.