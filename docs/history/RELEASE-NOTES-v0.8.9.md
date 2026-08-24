# ThreadSafeCollections-FP v0.8.9 Release Notes

[Documentation home](../index.md) ·
[Pull request summary](PR_v0.8.9.md) ·
[Changelog](../../CHANGELOG.md)

**Status:** Released

**Release date:** 2026-08-24

## Overview

Version 0.8.9 is the **final planned pre-1.0 API-hardening release**. It does
not add collection types or major features. It makes the four public APIs
(`TThreadSafeList<T>`, `TThreadSafeDeque<T>`,
`TThreadSafeDictionary<TKey, TValue>`, `TThreadSafeHashSet<T>`) coherent,
predictable, and testable enough to consider freezing at v1.0.0.

It also integrates the post-v0.8.8 documentation-system work (docs-as-code
site, GitHub Pages publishing, documentation tooling and checks) so the release
history reflects everything currently in `main`.

## Highlights

- **`Lock()` is finalized and made safe (v0.8.9 lock policy).** Each collection
  now uses a re-entrant lock (`TRecursiveCriticalSection`): the owning thread
  may call public methods — and nest `Lock()` tokens — while holding a manual
  token, with other threads still excluded for the whole sequence. The old
  POSIX deadlock trap is gone; the direct regression that was previously
  Windows-only now runs on every platform.
- **Predictable exception contracts.** Invalid indexes/ranges/capacity raise
  `EArgumentOutOfRangeException`; empty List `First`/`Last` and empty Deque
  `Pop*`/`Peek*` raise `EListError`; Dictionary missing-key raises
  `EKeyNotFoundException` and duplicate-key raises `EArgumentException`;
  reading `Current` before the first `MoveNext` raises `EInvalidOperation` on
  every enumerator (List now guards it too).
- **Construction normalized and documented.** List requires a comparer; Deque
  has default/capacity construction; Dictionary maps nil callbacks to built-in
  defaults; HashSet generic construction now rejects nil callbacks and the
  dictionary's `Count` is a read-only property (matching the other three
  collections).
- **HashSet comparer naming resolved for 1.0.** `THashSetEqualityComparer<T>`
  is the intended public name; the legacy `TEqualityComparer<T>` alias is
  retained for source compatibility. The generic collision with
  `Generics.Defaults.TEqualityComparer<T>` is documented and exercised by a
  new mixed-generics test.
- **Dictionary concrete-only API made deliberate.** `First`, `Last`,
  `BucketCount`, and `ResizeBuckets` are documented as intentional
  diagnostics/advanced members that are not part of the interface surface.
- **Edge/error contracts tested.** 29 new regression tests bring the FPCUnit
  suite to 175 tests: re-entrant and nested lock tokens, compound
  check-then-update under a token, lock-holding iteration calling public
  methods, nil callbacks, capacity rules, exception classes, empty/nil/self
  bulk inputs, callback-failure partial bulk state, snapshot-vs-mutation
  iteration, and mixed Dictionary + HashSet + RTL generics.
- **Stale claims corrected.** The Dictionary source header no longer claims a
  version number or "Delphi's TDictionary interface" compatibility and its
  iteration/locking doc comments now describe the real snapshot and
  re-entrancy behavior.
- **Documentation system integrated.** The docs-as-code site, versioned GitHub
  Pages publishing, search/themes/version selector, docs build/validation
  tooling, verified recipe examples, and the homepage-banner width fix are now
  part of the 0.8.9 record.
- **CI/release engineering.** A Windows package-smoke job builds the Lazarus
  package and runs `smoke-package.ps1` on Windows CI; the existing Linux
  package smoke and all documentation/release checks remain.

## Verification

- Windows x86-64, FPC 3.2.2 local: 175-test FPCUnit suite, 0 errors, 0
  failures, and 0 unfreed HeapTrc blocks; all 16 tracked examples compile.
- Documentation tooling tests, docs builders, and release-metadata checks all
  pass.

## API decisions

| Area | Decision |
|---|---|
| `Lock()` | Re-entrant for the owning thread; safe to combine with public calls; not cross-thread; the v1.1 scoped-atomic helpers (`GetOrAdd`, etc.) are deliberately out of scope. |
| HashSet callbacks | Both equality and hash are required; nil is rejected. |
| HashSet comparer name | `THashSetEqualityComparer<T>` is the 1.0 name; legacy alias retained. |
| Dictionary `Count` | Now a read-only property (was a function). |
| Dictionary concrete-only | `First`, `Last`, `BucketCount`, `ResizeBuckets` stay concrete-only and documented. |
| Exceptions | Standard FPC classes (`EArgumentOutOfRangeException`, `EListError`, `EKeyNotFoundException`, `EArgumentException`, `EInvalidOperation`). |
| Iterator policy | Unchanged and intentional (lock-holding for List/Deque/HashSet, snapshot for Dictionary). |

See the [Contracts & Limitations](../reference/contracts-and-limitations.md),
[Configuration](../reference/config.md), [Lock Tokens](../guides/lock-tokens.md),
and [Thread-safety & Iteration](../guides/thread-safety-and-iteration.md)
references for the full contracts.