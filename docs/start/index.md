# Start Here

This is the documentation home for ThreadSafeCollections-FP 0.8.8, released on
2026-08-17.

[Project README](../../README.md) · [Quick Start](quick-start.md) ·
[API cheat sheet](cheat-sheet.md) · [Roadmap](../../ROADMAP.md) ·
[Changelog](../../CHANGELOG.md)

## Choose a path

- **New to the project:** complete the
  [five-minute first run](quick-start.md#five-minute-first-run), then follow the
  [learning path](learning-path.md).
- **Adding the library to an application:** read
  [Installation](installation.md), choose a collection below, and review the
  [thread-safety model](../guides/thread-safety-and-iteration.md).
- **Building or contributing:** use the verified commands in
  [Building and verification](../project/building.md).
- **Looking up a declaration:** open the generated
  [API cheat sheet](cheat-sheet.md), then use the collection guide for behavior
  and examples.

## Collections

| Guide | Use it when you need |
|---|---|
| [List](../guides/list.md) | Indexed storage, sorting, searching, or range updates |
| [Deque](../guides/deque.md) | Queue or stack operations at both ends |
| [Dictionary](../guides/dictionary.md) | Key/value lookup, updates, or snapshots |
| [Hash set](../guides/hash-set.md) | Unique values, membership, or set operations |

## Concepts worth reading early

- [Lock tokens (RAII)](../guides/lock-tokens.md) — interface-counted lock
  lifetime, collection iteration, and manual-lock limits
- [Thread-safety, iteration, and lock policy](../guides/thread-safety-and-iteration.md)
  — the synchronization model, the decided iterator policy, and where each
  guarantee is tested
- [Hashing](../guides/hashing.md) — how the dictionary and hash set choose and
  use hash functions, including the XXHash32 string hash
- [Recipes](../guides/recipes.md) — complete, compiled programs for common tasks

## Examples

The repository contains 16 buildable example programs. A short progression is:

1. [SimpleNumberList](../../examples/SimpleNumberList/SimpleNumberList.lpr)
2. [SimpleDeque](../../examples/SimpleDeque/SimpleDeque.lpr)
3. [SimpleHashSet](../../examples/SimpleHashSet/SimpleHashSet.lpr)
4. [DictionaryIterator](../../examples/DictionaryIterator/DictionaryIterator.lpr)
5. [SimpleShoppingCart](../../examples/SimpleShoppingCart/SimpleShoppingCart.lpr)
6. [ChatMessageQueue](../../examples/ChatMessageQueue/ChatMessageQueue.lpr)

See [Building all examples](../project/building.md#building-all-examples) for
the complete inventory and exact commands, and the
[Learning Path](learning-path.md) for the recommended order.

## Historical records

The documents below are preserved as snapshots of a release, benchmark,
investigation, or design discussion. They may describe older APIs, measurements,
toolchains, or conclusions and should not be treated as current setup or API
guidance.

Release notes:

- [v0.8.0](../history/RELEASE-NOTES-v0.8.0.md)
- [v0.8.1](../history/RELEASE-NOTES-v0.8.1.md)
- [v0.8.2](../history/RELEASE-NOTES-v0.8.2.md)
- [v0.8.3](../history/RELEASE-NOTES-v0.8.3.md)
- [v0.8.4](../history/RELEASE-NOTES-v0.8.4.md)
- [v0.8.5](../history/RELEASE-NOTES-v0.8.5.md)
- [v0.8.6](../history/RELEASE-NOTES-v0.8.6.md)
- [v0.8.7](../history/RELEASE-NOTES-v0.8.7.md)
- [v0.8.8](../history/RELEASE-NOTES-v0.8.8.md)

Development and design records:

- [v0.8.1 pull-request summary](../history/PR_v0.8.1.md)
- [v0.8.5 pull-request summary](../history/PR_v0.8.5.md)
- [v0.8.6 pull-request summary](../history/PR_v0.8.6.md)
- [v0.8.7 pull-request summary](../history/PR_v0.8.7.md)
- [v0.8.8 pull-request summary](../history/PR_v0.8.8.md)
- [Maintainability improvements](../history/MAINTAINABILITY_IMPROVEMENTS.md)
- [Debugging tale](../history/Debugging-Tale.md)
- [Hash-set constructor evolution](../history/HashSet-Constructor-Evolution.md)

Test snapshots:

- [17 August 2026 test snapshot](../../tests/LatestTestOutput.md) — recorded on
  FPC 3.2.2 Win64 with `run-tests.ps1`, including the concurrency hardening
  suites
- [December 2025 raw test output](../../tests/251220-test-output.txt)

To assess the current checkout, run the tests yourself using
[the test commands](../project/building.md#running-the-tests).