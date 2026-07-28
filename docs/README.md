# ThreadSafeCollections-FP documentation

[Project README](../README.md) · [Build and verify](BUILDING.md) ·
[API cheat sheet](CHEATSHEET.md) · [Roadmap](../ROADMAP.md)

This is the documentation home for ThreadSafeCollections-FP 0.8.6, released on
2027-06-28.

## Choose a path

- **New to the project:** complete the
  [five-minute first run](../README.md#five-minute-first-run), then follow the
  [example learning path](../README.md#example-learning-path).
- **Adding the library to an application:** read
  [installation](../README.md#installation), choose a collection below, and
  review the [thread-safety model](../README.md#thread-safety-model).
- **Building or contributing:** use the verified commands in
  [Building and verification](BUILDING.md).
- **Looking up a declaration:** open the generated
  [API cheat sheet](CHEATSHEET.md), then use the collection guide for behavior
  and examples.

## Current guides

### Collections

| Guide | Use it when you need |
|---|---|
| [List](ThreadSafeCollections.List.md) | Indexed storage, sorting, searching, or range updates |
| [Deque](ThreadSafeCollections.Deque.md) | Queue or stack operations at both ends |
| [Dictionary](ThreadSafeCollections.Dictionary.md) | Key/value lookup, updates, or snapshots |
| [Hash set](ThreadSafeCollections.HashSet.md) | Unique values, membership, or set operations |

### Project and advanced topics

- [Building and verification](BUILDING.md) — prerequisites, first build, all
  examples, tests, Lazarus package, benchmark, and documentation generator
- [API cheat sheet](CHEATSHEET.md) — generated declarations and complexity
  annotations from the source
- [RAII-style locking through interface counting](RAII-style-locking-through-interface-counting.md)
  — lock-token lifetimes, iteration, and manual-lock limitations
- [XXHash32 explained](XXHash32-Explained.md) — an implementation-oriented
  explanation of the non-cryptographic string hash
- [Roadmap to 2.0](../ROADMAP.md) — planned compatibility, performance, quality,
  and documentation work
- [Changelog](../CHANGELOG.md) — unreleased and released changes

## Examples

The repository contains 16 buildable example programs. A short progression is:

1. [SimpleNumberList](../examples/SimpleNumberList/SimpleNumberList.lpr)
2. [SimpleDeque](../examples/SimpleDeque/SimpleDeque.lpr)
3. [SimpleHashSet](../examples/SimpleHashSet/SimpleHashSet.lpr)
4. [DictionaryIterator](../examples/DictionaryIterator/DictionaryIterator.lpr)
5. [SimpleShoppingCart](../examples/SimpleShoppingCart/SimpleShoppingCart.lpr)
6. [ChatMessageQueue](../examples/ChatMessageQueue/ChatMessageQueue.lpr)

See [Building all examples](BUILDING.md#building-all-examples) for the complete
inventory and exact commands.

## Historical records

The documents below are preserved as snapshots of a release, benchmark,
investigation, or design discussion. They may describe older APIs, measurements,
toolchains, or conclusions and should not be treated as current setup or API
guidance.

### Release snapshots

- [v0.8.0](RELEASE-NOTES-v0.8.0.md)
- [v0.8.1](RELEASE-NOTES-v0.8.1.md)
- [v0.8.2](RELEASE-NOTES-v0.8.2.md)
- [v0.8.3](RELEASE-NOTES-v0.8.3.md)
- [v0.8.4](RELEASE-NOTES-v0.8.4.md)
- [v0.8.5](RELEASE-NOTES-v0.8.5.md)
- [v0.8.6](RELEASE-NOTES-v0.8.6.md)

### Development and design records

- [v0.8.1 pull-request summary](PR_v0.8.1.md)
- [v0.8.5 pull-request summary](PR_v0.8.5.md)
- [v0.8.6 pull-request summary](PR_v0.8.6.md)
- [Maintainability improvements](MAINTAINABILITY_IMPROVEMENTS.md)
- [Debugging tale](Debugging-Tale.md)
- [Hash-set constructor evolution](HashSet-Constructor-Evolution.md)

### Test snapshots

- [17 July 2026 test snapshot](../tests/LatestTestOutput.md) — recorded on
  FPC 3.2.2 Win64
- [December 2025 raw test output](../tests/251220-test-output.txt)

To assess the current checkout, run the tests yourself using
[the test commands](BUILDING.md#running-the-tests).
